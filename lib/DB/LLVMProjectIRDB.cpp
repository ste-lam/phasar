/******************************************************************************
 * Copyright (c) 2017 Philipp Schubert.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Philipp Schubert and others
 *****************************************************************************/

#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>

#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Transforms/Utils.h"

#include "boost/filesystem.hpp"

#include "phasar/Config/Configuration.h"
#include "phasar/DB/LLVMProjectIRDB.h"
#include "phasar/PhasarLLVM/DataFlowSolver/IfdsIde/LLVMZeroValue.h"
#include "phasar/PhasarLLVM/Passes/GeneralStatisticsAnalysis.h"
#include "phasar/PhasarLLVM/Passes/ValueAnnotationPass.h"
#include "phasar/Utils/EnumFlags.h"
#include "phasar/Utils/IO.h"
#include "phasar/Utils/LLVMShorthands.h"
#include "phasar/Utils/Logger.h"
#include "phasar/Utils/PAMMMacros.h"
#include "phasar/Utils/Utilities.h"

using namespace psr;
using namespace std;

namespace psr {

LLVMProjectIRDB::LLVMProjectIRDB(IRDBOptions Options) : Options(Options) {
  // register the GeneralStaticsPass analysis pass to the ModuleAnalysisManager
  // such that we can query its results later on
  GeneralStatisticsAnalysis GSP;
  MAM.registerPass([&]() { return std::move(GSP); });
  PB.registerModuleAnalyses(MAM);
  // add the transformation pass ValueAnnotationPass
  MPM.addPass(ValueAnnotationPass());
  // just to be sure that none of the passes messed up the module!
  MPM.addPass(llvm::VerifierPass());
}

LLVMProjectIRDB::LLVMProjectIRDB(const std::vector<std::string> &IRFiles,
                                 IRDBOptions Options)
    : LLVMProjectIRDB(Options | IRDBOptions::OWNS) {
  for (const auto &File : IRFiles) {
    // if we have a file that is already compiled to llvm ir
    if ((File.find(".ll") != std::basic_string<char, std::char_traits<char>,
                                               std::allocator<char>>::npos ||
         File.find(".bc") != std::basic_string<char, std::char_traits<char>,
                                               std::allocator<char>>::npos) &&
        boost::filesystem::exists(File)) {
      llvm::SMDiagnostic Diag;
      std::unique_ptr<llvm::LLVMContext> C(new llvm::LLVMContext);
      std::unique_ptr<llvm::Module> M = llvm::parseIRFile(File, Diag, *C);
      bool BrokenDebugInfo = false;
      if (M == nullptr) {
        Diag.print(File.c_str(), llvm::errs());
      }
      /* Crash in presence of llvm-3.9.1 module (segfault) */
      if (M == nullptr ||
          llvm::verifyModule(*M, &llvm::errs(), &BrokenDebugInfo)) {
        throw std::runtime_error(File + " could not be parsed correctly");
      }
      if (BrokenDebugInfo) {
        std::cout << "caution: debug info is broken\n";
      }
      Modules.insert(std::make_pair(File, std::move(M)));
      Contexts.push_back(std::move(C));
    } else {
      throw std::invalid_argument(File + " is not a valid llvm module");
    }
  }
  if (Options & IRDBOptions::WPA) {
    linkForWPA();
  }
  preprocessAllModules();
}

LLVMProjectIRDB::LLVMProjectIRDB(const std::vector<llvm::Module *> &Modules,
                                 IRDBOptions Options)
    : LLVMProjectIRDB(Options) {
  for (auto *M : Modules) {
    insertModule(M);
  }
  if (Options & IRDBOptions::WPA) {
    linkForWPA();
  }
}

LLVMProjectIRDB::~LLVMProjectIRDB() {
  // release resources if IRDB does not own
  if (!(Options & IRDBOptions::OWNS)) {
    for (auto &Context : Contexts) {
      Context.release();
    }
    for (auto &[File, Module] : Modules) {
      Module.release();
    }
  }
  MAM.clear();
}

void LLVMProjectIRDB::preprocessModule(llvm::Module *M) {
  PAMM_GET_INSTANCE;
  // add moduleID to timer name if performing MWA!
  START_TIMER("LLVM Passes", PAMM_SEVERITY_LEVEL::Full);
  LOG_IF_ENABLE(BOOST_LOG_SEV(lg::get(), INFO)
                << "Preprocess module: " << M->getModuleIdentifier());
  MPM.run(*M, MAM);
  // retrieve data from the GeneralStatisticsAnalysis registered earlier
  auto GSPResult = MAM.getResult<GeneralStatisticsAnalysis>(*M);
  auto Allocas = GSPResult.getAllocaInstructions();
  AllocaInstructions.insert(Allocas.begin(), Allocas.end());

  // Problem:
  // Allocas contains const llvm::Instruction * elements
  // and AllocaInstructions is of type std::set<llvm::Instruction *>

  // possible fix
  // for (auto Alloca : Allocas) {
  //   AllocaInstructions.insert(const_cast<llvm::Instruction *>(Alloca));
  // }

  auto ATypes = GSPResult.getAllocatedTypes();
  AllocatedTypes.insert(ATypes.begin(), ATypes.end());
  auto RRInsts = GSPResult.getRetResInstructions();
  RetOrResInstructions.insert(RRInsts.begin(), RRInsts.end());
  STOP_TIMER("LLVM Passes", PAMM_SEVERITY_LEVEL::Full);
  buildIDModuleMapping(M);
}

void LLVMProjectIRDB::linkForWPA() {
  // Linking llvm modules:
  // Unfortunately linking between different contexts is currently not possible.
  // Therefore we must load all modules into one single context and then perform
  // the linkage. This is still very fast compared to compiling and
  // pre-processing
  // all modules.
  if (Modules.size() > 1) {
    llvm::Module *MainMod = getModuleDefiningFunction("main");
    assert(MainMod && "could not find main function");
    for (auto &[File, Module] : Modules) {
      // we do not want to link a module with itself!
      if (MainMod != Module.get()) {
        // reload the modules into the module containing the main function
        std::string IRBuffer;
        llvm::raw_string_ostream RSO(IRBuffer);
        llvm::WriteBitcodeToFile(*Module.get(), RSO);
        RSO.flush();
        llvm::SMDiagnostic ErrorDiagnostics;
        std::unique_ptr<llvm::MemoryBuffer> MemBuffer =
            llvm::MemoryBuffer::getMemBuffer(IRBuffer);
        std::unique_ptr<llvm::Module> TmpMod =
            llvm::parseIR(*MemBuffer, ErrorDiagnostics, MainMod->getContext());
        bool BrokenDebugInfo = false;
        if (TmpMod == nullptr ||
            llvm::verifyModule(*TmpMod, &llvm::errs(), &BrokenDebugInfo)) {
          llvm::report_fatal_error("Error: module is broken!");
        }
        if (BrokenDebugInfo) {
          // FIXME at least log this incident
        }
        // now we can safely perform the linking
        if (llvm::Linker::linkModules(*MainMod, std::move(TmpMod),
                                      llvm::Linker::LinkOnlyNeeded)) {
          llvm::report_fatal_error(
              "Error: trying to link modules into single WPA module failed!");
        }
      }
    }
    // Update the IRDB reflecting that we now only need 'MainMod' and its
    // corresponding context!
    // delete every other module
    for (auto It = Modules.begin(); It != Modules.end();) {
      if (It->second.get() != MainMod) {
        It = Modules.erase(It);
      } else {
        ++It;
      }
    }
    // delete every other context
    for (auto It = Contexts.begin(); It != Contexts.end();) {
      if (It->get() != &MainMod->getContext()) {
        It = Contexts.erase(It);
      } else {
        ++It;
      }
    }
    WPAModule = MainMod;
  } else if (Modules.size() == 1) {
    // In this case we only have one module anyway, so we do not have
    // to link at all. But we have to update the WPAMOD pointer!
    WPAModule = Modules.begin()->second.get();
  }
}

void LLVMProjectIRDB::preprocessAllModules() {
  for (auto &[File, Module] : Modules) {
    preprocessModule(Module.get());
  }
}

llvm::Module *LLVMProjectIRDB::getWPAModule() {
  if (!WPAModule) {
    linkForWPA();
  }
  return WPAModule;
}

void LLVMProjectIRDB::buildIDModuleMapping(llvm::Module *M) {
  for (auto &F : *M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        IDInstructionMapping[stol(getMetaDataID(&I))] = &I;
      }
    }
  }
}

llvm::Module *LLVMProjectIRDB::getModule(const std::string &ModuleName) {
  if (Modules.count(ModuleName)) {
    return Modules[ModuleName].get();
  }
  return nullptr;
}

llvm::Instruction *LLVMProjectIRDB::getInstruction(std::size_t Id) {
  if (IDInstructionMapping.count(Id)) {
    return IDInstructionMapping[Id];
  }
  return nullptr;
}

std::size_t LLVMProjectIRDB::getInstructionID(llvm::Instruction *I) const {
  std::size_t Id = 0;
  if (auto *MD = llvm::cast<llvm::MDString>(
          I->getMetadata(PhasarConfig::MetaDataKind())->getOperand(0))) {
    Id = stol(MD->getString().str());
  }
  return Id;
}

void LLVMProjectIRDB::print(std::ostream &OS) const {
  for (const auto &[File, Module] : Modules) {
    std::cout << "Module: " << File << std::endl;
    llvm::outs() << *Module;
  }
}

void LLVMProjectIRDB::emitPreprocessedIR(std::ostream &OS,
                                         bool ShortenIR) const {
  for (const auto &[File, Module] : Modules) {
    OS << "IR module: " << File << '\n';
    // print globals
    for (auto &Glob : Module->globals()) {
      if (ShortenIR) {
        OS << llvmIRToShortString(&Glob);
      } else {
        OS << llvmIRToString(&Glob);
      }
      OS << '\n';
    }
    OS << '\n';
    for (const auto *F : getAllFunctions()) {
      if (!F->isDeclaration() && Module->getFunction(F->getName())) {
        OS << F->getName().str() << " {\n";
        for (const auto &BB : *F) {
          // do not print the label of the first BB
          if (BB.getPrevNode()) {
            std::string BBLabel;
            llvm::raw_string_ostream RSO(BBLabel);
            BB.printAsOperand(RSO, false);
            RSO.flush();
            OS << "\n<label " << BBLabel << ">\n";
          }
          // print all instructions
          for (const auto &I : BB) {
            OS << "  ";
            if (ShortenIR) {
              OS << llvmIRToShortString(&I);
            } else {
              OS << llvmIRToString(&I);
            }
            OS << '\n';
          }
        }
        OS << "}\n\n";
      }
    }
    OS << '\n';
  }
}

llvm::Function *
LLVMProjectIRDB::getFunctionDefinition(const string &FunctionName) const {
  for (const auto &[File, Module] : Modules) {
    auto *F = Module->getFunction(FunctionName);
    if (F && !F->isDeclaration()) {
      return F;
    }
  }
  return nullptr;
}

llvm::Function *
LLVMProjectIRDB::getFunction(const std::string &FunctionName) const {
  for (const auto &[File, Module] : Modules) {
    auto *F = Module->getFunction(FunctionName);
    if (F) {
      return F;
    }
  }
  return nullptr;
}

llvm::GlobalVariable *LLVMProjectIRDB::getGlobalVariable(
    const std::string &GlobalVariableName) const {
  for (const auto &[File, Module] : Modules) {
    auto *G = Module->getGlobalVariable(GlobalVariableName);
    if (G) {
      return G;
    }
  }
  return nullptr;
}

llvm::GlobalVariable *LLVMProjectIRDB::getGlobalVariableDefinition(
    const std::string &GlobalVariableName) const {
  for (const auto &[File, Module] : Modules) {
    auto *G = Module->getGlobalVariable(GlobalVariableName);
    if (G && !G->isDeclaration()) {
      return G;
    }
  }
  return nullptr;
}

llvm::Module *LLVMProjectIRDB::getModuleDefiningFunction(
    const std::string &FunctionName) const {
  for (auto &[File, Module] : Modules) {
    auto *F = Module->getFunction(FunctionName);
    if (F && !F->isDeclaration()) {
      return Module.get();
    }
  }
  return nullptr;
}

std::string LLVMProjectIRDB::valueToPersistedString(llvm::Value *V) const {
  if (LLVMZeroValue::getInstance()->isLLVMZeroValue(V)) {
    return LLVMZeroValue::getInstance()->getName();
  } else if (const auto *I = llvm::dyn_cast<llvm::Instruction>(V)) {
    return I->getFunction()->getName().str() + "." + getMetaDataID(I);
  } else if (const auto *A = llvm::dyn_cast<llvm::Argument>(V)) {
    return A->getParent()->getName().str() + ".f" +
           std::to_string(A->getArgNo());
  } else if (const auto *G = llvm::dyn_cast<llvm::GlobalValue>(V)) {
    std::cout << "special case: WE ARE AN GLOBAL VARIABLE\n";
    std::cout << "all user:\n";
    for (const auto *User : V->users()) {
      if (const auto *I = llvm::dyn_cast<llvm::Instruction>(User)) {
        std::cout << I->getFunction()->getName().str() << "\n";
      }
    }
    return G->getName().str();
  } else if (llvm::isa<llvm::Value>(V)) {
    // In this case we should have an operand of an instruction which can be
    // identified by the instruction id and the operand index.
    std::cout << "special case: WE ARE AN OPERAND\n";
    // We should only have one user in this special case
    for (const auto *User : V->users()) {
      if (const auto *I = llvm::dyn_cast<llvm::Instruction>(User)) {
        for (unsigned Idx = 0; Idx < I->getNumOperands(); ++Idx) {
          if (I->getOperand(Idx) == V) {
            return I->getFunction()->getName().str() + "." + getMetaDataID(I) +
                   ".o." + std::to_string(Idx);
          }
        }
      }
    }
    llvm::report_fatal_error("Error: llvm::Value is of unexpected type.");
    return "";
  } else {
    llvm::report_fatal_error("Error: llvm::Value is of unexpected type.");
    return "";
  }
}

llvm::Value *
LLVMProjectIRDB::persistedStringToValue(const std::string &S) const {
  if (S.find(LLVMZeroValue::getInstance()->getName()) != std::string::npos) {
    return LLVMZeroValue::getInstance();
  } else if (S.find('.') == std::string::npos) {
    return getGlobalVariableDefinition(S);
  } else if (S.find(".f") != std::string::npos) {
    unsigned Argno = stoi(S.substr(S.find(".f") + 2, S.size()));
    return const_cast<llvm::Argument *>(getNthFunctionArgument(
        getFunctionDefinition(S.substr(0, S.find(".f"))), Argno));
  } else if (S.find(".o.") != std::string::npos) {
    unsigned I = S.find('.');
    unsigned J = S.find(".o.");
    unsigned InstID = stoi(S.substr(I + 1, J));
    // std::cout << "FOUND instID: " << instID << "\n";
    unsigned OpIdx = stoi(S.substr(J + 3, S.size()));
    // std::cout << "FOUND opIdx: " << to_string(opIdx) << "\n";
    llvm::Function *F = getFunctionDefinition(S.substr(0, S.find('.')));
    for (auto &BB : *F) {
      for (auto &Inst : BB) {
        if (getMetaDataID(&Inst) == std::to_string(InstID)) {
          return Inst.getOperand(OpIdx);
        }
      }
    }
    llvm::report_fatal_error("Error: operand not found.");
  } else if (S.find('.') != std::string::npos) {
    llvm::Function *F = getFunctionDefinition(S.substr(0, S.find('.')));
    for (auto &BB : *F) {
      for (auto &Inst : BB) {
        if (getMetaDataID(&Inst) == S.substr(S.find('.') + 1, S.size())) {
          return &Inst;
        }
      }
    }
    llvm::report_fatal_error("Error: llvm::Instruction not found.");
  } else {
    llvm::report_fatal_error(
        "Error: string cannot be translated into llvm::Value.");
  }
  return nullptr;
}

std::set<llvm::Function *> LLVMProjectIRDB::getAllFunctions() const {
  std::set<llvm::Function *> Functions;
  for (auto &[File, Module] : Modules) {
    for (auto &F : *Module) {
      Functions.insert(&F);
    }
  }
  return Functions;
}

void LLVMProjectIRDB::insertModule(llvm::Module *M) {
  Contexts.push_back(std::unique_ptr<llvm::LLVMContext>(&M->getContext()));
  Modules.insert(std::make_pair(M->getModuleIdentifier(), M));
  preprocessModule(M);
}

llvm::StructType *
LLVMProjectIRDB::getStructType(const std::string &TypeName) const {
  /// Returns the struct type's definition if available, its declaration
  /// otherwise.
  // TODO: implement
  return nullptr;
}

llvm::StructType *
LLVMProjectIRDB::getStructTypeDefinition(const std::string &TypeName) const {
  /// Returns the struct type's definition if available, null otherwise.
  // TODO: implement
  return nullptr;
}

std::set<llvm::StructType *> LLVMProjectIRDB::getAllocatedStructTypes() const {
  std::set<llvm::StructType *> StructTypes;
  for (auto *Ty : AllocatedTypes) {
    if (auto *StructTy = llvm::dyn_cast<llvm::StructType>(Ty)) {
      StructTypes.insert(StructTy);
    }
  }
  return StructTypes;
}

set<llvm::Value *> LLVMProjectIRDB::getAllMemoryLocations() const {
  // get all stack and heap alloca instructions
  auto AllocaInsts = getAllocaInstructions();
  set<llvm::Value *> AllMemoryLoc;
  for (auto *AllocaInst : AllocaInsts) {
    AllMemoryLoc.insert(AllocaInst);
  }
  set<string> IgnoredGlobalNames = {"llvm.used",
                                    "llvm.compiler.used",
                                    "llvm.global_ctors",
                                    "llvm.global_dtors",
                                    "vtable",
                                    "typeinfo"};
  // add global varibales to the memory location set, except the llvm
  // intrinsic global variables
  for (auto &[File, Module] : Modules) {
    for (auto &GV : Module->globals()) {
      if (GV.hasName()) {
        string GVName = cxxDemangle(GV.getName().str());
        if (!IgnoredGlobalNames.count(GVName.substr(0, GVName.find(' ')))) {
          AllMemoryLoc.insert(&GV);
        }
      }
    }
  }
  return AllMemoryLoc;
}

std::set<std::string> LLVMProjectIRDB::getAllSourceFiles() const {
  std::set<std::string> AllSourceFiles;
  for (auto &Element : Modules) {
    AllSourceFiles.insert(Element.first);
  }
  return AllSourceFiles;
}

bool LLVMProjectIRDB::hasDebugInfo() const {
  if (WPAModule) {
    return wasCompiledWithDebugInfo(WPAModule);
  }
  // During unittests WPAMOD might not be set
  else if (!Modules.empty()) {
    for (const auto &[File, Module] : Modules) {
      if (!wasCompiledWithDebugInfo(Module.get())) {
        return false;
      }
    }
    return true;
  }
  return false;
}

} // namespace psr
