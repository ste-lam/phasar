/******************************************************************************
 * Copyright (c) 2017 Philipp Schubert.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Philipp Schubert and others
 *****************************************************************************/

/*
 * MyHelloPass.cpp
 *
 *  Created on: 05.07.2016
 *      Author: pdschbrt
 */

#include <string>

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Demangle/Demangle.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/raw_os_ostream.h"

#include "phasar/PhasarLLVM/Passes/GeneralStatisticsAnalysis.h"
#include "phasar/Utils/LLVMShorthands.h"
#include "phasar/Utils/Logger.h"
#include "phasar/Utils/PAMMMacros.h"

using namespace std;
using namespace psr;

namespace psr {

llvm::AnalysisKey GeneralStatisticsAnalysis::Key;

GeneralStatisticsAnalysis::GeneralStatisticsAnalysis() = default;

GeneralStatistics
GeneralStatisticsAnalysis::run(llvm::Module &M,
                               llvm::ModuleAnalysisManager &AM) {
  LOG_IF_ENABLE(BOOST_LOG_SEV(lg::get(), INFO)
                << "Running GeneralStatisticsAnalysis");
  static const std::set<std::string> MemAllocatingFunctions = {
      "operator new(unsigned long)", "operator new[](unsigned long)", "malloc",
      "calloc", "realloc"};
  for (auto &F : M) {
    ++Stats.NumFunctions;
    for (auto &BB : F) {
      ++Stats.NumBasicBlocks;
      for (auto &I : BB) {
        // found one more instruction
        ++Stats.NumInstructions;
        // check for alloca instruction for possible types
        if (const llvm::AllocaInst *Alloc =
                llvm::dyn_cast<llvm::AllocaInst>(&I)) {
          Stats.AllocatedTypes.insert(Alloc->getAllocatedType());
          // do not add allocas from llvm internal NumFunctions
          Stats.AllocaInstructions.insert(&I);
          ++Stats.NumAllocationSites;
        } // check bitcast instructions for possible types
        else {
          for (auto *User : I.users()) {
            if (const llvm::BitCastInst *Cast =
                    llvm::dyn_cast<llvm::BitCastInst>(User)) {
              // types.insert(cast->getDestTy());
            }
          }
        }
        // check for return or resume instructions
        if (llvm::isa<llvm::ReturnInst>(I) || llvm::isa<llvm::ResumeInst>(I)) {
          Stats.RetResInstructions.insert(&I);
        }
        // check for store instructions
        if (llvm::isa<llvm::StoreInst>(I)) {
          ++Stats.NumStoreInstructions;
        }
        // check for load instructions
        if (llvm::isa<llvm::LoadInst>(I)) {
          ++Stats.NumLoadInstructions;
        }
        // check for llvm's memory intrinsics
        if (llvm::isa<llvm::MemIntrinsic>(I)) {
          ++Stats.NumMemIntrinsics;
        }
        // check for function calls
        if (llvm::isa<llvm::CallInst>(I) || llvm::isa<llvm::InvokeInst>(I)) {
          ++Stats.NumCallSites;
          llvm::ImmutableCallSite CS(&I);
          if (CS.getCalledFunction()) {
            if (MemAllocatingFunctions.count(
                    llvm::demangle(CS.getCalledFunction()->getName().str()))) {
              // do not add allocas from llvm internal functions
              Stats.AllocaInstructions.insert(&I);
              ++Stats.NumAllocationSites;
              // check if an instance of a user-defined type is allocated on the
              // heap
              for (auto *User : I.users()) {
                if (auto *Cast = llvm::dyn_cast<llvm::BitCastInst>(User)) {
                  if (Cast->getDestTy()
                          ->getPointerElementType()
                          ->isStructTy()) {
                    // finally check for ctor call
                    for (auto *User : Cast->users()) {
                      if (llvm::isa<llvm::CallInst>(User) ||
                          llvm::isa<llvm::InvokeInst>(User)) {
                        // potential call to the structures ctor
                        llvm::ImmutableCallSite CTor(User);
                        if (CTor.getCalledFunction() &&
                            getNthFunctionArgument(CTor.getCalledFunction(), 0)
                                    ->getType() == Cast->getDestTy()) {
                          Stats.AllocatedTypes.insert(
                              Cast->getDestTy()->getPointerElementType());
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  // check for global pointers
  for (auto &Global : M.globals()) {
    if (Global.getType()->isPointerTy()) {
      ++Stats.NumGlobalPointers;
    }
    ++Stats.NumGlobals;
  }
  // register stuff in PAMM
  // For performance reasons (and out of sheer convenience) we simply initialize
  // the counter with the values of the counter varibles, i.e. PAMM simply
  // holds the results.
  PAMM_GET_INSTANCE;
  REG_COUNTER("GS Instructions", NumInstructions, PAMM_SEVERITY_LEVEL::Core);
  REG_COUNTER("GS Allocated Types", AllocatedTypes.size(),
              PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Allocation-Sites", NumAllocationSites,
              PAMM_SEVERITY_LEVEL::Core);
  REG_COUNTER("GS Basic Blocks", NumBasicBlocks, PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Call-Sites", NumCallSites, PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Functions", NumFunctions, PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Globals", NumGlobals, PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Global Pointer", NumGlobalPointers,
              PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Memory Intrinsics", NumMemIntrinsics,
              PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Store Instructions", NumStoreInstructions,
              PAMM_SEVERITY_LEVEL::Full);
  REG_COUNTER("GS Load Instructions", NumLoadInstructions,
              PAMM_SEVERITY_LEVEL::Full);
  // Using the logging guard explicitly since we are printing allocated types
  // manually
  if (boost::log::core::get()->get_logging_enabled()) {
    BOOST_LOG_SEV(lg::get(), INFO)
        << "GeneralStatisticsAnalysis summary for module: '"
        << M.getName().str() << "'";
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Instructions       : " << Stats.NumInstructions;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Allocated Types    : " << Stats.AllocatedTypes.size();
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Allocation Sites   : " << Stats.NumAllocationSites;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Basic Blocks       : " << Stats.NumBasicBlocks;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Calls Sites        : " << Stats.NumCallSites;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Functions          : " << Stats.NumFunctions;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Globals            : " << Stats.NumGlobals;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Global Pointer     : " << Stats.NumGlobalPointers;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Memory Intrinsics  : " << Stats.NumMemIntrinsics;
    BOOST_LOG_SEV(lg::get(), INFO)
        << "Store Instructions : " << Stats.NumStoreInstructions;
    BOOST_LOG_SEV(lg::get(), INFO) << ' ';
    for (const auto *Type : Stats.AllocatedTypes) {
      std::string TypeStr;
      llvm::raw_string_ostream Rso(TypeStr);
      Type->print(Rso);
      BOOST_LOG_SEV(lg::get(), INFO) << "  " << Rso.str();
    }
  }
  // now we are done and can return the results
  return Stats;
}

size_t GeneralStatistics::getAllocationsites() const {
  return NumAllocationSites;
}

size_t GeneralStatistics::getFunctioncalls() const { return NumCallSites; }

size_t GeneralStatistics::getInstructions() const { return NumInstructions; }

size_t GeneralStatistics::getGlobalPointers() const {
  return NumGlobalPointers;
}

size_t GeneralStatistics::getBasicBlocks() const { return NumBasicBlocks; }

size_t GeneralStatistics::getFunctions() const { return NumFunctions; }

size_t GeneralStatistics::getGlobals() const { return NumGlobals; }

size_t GeneralStatistics::getMemoryIntrinsics() const {
  return NumMemIntrinsics;
}

size_t GeneralStatistics::getStoreInstructions() const {
  return NumStoreInstructions;
}

set<llvm::Type *> GeneralStatistics::getAllocatedTypes() const {
  return AllocatedTypes;
}

set<llvm::Instruction *> GeneralStatistics::getAllocaInstructions() const {
  return AllocaInstructions;
}

set<llvm::Instruction *> GeneralStatistics::getRetResInstructions() const {
  return RetResInstructions;
}

} // namespace psr
