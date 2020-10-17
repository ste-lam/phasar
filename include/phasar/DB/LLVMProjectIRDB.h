/******************************************************************************
 * Copyright (c) 2017 Philipp Schubert.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Philipp Schubert and others
 *****************************************************************************/

#ifndef PHASAR_DB_LLVMPROJECTIRDB_H_
#define PHASAR_DB_LLVMPROJECTIRDB_H_

#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"

#include "phasar/DB/ProjectIRDB.h"
#include "phasar/Utils/EnumFlags.h"

namespace llvm {
class Value;
class Instruction;
class Type;
class Function;
class GlobalVariable;
} // namespace llvm

namespace psr {

/// This class owns the LLVM IR code of the project under analysis and some
/// very important information associated with the IR.
/// When an object of this class is destroyed it will clean up all IR related
/// stuff that is stored in it.
class LLVMProjectIRDB
    : public ProjectIRDB<llvm::Module *, llvm::Function *, llvm::Instruction *,
                         llvm::GlobalVariable *, llvm::Type *,
                         llvm::Value *> {
private:
  llvm::Module *WPAModule = nullptr;
  IRDBOptions Options;
  llvm::PassBuilder PB;
  llvm::ModuleAnalysisManager MAM;
  llvm::ModulePassManager MPM;
  // Stores all allocation instructions
  std::set<llvm::Instruction *> AllocaInstructions;
  // Stores all allocated types
  std::set<llvm::Type *> AllocatedTypes;
  // Return or resum instructions
  std::set<llvm::Instruction *> RetOrResInstructions;
  // Stores the contexts
  std::vector<std::unique_ptr<llvm::LLVMContext>> Contexts;
  // Contains all modules that correspond to a project and owns them
  std::map<std::string, std::unique_ptr<llvm::Module>> Modules;
  // Maps an id to its corresponding instruction
  std::map<std::size_t, llvm::Instruction *> IDInstructionMapping;

  void buildIDModuleMapping(llvm::Module *M);

  void preprocessModule(llvm::Module *M);
  static bool wasCompiledWithDebugInfo(llvm::Module *M) {
    return M->getNamedMetadata("llvm.dbg.cu") != nullptr;
  };

  void preprocessAllModules();

public:
  /// Constructs an empty LLVMProjectIRDB
  LLVMProjectIRDB(IRDBOptions Options);
  /// Constructs a LLVMProjectIRDB from a bunch of LLVM IR files
  LLVMProjectIRDB(const std::vector<std::string> &IRFiles,
                  IRDBOptions Options = (IRDBOptions::WPA | IRDBOptions::OWNS));
  /// Constructs a ProjecIRDB from a bunch of LLVM Modules
  LLVMProjectIRDB(const std::vector<llvm::Module *> &Modules,
                  IRDBOptions Options = IRDBOptions::WPA);

  LLVMProjectIRDB(LLVMProjectIRDB &&) = default;
  LLVMProjectIRDB &operator=(LLVMProjectIRDB &&) = default;

  LLVMProjectIRDB(LLVMProjectIRDB &) = delete;
  LLVMProjectIRDB &operator=(const LLVMProjectIRDB &) = delete;

  ~LLVMProjectIRDB() override;

  void insertModule(llvm::Module *M);

  // add WPA support by providing a fat completely linked module
  void linkForWPA();
  // get a completely linked module for the WPA_MODE
  llvm::Module *getWPAModule() override;

  [[nodiscard]] inline bool
  containsSourceFile(const std::string &File) const override {
    return Modules.find(File) != Modules.end();
  };

  [[nodiscard]] inline bool empty() const override { return Modules.empty(); };

  [[nodiscard]] bool hasDebugInfo() const override;

  llvm::Module *getModule(const std::string &ModuleName) override;

  [[nodiscard]] inline std::set<llvm::Module *> getAllModules() const override {
    std::set<llvm::Module *> ModuleSet;
    for (auto &[File, Module] : Modules) {
      ModuleSet.insert(Module.get());
    }
    return ModuleSet;
  }

  [[nodiscard]] std::set<llvm::Function *> getAllFunctions() const override;

  [[nodiscard]] llvm::Function *
  getFunctionDefinition(const std::string &FunctionName) const override;

  [[nodiscard]] llvm::Function *
  getFunction(const std::string &FunctionName) const override;

  [[nodiscard]] llvm::GlobalVariable *
  getGlobalVariable(const std::string &GlobalVariableName) const override;

  [[nodiscard]] llvm::GlobalVariable *getGlobalVariableDefinition(
      const std::string &GlobalVariableName) const override;

  [[nodiscard]] llvm::Module *
  getModuleDefiningFunction(const std::string &FunctionName) const override;

  [[nodiscard]] std::set<llvm::Instruction *>
  getAllocaInstructions() const override {
    return AllocaInstructions;
  };

  /**
   * LLVM's intrinsic global variables are excluded.
   *
   * @brief Returns all stack and heap allocations, including global variables.
   */
  [[nodiscard]] std::set<llvm::Value *> getAllMemoryLocations() const override;

  [[nodiscard]] std::set<std::string> getAllSourceFiles() const override;

  [[nodiscard]] std::set<llvm::Type *> getAllocatedTypes() const override {
    return AllocatedTypes;
  };

  [[nodiscard]] std::set<llvm::Type *>
  getAllocatedStructTypes() const override;

  [[nodiscard]] std::set<llvm::Instruction *>
  getRetOrResInstructions() const override {
    return RetOrResInstructions;
  };

  llvm::Type *getStructType(const std::string &TypeName) const override;

  llvm::Type *
  getStructTypeDefinition(const std::string &TypeName) const override;

  [[nodiscard]] std::size_t getNumberOfModules() const override {
    return Modules.size();
  };

  [[nodiscard]] llvm::Instruction *getInstruction(std::size_t ID) override;

  [[nodiscard]] std::size_t
  getInstructionID(llvm::Instruction *I) const override;

  void print(std::ostream &OS = std::cout) const override;

  void emitPreprocessedIR(std::ostream &OS = std::cout,
                          bool ShortenIR = true) const override;

  /**
   * Allows the (de-)serialization of Instructions, Arguments, GlobalValues and
   * Operands into unique Hexastore string representation.
   *
   * What values can be serialized and what scheme is used?
   *
   * 	1. Instructions
   *
   * 		<function name>.<id>
   *
   * 	2. Formal parameters
   *
   *		<function name>.f<arg-no>
   *
   *	3. Global variables
   *
   *		<global variable name>
   *
   *	4. ZeroValue
   *
   *		<ZeroValueInternalName>
   *
   *	5. Operand of an instruction
   *
   *		<function name>.<id>.o.<operand no>
   *
   * @brief Creates a unique string representation for any given
   * llvm::Value.
   */
  [[nodiscard]] std::string
  valueToPersistedString(llvm::Value *V) const override;
  /**
   * @brief Convertes the given string back into the llvm::Value it represents.
   * @return Pointer to the converted llvm::Value.
   */
  [[nodiscard]] llvm::Value *
  persistedStringToValue(const std::string &StringRep) const override;
};

} // namespace psr

#endif
