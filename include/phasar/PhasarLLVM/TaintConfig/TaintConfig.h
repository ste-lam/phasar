/******************************************************************************
 * Copyright (c) 2021 Philipp Schubert.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Philipp Schubert and others
 *****************************************************************************/

#ifndef PHASAR_PHASARLLVM_TAINT_CONFIG_TAINT_CONFIG_H
#define PHASAR_PHASARLLVM_TAINT_CONFIG_TAINT_CONFIG_H

#include <functional>
#include <iostream>
#include <set>
#include <string>
#include <unordered_set>

#include "nlohmann/json.hpp"

#include "llvm/ADT/STLExtras.h" // function_ref
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Value.h"

#include "phasar/DB/ProjectIRDB.h"

namespace psr {

enum class TaintCategory { Source, Sink, Sanitizer, None };

std::string toString(TaintCategory Cat);

TaintCategory toTaintCategory(llvm::StringRef Str);

//===----------------------------------------------------------------------===//
//                            TaintConfig Class
//===----------------------------------------------------------------------===//

// This class models a taint configuration to be used to parameterize a taint
// analysis.
class TaintConfig {

  void addAllFunctions(const ProjectIRDB &IRDB, const nlohmann::json &Config);

public:
  using TaintDescriptionCallBackTy =
      std::function<std::set<const llvm::Value *>(const llvm::Instruction *)>;

  TaintConfig(const psr::ProjectIRDB &Code, const nlohmann::json &Config);
  TaintConfig(const psr::ProjectIRDB &AnnotatedCode);
  TaintConfig(TaintDescriptionCallBackTy SourceCB,
              TaintDescriptionCallBackTy SinkCB);

  void registerSourceCallBack(const TaintDescriptionCallBackTy &CB);
  void registerSinkCallBack(const TaintDescriptionCallBackTy &CB);
  [[nodiscard]] const TaintDescriptionCallBackTy &
  getRegisteredSourceCallBack() const;
  [[nodiscard]] const TaintDescriptionCallBackTy &
  getRegisteredSinkCallBack() const;

  [[nodiscard]] bool isSource(const llvm::Value *V) const;
  [[nodiscard]] bool isSink(const llvm::Value *V) const;
  [[nodiscard]] bool isSanitizer(const llvm::Value *V) const;

  /// \brief Calls Handler for all operands of Inst (maybe including Inst
  /// itself) that are generated unconditionally as tainted.
  ///
  /// If Inst is a function-call, the Callee function should be specified
  /// explicitly.
  void forAllGeneratedValuesAt(
      const llvm::Instruction *Inst, const llvm::Function *Callee,
      llvm::function_ref<void(const llvm::Value *)> Handler) const;

  /// \brief Calls Handler for all operands of Inst that may generate a leak
  /// when they are tainted.
  ///
  /// If Inst is a function-call, the Callee function should be specified
  /// explicitly.
  void forAllLeakCandidatesAt(
      const llvm::Instruction *Inst, const llvm::Function *Callee,
      llvm::function_ref<void(const llvm::Value *)> Handler) const;

  /// \brief Calls Handler for all operands of Inst that become sanitized after
  /// the instruction is completed.
  ///
  /// If Inst is a function-call, the Callee function should be specified
  /// explicitly.
  void forAllSanitizedValuesAt(
      const llvm::Instruction *Inst, const llvm::Function *Callee,
      llvm::function_ref<void(const llvm::Value *)> Handler) const;

  [[nodiscard]] bool generatesValuesAt(const llvm::Instruction *Inst,
                                       const llvm::Function *Callee) const;
  [[nodiscard]] bool mayLeakValuesAt(const llvm::Instruction *Inst,
                                     const llvm::Function *Callee) const;
  [[nodiscard]] bool sanitizesValuesAt(const llvm::Instruction *Inst,
                                       const llvm::Function *Callee) const;

  [[nodiscard]] TaintCategory getCategory(const llvm::Value *V) const;

  void addSourceValue(const llvm::Value *V);
  void addSinkValue(const llvm::Value *V);
  void addSanitizerValue(const llvm::Value *V);
  void addTaintCategory(const llvm::Value *Val, llvm::StringRef AnnotationStr);
  void addTaintCategory(const llvm::Value *Val, TaintCategory Annotation);

  [[nodiscard]] std::map<const llvm::Instruction *,
                         std::set<const llvm::Value *>>
  makeInitialSeeds() const;

  friend std::ostream &operator<<(std::ostream &OS, const TaintConfig &TC);

private:
  std::unordered_set<const llvm::Value *> SourceValues;
  std::unordered_set<const llvm::Value *> SinkValues;
  std::unordered_set<const llvm::Value *> SanitizerValues;
  TaintDescriptionCallBackTy SourceCallBack;
  TaintDescriptionCallBackTy SinkCallBack;
};

//===----------------------------------------------------------------------===//
// Miscellaneous helper functions

nlohmann::json parseTaintConfig(const std::string &Path);

} // namespace psr

#endif
