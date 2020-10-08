/******************************************************************************
 * Copyright (c) 2020 Philipp Schubert.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Philipp Schubert and others
 *****************************************************************************/

#ifndef PHASAR_DB_PROJECTIRDB_H_
#define PHASAR_DB_PROJECTIRDB_H_

#include <iostream>
#include <set>
#include <string>

namespace psr {

/// Specifies the IRDB options available.
enum class IRDBOptions : uint32_t { NONE = 0, WPA = (1 << 0), OWNS = (1 << 1) };

/// This class owns the IR code of the project under analysis and some
/// very important information associated with the IR.
/// When an object of this class is destroyed it will clean up all IR related
/// stuff that is stored in it.
///
/// M - module type
/// F - function type
/// N - instruction type
/// G - global variable type
/// T - data type type
/// V - value type
///
template <typename M, typename F, typename N, typename G, typename T,
          typename V>
class ProjectIRDB {
public:
  virtual ~ProjectIRDB() = default;

  /// Returns the fully linked WPA module that represents the entire program.
  virtual M getWPAModule() const = 0;

  /// Returns the specified module.
  virtual M getModule(const std::string &ModuleName) const = 0;

  /// Returns the module that provides the specified function definition or
  /// null.
  virtual M
  getModuleDefiningFunction(const std::string &FunctionName) const = 0;

  /// Returns all managed modules.
  virtual std::set<M> getAllModules() const = 0;

  /// Returns the number of managed modules.
  virtual std::size_t getNumberOfModules() const = 0;

  /// Returns the function's definition if available, its declaration otherwise.
  virtual F getFunction(const std::string &FunctionName) const = 0;

  /// Returns the function's definition if available, null otherwise.
  virtual F getFunctionDefinition(const std::string &FunctionName) const = 0;

  /// Returns a set of all function definitions and declarations available.
  virtual std::set<F> getAllFunctions() const = 0;

  /// Returns the global variable's definition if available, its declaration
  /// otherwise.
  virtual G getGlobalVariable(const std::string &GlobalVariableName) const = 0;

  /// Returns the global variable's definition if available, null otherwise.
  virtual G
  getGlobalVariableDefinition(const std::string &GlobalVariableName) const = 0;

  /// Returns the instruction to the corresponding ID.
  virtual N getInstruction(std::size_t Id) const = 0;

  /// Returns an instruction's ID.
  virtual std::size_t getInstructionID(N I) const = 0;

  /// Returns all stack and heap allocating instructions.
  virtual std::set<N> getAllocaInstructions() const = 0;

  /// Returns all memory locations including global variables.
  virtual std::set<V> getAllMemoryLocations() const = 0;

  /// Returns all return and resume instructions.
  virtual std::set<N> getRetOrResInstructions() const = 0;

  /// Returns the struct type's definition if available, its declaration
  /// otherwise.
  virtual T getStructType(const std::string &TypeName) const = 0;

  /// Returns the struct type's definition if available, null otherwise.
  virtual T getStructTypeDefinition(const std::string &TypeName) const = 0;

  /// Returns all allocated types.
  virtual std::set<T> getAllocatedTypes() const = 0;

  /// Returns all allocated struct types.
  virtual std::set<T> getAllocatedStructTypes() const = 0;

  /// Returns all source files managed by this ProjectIRDB.
  virtual std::set<std::string> getAllSourceFiles() const = 0;

  /// Checks if the ProjectIRDB contains the specified source file.
  virtual bool containsSourceFile(const std::string &File) const = 0;

  /// Check if ProjectIRDB is empty.
  virtual bool empty() const = 0;

  /// Check if debug information are available.
  virtual bool hasDebugInfo() const = 0;

  /// Prints the contents of this ProjectIRDB to the specified output stream.
  virtual void print(std::ostream &OS = std::cout) const = 0;

  /// Prints the potentially preprocessed contents of this ProjectIRDB to the
  /// specified output stream.
  virtual void emitPreprocessedIR(std::ostream &OS = std::cout,
                                  bool ShortenIR = true) const = 0;

  /// Builds a strings representation of the given value that can be persisted.
  virtual std::string valueToPersistedString(V Val) const = 0;

  /// Retrieves the original value that has been persisted by the given string
  /// representation.
  virtual V persistedStringToValue(const std::string &StringRep) const = 0;
};

} // namespace psr

#endif
