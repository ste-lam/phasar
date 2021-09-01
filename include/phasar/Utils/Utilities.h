/******************************************************************************
 * Copyright (c) 2017 Philipp Schubert.
 * All rights reserved. This program and the accompanying materials are made
 * available under the terms of LICENSE.txt.
 *
 * Contributors:
 *     Philipp Schubert and others
 *****************************************************************************/

#ifndef PHASAR_UTILS_UTILITIES_H_
#define PHASAR_UTILS_UTILITIES_H_

#include <iosfwd>
#include <set>
#include <string>
#include <vector>

#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"

#include "phasar/Utils/BitVectorSet.h"

namespace llvm {
class Type;
} // namespace llvm

namespace psr {

std::string createTimeStamp();

bool isConstructor(const std::string &MangledName);

std::string debasify(const std::string &name);

const llvm::Type *stripPointer(const llvm::Type *pointer);

bool isMangled(const std::string &name);

std::vector<std::string> splitString(const std::string &str,
                                     const std::string &delimiter);

template <typename T>
std::set<std::set<T>> computePowerSet(const std::set<T> &s) {
  // compute all subsets of {a, b, c, d}
  //  bit-pattern - {d, c, b, a}
  //  0000  {}
  //  0001  {a}
  //  0010  {b}
  //  0011  {a, b}
  //  0100  {c}
  //  0101  {a, c}
  //  0110  {b, c}
  //  0111  {a, b, c}
  //  1000  {d}
  //  1001  {a, d}
  //  1010  {b, d}
  //  1011  {a, b, d}
  //  1100  {c, d}
  //  1101  {a, c, d}
  //  1110  {b, c, d}
  //  1111  {a, b, c, d}
  std::set<std::set<T>> powerset;
  for (std::size_t i = 0; i < (1 << s.size()); ++i) {
    std::set<T> subset;
    for (std::size_t j = 0; j < s.size(); ++j) {
      if ((i & (1 << j)) > 0) {
        auto it = s.begin();
        advance(it, j);
        subset.insert(*it);
      }
      powerset.insert(subset);
    }
  }
  return powerset;
}

namespace detail {

template <typename T, typename = void>
struct has_erase_iterator : std::false_type {};

template <typename T>
struct has_erase_iterator<T, std::void_t<decltype(std::declval<T>().erase(
                                 std::declval<typename T::iterator>()))>>
    : std::true_type {};

template <typename T, typename = size_t>
struct is_std_hashable : std::false_type {};
template <typename T>
struct is_std_hashable<T, decltype(std::declval<std::hash<T>>()(
                              std::declval<T>()))> : std::true_type {};

template <typename T, typename = llvm::hash_code>
struct is_llvm_hashable : std::false_type {};
template <typename T>
struct is_llvm_hashable<T, decltype(hash_value(std::declval<T>()))>
    : std::true_type {};

} // namespace detail

template <typename T>
constexpr bool has_erase_iterator_v = detail::has_erase_iterator<T>::value;

template <typename T>
constexpr bool is_std_hashable_v = detail::is_std_hashable<T>::value;

template <typename T>
constexpr bool is_llvm_hashable_v = detail::is_llvm_hashable<T>::value;

/// \brief Computes the set-intersection of the potentially unordered sets
/// Dest and Src and stores the result back in Dest.
///
/// This function should work on all types of sets as long as they provide the
/// type value_type specifying, which type their elements have.
/// By-reference iteration is required, but the elements do not have any
/// requirements, although the performance is probably higher for small
/// elements that are trivially copyable.
template <typename ContainerTy, typename OtherContainerTy>
std::enable_if_t<!has_erase_iterator_v<ContainerTy>>
intersectWith(ContainerTy &Dest, const OtherContainerTy &Src) {
  static_assert(std::is_same_v<typename ContainerTy::value_type,
                               typename OtherContainerTy::value_type>,
                "The containers Src and Dest must be compatible");
  using ValueTy = typename ContainerTy::value_type;
  using ElementTy =
      std::conditional_t<std::is_trivially_copy_constructible_v<ValueTy> &&
                             sizeof(ValueTy) <= sizeof(void *),
                         ValueTy, ValueTy *>;

  auto removeFrom = [](auto &Dst, auto &&Elem) {
    if constexpr (std::is_same_v<ValueTy, ElementTy>) {
      Dst.erase(Elem);
    } else {
      Dst.erase(*Elem);
    }
  };

  /// This whole functionality is only for computing the set-intersection of
  /// Dest and Src storing the result in-place in Dest. It would be a big win,
  /// if the STL would provide us with in-place set operations and those that
  /// do not require the sets to be sorted...

  llvm::SmallVector<ElementTy, 16> Buffer;

  if constexpr (std::is_same_v<ValueTy, ElementTy>) {
    for (auto &&Elem : Dest) {
      if (!Src.count(Elem)) {
        Buffer.push_back(Elem);
      }
    }
  } else {
    for (auto &Elem : Dest) {
      if (!Src.count(Elem)) {
        Buffer.insert(&Elem);
      }
    }
  }

  for (auto &&Elem : Buffer) {
    removeFrom(Dest, Elem);
  }
}

template <typename ContainerTy, typename OtherContainerTy>
std::enable_if_t<has_erase_iterator_v<ContainerTy>>
intersectWith(ContainerTy &Dest, const OtherContainerTy &Src) {
  static_assert(std::is_same_v<typename ContainerTy::value_type,
                               typename OtherContainerTy::value_type>,
                "The containers Src and Dest must be compatible");

  for (auto it = Dest.begin(), end = Dest.end(); it != end;) {
    if (Src.count(*it)) {
      ++it;
    } else {
      it = Dest.erase(it);
    }
  }
}

template <typename T>
void intersectWith(BitVectorSet<T> &Dest, const BitVectorSet<T> &Src) {
  Dest.setIntersectWith(Src);
}

std::ostream &operator<<(std::ostream &os, const std::vector<bool> &bits);

struct stringIDLess {
  bool operator()(const std::string &lhs, const std::string &rhs) const;
};

} // namespace psr

#endif
