#pragma once

#include <algorithm>
#include <tuple>
#include <vector>

// === tuple utils ===
template<typename F, typename...T>
constexpr auto tuple_map(F&& f, const std::tuple<T...>& t) {
  return std::tuple(f(std::get<T>(t))...);
}

template<typename F, typename...T>
auto tuple_map(F&& f, std::tuple<T...>& t) {
  return std::tuple(f(std::get<T>(t))...);
}

template<typename F, typename...T>
constexpr auto tuple_map_forward(F&& f, const std::tuple<T...>& t) {
  return std::forward_as_tuple(f(std::get<T>(t))...);
}

template<typename F, typename...T>
auto tuple_map_forward(F&& f, std::tuple<T...>& t) {
  return std::forward_as_tuple(f(std::get<T>(t))...);
}

template<typename F, typename...T>
constexpr void tuple_foreach(F&& f, const std::tuple<T...>& t) {
  (f(std::get<T>(t)), ...);
}

template<typename F, typename...T>
void tuple_foreach(F&& f, std::tuple<T...>& t) {
  (f(std::get<T>(t)), ...);
}

// === errors ===
struct Error {
  bool ok = true;
  std::string reason;

  Error() { }
  Error(std::string reason) : ok(false), reason(std::move(reason)) { }
};

inline Error operator&&(const Error& e, const Error& e2) {
  if (!e.ok) return e;
  return e2;
}

// === misc ===
template<typename...StrLike>
std::string concat_strings(std::string s, const StrLike&...rest) {
  (s.append(rest), ...);
  return s;
}

template<typename...StrLike>
std::string concat_strings(const StrLike&...s) {
  return concat_strings(std::string(), s...);
}

template<typename Container, typename T, typename F = std::less<>>
typename Container::iterator lower_bound(Container& c, const T& t,
                                         F&& f = F()) {
  return std::lower_bound(c.begin(), c.end(), t, std::forward<F>(f));
}

template<typename Container, typename T, typename F = std::less<>>
typename Container::const_iterator lower_bound(const Container& c, const T& t,
                                               F&& f = F()) {
  return std::lower_bound(c.begin(), c.end(), t, std::forward<F>(f));
}

struct Identity {
  template<typename T>
  T operator()(T&& t) const { return std::forward<T>(t); }
};
static constexpr Identity identity;

template<typename F>
constexpr auto invoker(F f) {
  return [f](auto&&...x) { return std::invoke(f, x...); };
}

template<typename T>
class SortedVector {
  std::vector<T> data_;

public:
  using iterator = std::vector<T>::iterator;
  using const_iterator = std::vector<T>::const_iterator;
  using value_type = std::vector<T>::value_type;

  SortedVector() { }

  std::size_t size() const { return data_.size(); }

  T& operator[](std::size_t i) { return data_[i]; }
  const T& operator[](std::size_t i) const { return data_[i]; }

  iterator begin() { return data_.begin(); }
  iterator end() { return data_.end(); }

  const_iterator begin() const { return data_.begin(); }
  const_iterator end() const { return data_.end(); }

  value_type& back() { return data_.back(); }
  const value_type& back() const { return data_.back(); }

  template<typename U, typename Key = Identity>
  std::pair<iterator, bool> find(const U& u, Key key = Key()) {
    auto pred = [key](const value_type& v, const U& u) { return key(v) < u; };
    auto it = lower_bound(data_, u, pred);
    return std::pair(it, it != data_.end() && key(*it) == u);
  }

  template<typename U, typename Key = Identity>
  std::pair<const_iterator, bool> find(const U& u, Key key = Key()) const {
    auto pred = [key](const value_type& v, const U& u) { return key(v) < u; };
    auto it = lower_bound(data_, u, pred);
    return std::pair(it, it != data_.end() && key(*it) == u);
  }

  template<typename U, typename Key= Identity>
  bool contains(const U& u, Key key = Key()) const {
    return find(u, key).second;
  }

  void clear() { data_.clear(); }

  // TODO: Assert that v > this->back(). For now, callers are responsible for
  // keeping this sorted.
  void push_back(value_type v) { data_.push_back(std::move(v)); }
  void emplace_back(value_type v) { data_.emplace_back(std::move(v)); }
  template<typename U>
  void push_back(U&& u) { data_.push_back(std::forward<U>(u)); }
  template<typename U>
  void emplace_back(U&& u) { data_.emplace_back(std::forward<U>(u)); }

  void pop_back() { data_.pop_back(); }

  template<typename...U>
  iterator emplace(const_iterator it, U&&...u) {
    return data_.emplace(it, std::forward<U>(u)...);
  }

  template<typename Pred>
  std::size_t erase_if(Pred&& pred) {
    return std::erase_if(data_, std::forward<Pred>(pred));
  }

  // Here's where we break from the standard bigly. insert() is a badly named
  // function because it's unclear what it does. Be more explicit:
  template<typename U>
  std::pair<iterator, bool> insert_if_not_present(U&& u) {
    auto [it, found] = find(u);
    if (!found) it = data_.insert(it, std::forward<U>(u));
    return std::pair(it, !found);
  }

  template<typename U>
  iterator insert_or_update(U&& u) {
    auto [it, did_insert] = insert_if_not_present(std::forward<U>(u));
    if (!did_insert) *it = std::forward<U>(u);
    return it;
  }

  template<typename U>
  std::pair<iterator, bool> update(U&& u) {
    auto [it, found] = find(u);
    if (found) *it = std::forward<U>(u);
    return std::pair(it, found);
  }

  void erase(const_iterator it) {
    data_.erase(it);
  }

  template<typename U, typename Key = Identity>
  void find_erase(const U& u, Key key = Key()) {
    auto [it, found] = find(u, key);
    if (found) erase(it);
  }
};
