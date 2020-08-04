#pragma once

#include <algorithm>
#include <tuple>

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
