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

template<typename...StrLike>
std::string concat_strings(std::string s, const StrLike&...rest) {
  (s.append(rest), ...);
  return s;
}

template<typename...StrLike>
std::string concat_strings(const StrLike&...s) {
  return concat_strings(std::string(), s...);
}

