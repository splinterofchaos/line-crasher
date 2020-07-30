#pragma once
#include <algorithm>
#include <cmath>
#include <numeric>
#include <type_traits>

template<typename T, size_t N>
class Vec {
  T data[N];

 public:
  constexpr T* begin() { return data; }
  constexpr const T* begin() const { return data; }

  constexpr T* end() { return data + N; }
  constexpr const T* end() const { return data + N; }

  constexpr Vec() { std::fill_n(data, N, T()); }

  template<typename...U>
  constexpr Vec(U...u) {
    static_assert(sizeof...(U) == N);
    size_t i = 0;
    (((*this)[i++] = u), ...);  // TODO: This works. does the std allow it?
  }

  T& operator[](const size_t i) { return data[i]; }
  const T& operator[](const size_t i) const { return data[i]; }

  const T& x() const { static_assert(N > 0); return data[0]; }
  T& x() { static_assert(N > 0); return data[0]; }
  const T& y() const { static_assert(N > 1); return data[1]; }
  T& y() { static_assert(N > 1); return data[1]; }
  const T& z() const { static_assert(N > 2); return data[2]; }
  T& z() { static_assert(N > 2); return data[2]; }

  const T& r() const { static_assert(N > 0); return data[0]; }
  T& r() { static_assert(N > 0); return data[0]; }
  const T& g() const { static_assert(N > 1); return data[1]; }
  T& g() { static_assert(N > 1); return data[1]; }
  const T& b() const { static_assert(N > 2); return data[2]; }
  T& b() { static_assert(N > 2); return data[2]; }
};

template<typename F, typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> bimap_vec(F f, Vec<T, N> a, Vec<U, N> b) {
  Vec<std::common_type_t<T, U>, N> c;
  for (size_t i = 0; i < N; ++i) c[i] = f(a[i], b[i]);
  return c;
}

template<typename F, typename T, typename U, size_t N>
constexpr Vec<T, N> map_vec_x(F f, U u, Vec<T, N> v) {
  for (size_t i = 0; i < N; ++i) v[i] = f(u, v[i]);
  return v;
}

template<typename...U>
Vec(U...u) -> Vec<std::common_type_t<U...>, sizeof...(U)>;

template<typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> operator+(Vec<T, N> a, Vec<U, N> b) {
  return bimap_vec(std::plus{}, a, b);
}

template<typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> operator-(Vec<T, N> a, Vec<U, N> b) {
  return bimap_vec(std::minus{}, a, b);
}

template<typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> operator*(Vec<T, N> a, U b) {
  return map_vec_x(std::multiplies{}, b, a);
}

template<typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> operator*(T a, Vec<U, N> b) {
  return b * a;
}

template<typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> operator/(T a, Vec<U, N> b) {
  return map_vec_x(std::divides{}, a, b);
}

template<typename T, typename U, size_t N,
         typename X = std::common_type_t<T, U>>
constexpr Vec<X, N> operator/(Vec<T, N> a, U b) {
  auto div = [](T x, U b) { return b / x; };
  return map_vec_x(div, b, a);
}

template<typename T, typename U, size_t N>
constexpr bool operator==(Vec<T, N> a, Vec<U, N> b) {
  return std::equal(std::begin(a), std::end(a),
                    std::begin(b));
}

template<typename T, typename U, size_t N>
constexpr bool operator!=(Vec<T, N> a, Vec<U, N> b) {
  return !(a == b);
}

template<typename T, size_t N>
constexpr T magnitude_squared(Vec<T, N> a) {
  return std::accumulate(std::begin(a), std::end(a), T(0),
                         [](T acc, T x) { return acc + x * x; });
}

template<typename T, size_t N>
constexpr T magnitude(Vec<T, N> a) {
  return std::sqrt(magnitude_squared(a));
}
