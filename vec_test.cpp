#include <iostream>
#include <cstdlib>

#include "vec.h"
#include "test.h"

template<typename T, size_t N>
std::ostream& operator<<(std::ostream& os, Vec<T, N> v) {
  os << '<';
  bool first = true;
  for (int x : v) {
    if (!first) os << ", ";
    os << x;
    first = false;
  }
  return os << '>';
}

int main() {
  TEST(Vec(1, 1) + Vec(1.0, 1.0), Vec(2.0, 2.0));
  TEST(Vec(1, 1) - Vec(1.0, 1.0), Vec(0.0, 0.0));
  TEST(Vec(1, 1, 1) - Vec(1, 1, 1), Vec(0, 0, 0));
  TEST(Vec(1, 2, 3) * 2, Vec(2, 4, 6));
  TEST(Vec(1, 2, 3) / 2, Vec(0, 1, 1));
  TEST(2 / Vec(1, 2, 3), Vec(2, 1, 0));
  TEST(magnitude_squared(Vec(3,4)), 5 * 5);
  TEST(magnitude(Vec(3,4)), 5);
}
