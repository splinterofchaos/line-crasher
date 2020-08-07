#pragma once

#include <random>

void random_seed();
int random_int(int min, int max);
int random_int(int max);
bool random_bool();

inline int random_sign() { return random_bool() ? -1 : 1; }

inline float random_float(float min, float max, unsigned int precision) {
  return random_int(min * precision, max * precision) / float(precision);
}
