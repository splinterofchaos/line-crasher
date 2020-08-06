#include "random.h"

// Globals are bad, but random is good.
static std::random_device rd;
static std::mt19937 random_gen;

void random_seed() { random_gen.seed(rd()); }

int random_int(int min, int max) {
  auto distribution = std::uniform_int_distribution(min, max - 1);
  return distribution(random_gen);
}

int random_int(int max) {
  return random_int(0, max);
}

bool random_bool() {
  auto distribution = std::uniform_int_distribution<int>(false, true);
  return distribution(random_gen);
}

