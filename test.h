#include <iostream>

template<typename T, typename U>
bool test(const char* const op_desc, T result, U expected) {
  bool ok = result == expected;
  if (!ok) {
    std::cerr << op_desc << ":\n";
    std::cerr << "       got: " << result << '\n';
    std::cerr << "  expected: " << expected << std::endl;
  }
  return ok;
}

#define TEST(op, expect) test(#op, op, expect)
#define TEST_WITH(init, op, expect) do \
  { init; test(#init"; "#op, op, expect); } while(false)
