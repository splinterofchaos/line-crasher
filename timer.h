#pragma once

#include <chrono>

class Timer {
  std::chrono::high_resolution_clock::time_point start_, end_;

public:
  Timer(std::chrono::high_resolution_clock::time_point start,
        std::chrono::high_resolution_clock::time_point end)
    : start_(start), end_(end)
  {
  }

  template<typename Rep, typename Period>
  Timer(std::chrono::high_resolution_clock::time_point start,
        std::chrono::duration<Rep, Period> duration)
    : Timer(start, start + duration)
  {
  }

  bool expired(std::chrono::high_resolution_clock::time_point now) const { 
    return now >= end_;
  }

  float ratio_consumed(
      std::chrono::high_resolution_clock::time_point now) const {
    return float((now - start_).count()) / (end_ - start_).count();
  }
};
