#include <stdint.h>

uint64_t umulh(uint64_t x, uint64_t y) {
  return ((unsigned __int128)x * y) >> 64;
}

uint64_t umodh(uint64_t lo, uint64_t hi, uint64_t m) {
  return (((unsigned __int128)hi << 64) + lo) % m;
}
