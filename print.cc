#include "print.hh"

extern "C" char* float_to_string(f32 x, char* buffer);

static void print_uint(Print& s, u64 x, u32 max) {
  u32 cur = s.chars.size;
  s.chars.expand(cur + max);
  auto it = s.chars.begin() + cur;
  auto begin = it;
  do {
    *it++ = '0' + x % 10;
    x /= 10;
  } while (x);
  auto end = it;
  while (begin < --it) {
    char tmp = *begin;
    *begin++ = *it;
    *it = tmp;
  }
  s.chars.size = u32(end - s.chars.begin());
}

static void print_int(Print& s, i64 x, u32 max) {
  if (x < 0) {
    s.chars.push('-');
    x = -x;
  }
  print_uint(s, u64(x), max);
}

void print(f32 x, Print& s) {
  u32 cur = s.chars.size;
  s.chars.expand(cur + 16);
  auto it = s.chars.begin() + cur;
  auto end = float_to_string(x, it);
  s.chars.size = u32(end - s.chars.begin());
}

void print(u8 x, Print& s) { print_uint(s, x, 3); }

void print(u16 x, Print& s) { print_uint(s, x, 5); }

void print(u32 x, Print& s) { print_uint(s, x, 10); }

void print(u64 x, Print& s) { print_uint(s, x, 20); }

void print(i8 x, Print& s) { print_int(s, x, 3); }

void print(i16 x, Print& s) { print_int(s, x, 5); }

void print(i32 x, Print& s) { print_int(s, x, 10); }

void print(i64 x, Print& s) { print_int(s, x, 20); }

void print_array(
    char const* base, u32 size, u32 count, void (*visit)(char const*, Print&),
    Print& s) {
  print('[', s);
  if (count) {
    visit(base, s);
    while (--count) {
      print(", ", s);
      visit(base += size, s);
    }
  }
  print(']', s);
}
