#include <math.h>
#include <string.h>
#include <stdlib.h>

typedef double f64;
typedef float f32;
typedef unsigned long long u64;
typedef unsigned u32;
typedef int i32;
typedef _Bool bool;

#define check(x) if (!(x)) abort()

static void reverse(char* a, char* b) {
  while (a < --b) {
    char t = *a;
    *a++ = *b;
    *b = t;
  }
}

// computes `a * 2^b / 10^c`
static f64 calc(f64 a, i32 b, i32 c) {
  a *= pow(2, b - c);
  return c > 0 ? a / pow(5, c) : a * pow(5, -c);
}

static i32 floor_div(i32 a, i32 b) { return a / b - (a % b && (a ^ b) < 0); }

static char* write_u32(u32 x, char* dst) {
  char* begin = dst;
  do {
    *dst++ = (char) ('0' + x % 10);
    x /= 10;
  } while (x);
  reverse(begin, dst);
  return dst;
}

static char* write_negative_sci_exp(u32 nexp, char* dst) {
  *dst++ = 'e';
  *dst++ = '-';
  return write_u32(nexp, dst);
}

static char* write_negative_exp(u32 sig, u32 nexp, char* dst) {
  if (sig < 10) {
    if (nexp > 2) {
      *dst++ = (char) ('0' + sig);
      return write_negative_sci_exp(nexp, dst);
    } else {
      *dst++ = '0';
      *dst++ = '.';
      while (--nexp)
        *dst++ = '0';
      *dst++ = (char) ('0' + sig);
      return dst;
    }
  }

  // start writing out digits
  // if ever exp == 0, put the dot
  // if ever you only have one digit remaining,
  //   putting the dot would mean we add '.' and 'e{exp}' at the end, for total of 3 + exp
  //   not putting the dot would mean we add '.', {-exp} zeros, for total of 1 + -exp
  //   if exp = 2, 2nd is 3, 1st is 4
  //   if exp = 3, 2nd is 4, 1st is 4
  char* begin = dst;
  do {
    *dst++ = (char) ('0' + sig % 10);
    sig /= 10;
    --nexp;
    if (!nexp) {
      *dst++ = '.';
      do {
        *dst++ = (char) ('0' + sig % 10);
        sig /= 10;
      } while (sig);
      reverse(begin, dst);
      return dst;
    }
  } while (sig >= 10);

  if (nexp > 2) {
    *dst++ = '.';
    *dst++ = (char) ('0' + sig);
    reverse(begin, dst);
    return write_negative_sci_exp(nexp, dst);
  } else {
    *dst++ = (char) ('0' + sig);
    while (--nexp)
      *dst++ = '0';
    *dst++ = '.';
    *dst++ = '0';
    reverse(begin, dst);
    return dst;
  }
}

static char* decimal_to_string_nz(u32 sig, i32 exp, char* dst) {
  check(sig);

  while (!(sig % 10)) {  // divide off extra 0s
    sig /= 10;
    ++exp;
  }

  if (exp >= 0) {
    dst = write_u32(sig, dst);
    if (exp > 2) {
      *dst++ = 'e';
      return write_u32((u32) exp, dst);
    } else {
      while (exp--)
        *dst++ = '0';
      return dst;
    }
  } else {
    return write_negative_exp(sig, (u32) -exp, dst);
  }
}

char* decimal_to_string(i32 sig, i32 exp, char* dst) {
  if (!sig) {
    *dst++ = '0';
    return dst;
  }
  if (sig < 0) {
    *dst++ = '-';
    sig = -sig;
  }
  return decimal_to_string_nz((u32) sig, exp, dst);
}

char* float_to_string(f32 x, char* buffer) {
  u32 bits;
  memcpy(&bits, &x, 4);
  bool sign = bits >> 31;
  u32 exp_bits = (u32) (bits >> 23u) & ((1u << 8u) - 1u);
  u32 sig_bits = bits & ((1u << 23u) - 1u);

  bool is_infinite = exp_bits == 0b11111111u;

  if (is_infinite) {
    if (!sig_bits) {
      if (sign)
        *buffer++ = '-';
      memcpy(buffer, "inf", 3);
      return buffer + 3;
    } else {
      memcpy(buffer, "nan", 3);
      return buffer + 3;
    }
  }

  if (sign)
    *buffer++ = '-';

  if (!sig_bits && !exp_bits) {
    memcpy(buffer, "0e0", 3);
    return buffer + 3;
  }

  i32 exp2 = (i32) exp_bits - 149;  // 127 + 23
  u32 sig2 = sig_bits;
  if (exp_bits) {  // normalized vs denormalized
    sig2 |= 1u << 23u;
    --exp2;
  }

  // check if at bottom of exponent range (denser floats below)
  // this influences the interval width `exp10`, and the lower bound value `lo`
  i32 exp10;
  f64 lo;
  if (sig_bits) {
    exp10 = floor_div(exp2 * 30103, 100000);
    lo = calc(2 * sig2 - 1, exp2 - 1, exp10);
  } else {
    exp10 = floor_div((exp2 - 2) * 30103 + 47712, 100000);
    lo = calc(4 * sig2 - 1, exp2 - 2, exp10);
  }

  // interval upper bound `hi` is always determined by current exponent
  f64 hi = calc(2 * sig2 + 1, exp2 - 1, exp10);

  u32 a, b;
  if (sig_bits & 1) {  // odd, round away -> open interval
    a = (u32) floor(lo) + 1;
    b = (u32) ceil(hi) - 1;
  } else {  // even, round towards -> closed interval
    a = (u32) ceil(lo);
    b = (u32) floor(hi);
  }

  u32 d = b / 10;  // power of 10 candidate

  u32 sig10;
  if (d * 10 > a) {
    sig10 = d;
    ++exp10;
  } else {
    // note: this algorithm currently makes no attempt to pick the closest
    // representation if multiple valid exist, but if you wanted to do that,
    // this would be the place to do it, using round(calc(sig2, exp2, exp10))).
    sig10 = b;
  }

  return decimal_to_string_nz(sig10, exp10, buffer);
}
