/**
 * Basic C functions to support early work on the compiler */

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

// native fn std:bootstrap:print_num(num i64) -> void
void _I3std9bootstrap9print_num(int64_t num) { printf("%li\n", num); }

// native fn std:bootstrap:print_char(c u8) -> void
void _I3std9bootstrap10print_char(uint8_t c) { printf("%c", c); }

// native fn std:bootstrap:print_float(f f64) -> void
void _I3std9bootstrap11print_float(double f) { printf("%lf\n", f); }

// native fn std:bootstrap:assert(cond bool) -> void
void _I3std9bootstrap6assert(_Bool cond) { assert(cond); }

// native fn std:bootstrap:i32_to_f64(num i32) -> f64
double _I3std9bootstrap10i32_to_f64(int32_t num) { return (double)num; }
