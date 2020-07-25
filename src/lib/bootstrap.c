/**
 * Basic C functions to support early work on the compiler */

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

// native fn std:bootstrap:print_num(num i64) -> void
void _I3std9bootstrap9print_num(int64_t num) { printf("%li\n", num); }

// native fn std:bootstrap:print_char(c i8) -> void
void _I3std9bootstrap10print_char(int8_t c) { printf("%c", c); }

// native fn std:bootstrap:assert(cond bool) -> void
void _I3std9bootstrap6assert(_Bool cond) { assert(cond); }
