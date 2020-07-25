/**
 * Basic C functions to support early work on the compiler */

#include <stdint.h>
#include <stdio.h>

// native fn std:bootstrap:print_num(num i64) -> void
void _I3std9bootstrap9print_num(int64_t num) { printf("%li\n", num); }
