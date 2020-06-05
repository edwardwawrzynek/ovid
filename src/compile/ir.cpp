#include "ir.hpp"

// ir is mostly header -- see ir.hpp

namespace ovid::ir {
static uint64_t ir_id = 0;

uint64_t next_id() { return ir_id++; }

void reset_id() { ir_id = 0; }
} // namespace ovid::ir