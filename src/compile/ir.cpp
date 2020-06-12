#include "ir.hpp"

// ir is mostly header -- see ir.hpp

namespace ovid::ir {
static uint64_t ir_id = 0;

uint64_t next_id() { return ir_id++; }

void reset_id() { ir_id = 0; }

bool Expression::isAddressable() const { return false; }

bool Allocation::isAddressable() const {
  // allocation always addressable
  return true;
}
bool Dereference::isAddressable() const {
  // dereference of an address, so result is addressable
  return true;
}
} // namespace ovid::ir