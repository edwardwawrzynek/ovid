#include "escape_analysis.hpp"

namespace ovid::ir {

Flow::Flow(const FlowValue &value, const FlowValue &into)
    : value(value), into(into) {
  // types of value and into should match
  assert(value.value.type->equalToExpected(*into.value.type));
}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level)
    : value(value), indirect_level(indirect_level), field(-1) {}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level,
                     int32_t field)
    : value(value), indirect_level(indirect_level), field(field) {
  assert(field >= 0 || field == -1);
  // if field isn't -1, make sure expression is a product type
  if (field >= 0) {
    auto productType = std::dynamic_pointer_cast<ast::ProductType>(value.type);
    assert(productType != nullptr);
    assert(productType->getTypeOfField(field) != nullptr);
  }
}

bool FlowValue::isEmpty() {
  // if no indirection, value can't be empty
  if (indirect_level <= 0)
    return false;
  // otherwise, value is an indirection (dereference)
  // if it doesn't contain pointers, it is empty (nothing to be dereference)
  if (field == -1) {
    return !value.type->containsPointer();
  } else {
    auto productType = std::dynamic_pointer_cast<ast::ProductType>(value.type);
    assert(productType != nullptr);
    return !productType->getTypeOfField(field)->containsPointer();
  }
}

bool Flow::isEmpty() {
  // if value is empty, into should also be empty (b/c their types should match)
  assert(value.isEmpty() == into.isEmpty());

  return value.isEmpty();
}

} // namespace ovid::ir