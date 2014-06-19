#include "errors.hpp"

std::string printCursor(long line, long col) {
  std::ostringstream ss;
  ss << "(line " << line << ", column " << col << ")";
  return ss.str();
}

std::string ReaderError::msg() {
  std::string out = printCursor(this->line, this->col);
  return out + this->str;
}
