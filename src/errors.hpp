#include <cstdio>
#include <sstream>
#include <exception>

std::string printCursor(long line, long col);

class ReaderError : public std::exception {
  long line;
  long col;
  char* str;
public:
  std::string msg();
};
