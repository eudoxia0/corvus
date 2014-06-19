#include <cstdio>
#include <sstream>
#include <exception>

std::string printCursor(long line, long col);

class Error : public std::exception {
  long line;
  long col;
  std::string str;
public:
  Error(long l, long c, std::string msg) : line(l), col(c), str(msg) { }
  std::string msg();
};
