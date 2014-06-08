#include "ast.hpp"
#include <vector>
#include <map>

/* Each macro case associates a pattern with a template. When finding a macro
   invocation, the pattern matcher goes through every case in the macro, trying
   to match the input to the template. If it succeeds, the template is expanded
   using the variables bound during pattern matching. */
struct MacroCase {
  SExp* pattern;
  SExp* output;
};

struct Macro {
  std::vector<MacroCase> cases;
};

typedef std::map<std::string, Macro> MacroTable;

/* The following function is the sole public interface of the macro
   system. Given AST, it handles the following:

   - Extracting macro definitions from the source.
   - Finding macro invocations.
   - Performing pattern matching and macro expansion.
*/
SExp* extractMacrosAndExpand(SExp* ast);
