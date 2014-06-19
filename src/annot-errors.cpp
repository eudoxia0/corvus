/* The following functions represent the different errors that may be
   encountered when validating the AST. */
void formError(Atom atom, std::string explanation) {
  throw Error(atom.line, atom.col,
              "Bad '" + std::string(atom.val) + "' form: " + explanation);
}

void noBodyError(Atom atom) {
  formError(atom, "No body.");
}

void letBadBindingsError(Atom atom) {
  formError(atom, "Odd number of arguments in bindings.");
}

void fnNoNameError(Atom atom) {
  formError(atom, "No name in function definition.");
}

void fnNoArgsError(Atom atom) {
  formError(atom, "No argument list in function definition.");
}

void fnNoRetError(Atom atom) {
  formError(atom, "No return type specifier in function definition.");
}
