#include <vector>
#include <utility>

/* The base of all types. */
struct Type { };

/* Scalar types: Integers and floats. */
struct Scalar : Type { };

enum IntegerType {
  Bool = 1,
  Byte = 8,
  Short = 16,
  Int32 = 32,
  Int64 = 64,
  Int128 = 128
};

struct Integer : Scalar {
  IntegerType type;
};

enum FloatType { Half, Single, Double, Quad };

struct Float : Scalar {
  FloatType type;
};

/* Aggregate types */
struct Aggregate : Type { };

struct Array : Aggregate {
  Type base_type;
};

struct Tuple : Aggregate {
  std::vector<Type> types;
};

struct RecordField {
  char* name;
  Type type;
  char* docstring;
};

struct Record : Aggregate {
  std::vector<RecordField> fields;
};

/* Pointers */
struct Pointer : Type {
  int indirection;
  Type base_type;
};

/* Type constants */
