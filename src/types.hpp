#include <vector>
#include <utility>
#include <cstdlib>

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
public:
  Integer(IntegerType t) : type(t) { }
};

enum FloatType { Half, Single, Double, Quad };

struct Float : Scalar {
  FloatType type;
public:
  Float(FloatType t) : type(t) { }
};

/* Aggregate types */
struct Aggregate : Type { };

struct Array : Aggregate {
  Type base_type;
public:
  Array(Type type) : base_type(type) { }
};

struct Tuple : Aggregate {
  std::vector<Type> types;
public:
  Tuple(std::vector<Type> tup_types) : types(tup_types) { }
};

struct RecordField {
  char* name;
  Type type;
  char* docstring;
public:
  RecordField(char* n, Type t, char* doc=NULL) :
    name(n), type(t), docstring(doc) { }
};

struct Record : Aggregate {
  std::vector<RecordField> fields;
public:
  Record(std::vector<RecordField> record_fields) : fields(record_fields) { }
};

/* Pointers */
struct Pointer : Type {
  int indirection;
  Type base_type;
};

/* Type constants */
