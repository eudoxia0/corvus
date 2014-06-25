#include <vector>
#include <map>

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
  int width() { return (int)(this->type); }
};

enum FloatType { Half, Single, Double, Quad };

struct Float : Scalar {
  FloatType type;
public:
  Float(FloatType t) : type(t) { }
  int width() { return (int)(this->type); }
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

struct TypeField {
  char* name;
  Type type;
  char* docstring;
public:
  TypeField(char* n, Type t, char* doc=NULL) :
    name(n), type(t), docstring(doc) { }
};

struct Record : Aggregate {
  std::vector<TypeField> fields;
public:
  Record(std::vector<TypeField> record_fields) : fields(record_fields) { }
};

/* Function pointers */
struct Function : Type {
  std::vector<TypeField> arg_types;
  Type return_type;
public:
  Function(std::vector<TypeField> args, Type ret) :
    arg_types(args), return_type(ret) { }
};

/* Pointers */
struct Pointer : Type {
  int indirection;
  Type base_type;
};

/* Type constants */

/*Type i1 = Integer(Bool);
Type i8 = Integer(Byte);
Type i16 = Integer(Short);
Type i32 = Integer(Int32);
Type i64 = Integer(Int64);
Type i128 = Integer(Int128);

Type HalfFloat = Float(Half);
Type SingleFloat = Float(Single);
Type DoubleFloat = Float(Double);
Type QuadFloat = Float(Quad);*/

struct TypeEnv {
  std::map<char*, Type> types;
};

/* Type Specifiers */

Type emitType(SExp* sexp, TypeEnv* tenv);
