enum IntegerType {
    Byte = 8,
    Short = 16,
    Int32 = 32,
    Int64 = 64,
    Int128 = 128
}

enum FloatType {
    Single = 32,
    Double = 64,
    Quad = 128
}

enum Type {
    Unit,
    Bool,
    Integer(IntegerType),
    Float(FloatType)
}
