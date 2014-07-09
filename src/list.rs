pub enum List<a> {
    Value(a),
    Cons(Box<List<a>>, Box<List<a>>),
    Nil
}

pub fn mapcar<a>(list: List<a>, fun: |List<a>| -> List<a>) -> List<a> {
    match list {
        Cons(first, rest) => {
            Cons(box fun(*first), box mapcar(*rest, fun))
        },
        _ => Nil
    }
}
