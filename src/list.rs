pub enum List<a> {
    Value(Box<a>),
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

pub fn assoc<T: Eq, U>(key: T, list: List<(T, U)>) -> Option<U> {
    match list {
        Cons(first, rest) => {
            match *first {
                Value(pair) => {
                    match *pair {
                        (pair_key, pair_val) => {
                            if key == pair_key {
                                Some(pair_val)
                            } else {
                                assoc(key, *rest)
                            }
                        }
                    }
                },
                _ => None
            }
        },
        _ => None
    }
}
