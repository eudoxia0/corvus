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

pub fn length<a>(list: List<a>) -> i64 {
    let mut length = 0;
    mapcar(list, |elem| { length = length+1; Nil });
    length
}

pub fn append<a>(left: List<a>, right: List<a>) -> List<a> {
    match left {
        Nil => right,
        Value(val) => Nil,
        Cons(first,rest) => {
            Nil
        }
    }
}

pub fn reverse<a>(list: List<a>) -> List<a> {
    match list {
        Cons(first, rest) => {
            append(reverse(*rest),
                   Cons(first, box Nil))
        },
        _ => Nil
    }
}
