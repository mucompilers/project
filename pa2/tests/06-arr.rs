enum Lst {
    Empty,
    Cons(i32, Box<Lst>)
}

fn main() {
    let a = [1, 2, 3];

    let [mut x, y, z] = a;

    /*
     * println!("arr: {} {} {}\n", a[0], a[1], a[2]);
     */

    let list = Lst::Cons(1,
        Box::new(Lst::Cons(2,
            Box::new(Lst::Cons(3,
                Box::new(Lst::Empty))))));


    match (list) {
        Lst::Empty => { b"what"; },
        Lst::Cons(_, l) => { b"nope"; }
    }
}
