// fn putstr(x: &[u8]) {
//     match std::str::from_utf8(x) {
//         Ok(s) => print!("{}", s),
//         _ => return,
//     }
// }
//
// fn putint(x: i32) { println!("{}", x); }

fn main() {
    // A simple integer calculator:
    // `+` or `-` means add or subtract by 1
    // `*` or `/` means multiply or divide by 2

    let program : &[u8] = b"++ ++ * -- /";
    let mut mp : [u8; 3] = [1, 2, 3];
    mp[1] = b'x';
    let mut accumulator = 0;

    let mut i = 0;
    while (i < program) {
        match (program[i]) {
            b'+' => {accumulator += 1;},
            b'-' => {accumulator -= 1;},
            b'*' => {accumulator *= 2;},
            b'/' => {accumulator /= 2;},
            _ => { /* ignore everything else */ }
        };
        i += 1;
    };

    putstr(b"Result: ");
    putint(accumulator);
}

struct Stuff {
      Cons(1+1)
}
