struct Point { x: i32, y: i32, z: i32 }

struct Dog {
    loc : Point,
    tailless: bool
}

fn main() {
    let p : Point = Point { x: 1, y: 2, z: 3};
    let d : Dog = Dog { loc: p, tailless: false };
}
