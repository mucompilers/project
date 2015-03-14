struct Point { x: i32, y: i32, z: i32 }

struct Dog {
    loc : Point,
    tailless: bool
}

fn new_dog() -> Dog { Dog { loc: Point {x: 1, y: 2, z: 3}, tailless: true }}

fn main() {
    let p : Point = Point { x: 1, y: 2, z: 3};
    let d : Dog = Dog { loc: Point {x: p.x, y: p.y, z: p.z}, tailless: false };

    d.loc.x * d.loc.y * d.loc.z;
    new_dog().loc.x - new_dog().loc.y + new_dog().loc.z;
    d.tailless || new_dog().tailless;
}
