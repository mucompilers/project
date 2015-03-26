struct Point { x: i32, y: i32, z: i32 }

fn main() {
    let p = Point { x: 1, y: 2, z: 3};
    let Point {x: _, y: _, z: z} = p;

    let x = match (p) {
        Point {x: 1, y: 1, z: 1} => { z },
        Point {x: _, y: 2, z: _} => { 3 },
        Point {x: mut a, y: b, z: c} => { a = b; a + b + c }
    };
}
