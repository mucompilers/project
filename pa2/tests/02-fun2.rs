fn foo(x: i32, y: i32, b: bool) -> ! { loop { } }

fn bar(a: bool, b: Box<bool>, c: i32) -> Box<i32> {
    return Box::new(4);
}

struct Point {x : i32, y : i32}

fn baz(x: Point) -> i32 { 1 }

fn main() {
      bar(false, Box::new(false), baz(Point { x:12, y:13 }));
}
