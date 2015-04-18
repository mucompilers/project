struct Point { x : i32, y : i32 }

fn foo(x : i32, y : bool) -> i32 { x + x; y || y; return 3; 4; }

fn main() -> () {
      let x : bool = true || false;
      let y = 4;
      let z : Point = Point { x : 1, y : 2 };

      if (x || y < 3) {
            let w : &i32 = &y;
            loop { foo(z.x + z.y, !x); }
      } else {
            let z = [1, 2, 3];
            let w : Box<[i32;3]> = Box::new(z);
            while (z[0] < z[2]) { foo((*w)[1], y != 2); }
      }
}
