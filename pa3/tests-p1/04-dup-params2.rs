fn main() -> () { }
fn foo(x:i32, y:bool, z: [i32;2], x:i32, w: &i32) -> i32 { 
      if (y) {
            x + x + *w
      } else {
            x + x
      }
}

fn bar(x:i32) { }
