fn foo(x : bool) -> () {
      x + x;
      x || x;
      x[0];

      let x : i32 = 3;
      x + x;
      x || x;
      x[0];
      loop {
            let x : bool = true;
            x + x;
            x || x;
            x[0];
            if (false) {
                  let x : [i32;1] = [1];
                  x + x;
                  x || x;
                  x[0];
            };
            x + x;
            x || x;
            x[0];
      };
      x + x;
      x || x;
      x[0];
}

fn main() { }
