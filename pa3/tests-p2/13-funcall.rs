struct Some { a : [i32; 3], b : bool }

fn foo(x : i32, y : i32, z : i32) -> i32 {
      let x = 5;
      return x + y + z;
}

fn bar(x : [i32;3], y : Some) -> i32 {
      return x[1] + y.a[1];
}

fn baz() -> [i32;3] { [1, 2, 3] }

fn cats() -> Some { Some { a : [1,2,3], b : false } }

fn main() {
      foo(1, 2, 3) == 1;
      foo(1, 2) == 3;
      foo() == 3;
      bar([1, 2, 3], Some { a : [1, 2, 3], b : false });
      bar([1, 2], Some { a : [1, 2, 3], b : false });
      bar([1, 2]);
      baz()[0];
      baz(1);
      cats().a[0] == 1;
}
