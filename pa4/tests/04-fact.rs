fn fact(n : i32) -> i32 {
      return if (n < 1) { 1 } else { n * fact(n - 1) };
}

fn main() {
      println!("{}", fact(1));
      println!("{}", fact(5));
      println!("{}", fact(10));
}
