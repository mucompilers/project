fn fact(n : i32) -> i32 {
      return if (n < 1) { 1 } else { n * fact(n - 1) };
}

fn main() {
      printi(fact(1));
      printi(fact(5));
      printi(fact(10));
}
