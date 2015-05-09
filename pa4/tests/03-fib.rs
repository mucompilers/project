fn fib(n : i32) -> i32 {
      if (n < 1) { return 1; }
      else { return fib(n - 2) + fib(n - 1); };
}

fn main() {
      let mut n = 0;
      while (n < 8) {
            printi(fib(n));
            n += 1;
      };
}
