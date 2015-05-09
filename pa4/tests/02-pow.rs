fn pow(x : i32, mut n : i32) -> i32 {
      let mut z = 1;
      while (n > 0) {
            z = z * x;
            n -= 1;
      };
      return z;
}

fn main() {
      println!("{}", pow(2, 10));
      println!("{}", pow(3, 4));
      println!("{}", pow(20, 2));
}
