fn main() {
      let x = 1 + 1;
      let y = 2 * 2;
      println!("{}", 1 + 2 + 3);
      println!("{}", 1 * 2 % 3);
      println!("{}", x * (1 + 2) / y);
      println!("{}", x * (x + x - x) / x);
      let mut z : i32 = y / x;
      z += 1;
      z *= z + z;
      z /= 2;
      z %= 100;
      println!("{}", z * 3);
}
