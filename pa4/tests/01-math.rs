fn main() {
      let x = 1 + 1;
      let y = 2 * 2;
      printi(1 + 2 + 3);
      printi(1 * 2 % 3);
      printi(x * (1 + 2) / y);
      printi(x * (x + x - x) / x);
      let mut z : i32 = y / x;
      z += 1;
      z *= z + z;
      z /= 2;
      z %= 100;
      printi(z * 3);
}
