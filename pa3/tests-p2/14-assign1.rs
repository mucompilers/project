struct Cat { x : i32, y : [i32; 2] }

fn main() {
      let mut x : i32 = 1;
      x = 2;
      x += 2 + x;
      x *= x * 2;
      x /= 2 / x;
      x %= x % 2;

      let mut a : bool = true;
      a = true && a;
      a = a || false;

      let mut a : [i32; 2] = [0, 0];
      a[0] = x + a[1];
      a[0] += a[1];
      a = [1, 2];

      let mut b : Cat = Cat { x : 3, y : [1, 2] };
      b.x = x;
      b.y = a;
      b.y[0] = x;
      b = Cat {x : 9, y : [2, 3]};
}
