fn isprime(n : i32) -> bool {
      if (n < 2) { return false; };

      let mut i = 2;
      while (i <= (n / 2)) {
            if (n % i == 0) {
                  return false;
            };
            i += 1;
      };
      return true;
}

fn main() {
      let mut i = 0;
      while (i < 100) {
            if (isprime(i)) {
                  printi(i);
            };
            i += 1;
      };
}
