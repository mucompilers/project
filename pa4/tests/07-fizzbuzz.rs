fn main() {
      let mut n = 1;
      while (n <= 100) {
            if (n % 3 == 0 && n % 5 == 0) {
                  prints(b"FizzBuzz");
            } else { if (n % 3 == 0) {
                  prints(b"Fizz");
            } else { if (n % 5 == 0) {
                  prints(b"Buzz");
            } else {
                  printi(n);
            }; }; };
            n += 1;
      };
}
