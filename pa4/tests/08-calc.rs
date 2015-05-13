
fn calc(program : [u8;5]) -> i32 {
      // A simple integer calculator:
      // `+` or `-` means add or subtract by 1
      // `*` or `/` means multiply or divide by 2
      let sz = 5;
      let mut accumulator = 0;

      let mut i = 0;
      while (i < sz) {
            if (program[i] == b'+') {
                  accumulator += 1;
            };
            if (program[i] == b'-') {
                  accumulator -= 1;
            };
            if (program[i] == b'*') {
                  accumulator *= 2;
            };
            if (program[i] == b'/') {
                  accumulator /= 2;
            };
            i = i + 1;
      };
      return accumulator;
}

fn main() {
      let program1 = [b'+', b'+', b'*', b'-', b'/'];
      let program2 = [b'+', b'*', b'*', b'*', b'*'];
      let program3 = [b'+', b'-', b'+', b'-', b'+'];
      let program4 = [b'+', b'/', b'/', b'*', b'+'];
      let program5 = [b'-', b'-', b'*', b'*', b'/'];

      printi(calc(program1));
      printi(calc(program2));
      printi(calc(program3));
      printi(calc(program4));
      printi(calc(program5));
}

