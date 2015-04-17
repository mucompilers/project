fn main() {
      let x = 1 < 2;

      let y = if (x || false) { 1 } else { 2 };
      let z : () = if (x || false) { 1 } else { 2 };
      let w : () = if (x || false) { 1; };

      if (true) { 1 + 5 } else { 1 <= 5 };
      if (y < 1 || x) { true } else { 0 };

      if (x) { () }
}
