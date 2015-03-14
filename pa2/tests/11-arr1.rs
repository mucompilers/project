fn main() {
      let a : [bool; 3] = [true, false, false];
      let x = a[0];
      a[0];
      a[1 - 1];
      a[if (a[0]) { 0 } else { 1 }];
}
