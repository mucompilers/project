fn main() {
      let mut y : i32 = 1;

      match (y) {
            ref x => { x; }
      };

      match (y) {
            ref mut x => { x; }
      };

      match (y) {
            mut x => { x; }
      };
}
