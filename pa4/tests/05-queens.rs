// Solves the N-queens problem for an 8x8 board.

fn print_row(c : [i32;8]) {
      let side = 8;
      let mut i : i32 = 0;
      while (i < side) {
            if (c[i] == 9) {
                  if (i == 0) {
                        prints(b"Q _ _ _ _ _ _ _");
                  };
                  if (i == 1) {
                        prints(b"_ Q _ _ _ _ _ _");
                  };
                  if (i == 2) {
                        prints(b"_ _ Q _ _ _ _ _");
                  };
                  if (i == 3) {
                        prints(b"_ _ _ Q _ _ _ _");
                  };
                  if (i == 4) {
                        prints(b"_ _ _ _ Q _ _ _");
                  };
                  if (i == 5) {
                        prints(b"_ _ _ _ _ Q _ _");
                  };
                  if (i == 6) {
                        prints(b"_ _ _ _ _ _ Q _");
                  };
                  if (i == 7) {
                        prints(b"_ _ _ _ _ _ _ Q");
                  };
            };
            i += 1;
      };
}

fn print_board(b : [[i32;8];8]) {
      let side = 8;
      let mut i : i32 = 0;
      while (i < side) {
            print_row(b[i]);
            i += 1;
      };
      prints(b"");
}

struct Result {
      good : bool,
      b : [[i32;8];8]
}

fn place(mut b : [[i32;8];8], x : i32, y : i32) -> Result {
      let side = 8;

      if (b[x][y] != 0) {
            return Result {good: false, b: b};
      };

      let mut k : i32 = 0;
      while (k < side) {
            // Mark the row.
            if (b[x][k] == 9) {
                  return Result {good: false, b: b};
            } else {
                  b[x][k] = 1;
            };
            // Mark the column.
            if (b[k][y] == 9) {
                  return Result {good: false, b: b};
            } else {
                  b[k][y] = 1;
            };
            // Mark the diagonals.
            if (x + k < side && y + k < side) {
                  if (b[(x + k)][(y + k)] == 9) {
                        return Result {good: false, b: b};
                  } else {
                        b[(x + k)][(y + k)] = 1;
                  };
            };
            if (x + k < side && y - k >= 0) {
                  if (b[(x + k)][(y - k)] == 9) {
                        return Result {good: false, b: b};
                  } else {
                        b[(x + k)][(y - k)] = 1;
                  };
            };
            if (x - k >= 0 && y + k < side) {
                  if (b[(x - k)][(y + k)] == 9) {
                        return Result {good: false, b: b};
                  } else {
                        b[(x - k)][(y + k)] = 1;
                  };
            };
            if (x - k >= 0 && y - k >= 0) {
                  if (b[(x - k)][(y - k)] == 9) {
                        return Result {good: false, b: b};
                  } else {
                        b[(x - k)][(y - k)] = 1;
                  };
            };
            k += 1;
      };

      b[x][y] = 9;
      return Result {good: true, b: b};
}

fn try_row(b : [[i32;8];8], row : i32) {
      let nqueens = 8;
      let side = 8;

      if (row == nqueens) {
            print_board(b);
            return;
      };

      let mut i = 0;
      while (i < side) {
            let r = place(b, row, i);
            if (r.good) {
                  try_row(r.b, row + 1);
            };
            i += 1;
      };
}

fn main() {
      let b = [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
              ];

      try_row(b, 0);
}

