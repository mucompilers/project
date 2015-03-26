fn main() {
      let a : [bool;3] = [true, true, true];
      let [mut x, y, z] = a;

      let [[mut m], [mut n], [mut o], [mut p]] = [[1], [2], [3], [4]];

      match ([[1],[2],[3],[4]]) {
            [_, b, c, _] => {m + n + o + p;}
      };

      match ([[1]]) {
            [[2]] => { },
            [[x]] => { }
      };
}
