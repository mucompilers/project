struct Point { x : i32, y : i32 }
struct Car { fluff : i32, p : Point }
struct Cat { tails : [i32; 3], lives : [i32; 3] }

fn mid(p : Point, q : Point) -> Point {
      return Point { x : (p.x + q.x) / 2, y : (p.y + q.y) / 2 };
}

fn main() {
      let p : Point = Point { x: 1, y: 2 };
      let q : Point = Point { x: 5, y: 6 };

      p.x + p.y;

      let r = mid(p, q);

      let a = [r, Point { x : 3, y : 4 }];

      a[0].x + a[1].y;

      Car { fluff : 4, p : Point { x : 1, y : 3 } }.fluff + 3;
      Cat { tails : [1, 2, 3], lives : [1, 2, 3] }.lives[0];

}
