enum Either {
   Left(i32),
   Right(bool)
}

fn main() {
   let x = Either::Left(2);
   match (x) {
      Either::Left(n) => { 1; n; 3; },
      Either::Right(n) => { n; }
   }
}
  
