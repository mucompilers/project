enum Colors {
      Red,
      Gold,
      Violet
}

fn main() {
      match (Colors::Red) {
         Colors::Red => { },
         Colors::Gold => { },
         Colors::Violet => { }
      }
}
