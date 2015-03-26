enum Colors {
    Red,
    Blue,
    Green
}

enum Numbers {
    One,
    Two,
    Three
}

enum ABCs { A, B, C }

fn foo() -> Colors { Colors::Red }
fn bar(c : Colors) -> Numbers { Numbers::One }

fn main() {
      let x : Colors = Colors::Green;
      let y : Numbers = Numbers::Two;
      let z : ABCs = ABCs::A;
}

