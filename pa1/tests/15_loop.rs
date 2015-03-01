fn foo() -> ! {
      let x = 0;
      loop {
            x + 1;
      }
}

fn main() {
    let mut count = 32;

    // Infinite loop
    loop {
        count += 1;

        if count == 3 {
            // Skip the rest of this iteration
            continue;
        }

        if count == 5 {
            // Exit this loop
            break;
        }
    }

    let mut i = 0;
    while i < 10 {
      i += 1;
    }
}
