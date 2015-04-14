fn foo1() -> i32 { (); return 1 + 2 - 3 * 4 / 2; (); }
fn foo2() -> i32 { (); 1 + 2 - 3 * 4 / 2 }
fn foo3() { (); return 1 + 2 - 3 * 4 / 2; (); }
fn foo4() { (); 1 + 2 - 3 * 4 / 2 }
fn foo5() -> bool { (); return 1 + 2 - 3 * 4 / 2; (); }
fn foo6() -> bool { (); 1 + 2 - 3 * 4 / 2 }

fn main() { }
