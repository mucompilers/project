fn main() {
    let number = 13;

    /*** Comment ***/

    match number {
        1 => { },
        2 | 3 | 5 | 7 | 11 => { },
        13...19 => { },
        _ => { },
    }
    /* 
    match number {
        1 => { },
        2 | 3 | 5 | 7 | 11 => { },
        13...19 => { },
        _ => { },
    }
    */

    let boolean = true;
    let binary = match boolean {
        false => 0,
        true => 1,
    };

    // Another comment

}

