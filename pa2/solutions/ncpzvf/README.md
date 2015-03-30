Prepare for execution:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

Run with interaction (CTRL-D to exit):

    $ cabal run

Run with file input:

    $ cabal run < test.rs

test.rs:
```
#!rust

fn main() {
    if b'c' == b'\'' {
        b" \"\\\" "
    }
}
```
