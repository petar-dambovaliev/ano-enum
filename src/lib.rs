#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        //bar1(Bar);
    }
}

mod chick {
    use ano_enum_codegen::{ano_enum};

    #[derive(Debug)]
    pub struct Bar<T>(T);
    use std::time::Duration;

    #[ano_enum]
    pub fn foo<T>(b: T) -> ano!(Bar<T> | u8 | u64 | Duration) {
        let a = false;
        match a {
            true => Bar(b),
            false => byte()
        }
    }

    fn byte() -> u8 {
        0_u8
    }
}

use ano_enum_codegen::{ano_enum};

fn asd() {
    let c = chick::foo(5);

    match c {
        chick::foo::u8(n) => {println!("{}", n + 1)},
        chick::foo::u64(n) => {println!("{}", n)},
        chick::foo::Bar(n) => {println!("{:#?}", n)},
        _ => {}
    }
}

use chick::Bar;

#[ano_enum]
fn bar1(foo: ano!(i8 | u8 | Bar<u8>)) {
    let a = foo;
    match ano!(a) {
        foo::u8(n) => {println!("{}", n + 1)},
        foo::i8(n) => {println!("{}", n)},
        foo::Bar(n) => {println!("{:#?}", n)},
    }
}
