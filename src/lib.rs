#[cfg(test)]
mod tests {
    use ano_enum_codegen::{ano_enum, ano};
    use std::time::Duration;

    #[test]
    fn test_basic_types() {
        #[ano_enum]
        fn process_number(x: ano!(i32 | u32 | f32)) -> ano!(i32 | u32 | f32) {
            match ano!(x) {
                x::i32(n) => n,
                x::u32(n) => n,
                x::f32(n) => n,
            }
        }

        match process_number(5) {
            process_number::i32(n) => assert_eq!(n, 5),
            _ => panic!("Expected i32"),
        }
        match process_number(5u32) {
            process_number::u32(n) => assert_eq!(n, 5),
            _ => panic!("Expected u32"),
        }
        match process_number(5.0f32) {
            process_number::f32(n) => assert_eq!(n, 5.0),
            _ => panic!("Expected f32"),
        }
    }

    #[test]
    fn test_generic_types() {
        #[derive(Debug, PartialEq)]
        struct Wrapper<T>(T);

        #[ano_enum]
        fn process_wrapper<T: std::fmt::Debug>(x: ano!(Wrapper<T> | T)) -> ano!(Wrapper<T> | T) {
            match ano!(x) {
                x::Wrapper(w) => w,
                x::T(v) => Wrapper(v),
            }
        }

        match process_wrapper(Wrapper(5)) {
            process_wrapper::Wrapper(w) => assert_eq!(w, Wrapper(5)),
            _ => panic!("Expected Wrapper"),
        }
        match process_wrapper(5) {
            process_wrapper::Wrapper(w) => assert_eq!(w, Wrapper(5)),
            _ => panic!("Expected Wrapper"),
        }
    }

    #[test]
    fn test_multiple_parameters() {
        #[ano_enum]
        fn combine(a: ano!(i32 | String), b: ano!(i32 | String)) -> ano!(i32 | String) {
            let a_val = match ano!(a) {
                a::i32(n) => n.to_string(),
                a::String(s) => s,
            };
            let b_val = match ano!(b) {
                b::i32(n) => n.to_string(),
                b::String(s) => s,
            };
            format!("{}{}", a_val, b_val)
        }

        match combine(5, 3) {
            combine::String(s) => assert_eq!(s, "53"),
            _ => panic!("Expected String"),
        }
        match combine("Hello".to_string(), "World".to_string()) {
            combine::String(s) => assert_eq!(s, "HelloWorld"),
            _ => panic!("Expected String"),
        }
        match combine(42, " is the answer".to_string()) {
            combine::String(s) => assert_eq!(s, "42 is the answer"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_complex_types() {
        #[ano_enum]
        fn process_complex(x: ano!(Vec<i32> | Duration | String)) -> ano!(Vec<i32> | Duration | String) {
            match ano!(x) {
                x::Vec(v) => v.into_iter().map(|n| n * 2).collect::<Vec<i32>>(),
                x::Duration(d) => d * 2,
                x::String(s) => format!("{}!", s),
            }
        }

        match process_complex(vec![1, 2, 3]) {
            process_complex::Vec(v) => assert_eq!(v, vec![2, 4, 6]),
            _ => panic!("Expected Vec"),
        }
        match process_complex(Duration::from_secs(1)) {
            process_complex::Duration(d) => assert_eq!(d, Duration::from_secs(2)),
            _ => panic!("Expected Duration"),
        }
        match process_complex("test".to_string()) {
            process_complex::String(s) => assert_eq!(s, "test!"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_error_handling() {
        #[ano_enum]
        fn process_result(x: ano!(Result<i32, String> | i32)) -> ano!(Result<i32, String> | i32) {
            match ano!(x) {
                x::Result(r) => r.map(|n| n * 2),
                x::i32(n) => Ok(n * 2),
            }
        }

        match process_result(5) {
            process_result::Result(r) => assert_eq!(r, Ok(10)),
            _ => panic!("Expected Result"),
        }
        match process_result(Ok(5)) {
            process_result::Result(r) => assert_eq!(r, Ok(10)),
            _ => panic!("Expected Result"),
        }
        match process_result(Err("error".to_string())) {
            process_result::Result(r) => assert_eq!(r, Err("error".to_string())),
            _ => panic!("Expected Result"),
        }
    }

    #[test]
    fn test_example_usage() {
        mod chick {
            use ano_enum_codegen::{ano_enum, ano};
            use std::time::Duration;

            #[derive(Debug, PartialEq)]
            pub struct Bar<T>(pub T);

            #[ano_enum]
            pub fn foo<T>(b: T) -> ano!(Bar<T> | u8 | u64 | Duration) {
                let a = false;
                match a {
                    true => Bar(b),
                    false => 0_u8
                }
            }
        }

        use chick::Bar;

        // Test foo function
        let c = chick::foo(5);
        match c {
            chick::foo::u8(n) => assert_eq!(n, 0),
            _ => panic!("Expected u8"),
        }

        // Test bar1 function
        #[ano_enum]
        fn bar1(foo: ano!(i8 | u8 | Bar<u8>)) -> ano!(i8 | u8 | Bar<u8>) {
            match ano!(foo) {
                foo::u8(n) => n + 1,
                foo::i8(n) => n as u8,
                foo::Bar(n) => n.0,
            }
        }

        match bar1(5u8) {
            bar1::u8(n) => assert_eq!(n, 6),
            _ => panic!("Expected u8"),
        }
        match bar1(5i8) {
            bar1::u8(n) => assert_eq!(n, 5),
            _ => panic!("Expected u8"),
        }
        match bar1(Bar(5u8)) {
            bar1::u8(n) => assert_eq!(n, 5),
            _ => panic!("Expected u8"),
        }
    }
}
