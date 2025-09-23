fn dangerous(array: &Vec<i32>, index: i32) -> i32 { array[(index + 2) as usize] }
fn foo(array: &Vec<i32>, index: i32) -> i32 { dangerous(array, index) }
fn foo1(array: &Vec<i32>, index: i32) -> i32 { foo(array, index * 3) }
fn foo2(array: &Vec<i32>, index: i32) -> i32 { foo1(array, index + 137) }
fn foo3(array: &Vec<i32>, index: i32) -> i32 { foo2(array, index - 1) }
fn foo4(array: &Vec<i32>, index: i32) -> i32 { foo3(array, index * 137) }
fn foo5(array: &Vec<i32>, index: i32) -> i32 { foo4(array, index + 20) }
fn foo6(array: &Vec<i32>, index: i32) -> i32 { foo5(array, index / 3) }
fn main() {
    let array = vec![0i32; 1000];
    println!("{}", foo6(&array, 50));
}

