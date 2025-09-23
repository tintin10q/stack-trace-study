// Run with: RUST_BACKTRACE=1 ./bin/rust/main3
fn dangerous(array: &Vec<i32>, index: usize) -> i32 {
    array[index] // panic on OOB
}

fn foo(array: &Vec<i32>, counter: i32) -> i32 {
    if counter == 0 {
        return dangerous(array, (counter + 9137) as usize);
    }
    foo(array, counter - 1)
}

fn main() {
    let array = vec![0i32; 1000];
    let r = foo(&array, 900);
    println!("The result is {}", r);
}
