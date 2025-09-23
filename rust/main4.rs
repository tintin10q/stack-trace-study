fn dangerous(value1: i32, value2: i32) -> i32 { value1 / value2 }
fn foo(array: &Vec<i32>, counter: i32) -> i32 {
    if counter == 0 { return dangerous(array[0], counter); }
    foo(array, counter - 1)
}
fn main(){ let array = vec![0i32; 1000]; println!("The result is {}", foo(&array, 6)); }

