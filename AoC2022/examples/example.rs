use aoc_2022::read_content;

fn main() {
    println!("Hello");
    let filecontent = read_content("filename").unwrap();
    print!("Filename: {}",&filecontent)

}