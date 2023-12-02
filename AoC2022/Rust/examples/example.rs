use aoc_2022::read_content;

fn main() {
    println!("Hello");
    let filecontent = read_content("examples.txt").unwrap();
    print!("Filename: {}", &filecontent)
}
