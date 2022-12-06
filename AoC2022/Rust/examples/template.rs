use aoc_2022::read_lines;
use std::time::Instant;

const DAY: &str = ".txt";

fn main() {
    let time_part_one = Instant::now();
    part_one();
    println!("Runtime: {:?}\n", time_part_one.elapsed());
    println!();
    let time_part_two = Instant::now();
    part_two();
    println!("Runtime: {:?}", time_part_two.elapsed());
}

fn part_one() {
    println!("PART I");
    let content = read_lines(DAY).unwrap();
    todo!()
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();
    todo!()
}

#[cfg(test)]
mod test {}
