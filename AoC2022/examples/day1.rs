use std::vec;

use aoc_2022::{read_lines};

const DAY:&str = "day1.txt";

fn main() {

    part_one();

    part_two();
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();

    let mut first: i64 = 0;
    let mut second: i64 = 0;
    let mut third: i64 = 0;

    let mut first_p: i32 = 0;
    let mut second_p: i32 = 0;
    let mut third_p : i32 = 0;

    let mut current: i64 = 0;
    
    let mut elf:i32 = 0;
    
    for line in content {
        if let Ok(current_line) = line {
            if current_line.is_empty() {
                elf += 1;
                
                if current > first {
                    third = second;
                    second = first;
                    first = current;
                    
                    third_p = second_p;
                    second_p = first_p;
                    first_p = elf;
                } else if current > second {
                    third = second;
                    second = current;

                    third_p = second_p;
                    second_p = elf;

                } else if current > third {
                    third = current;
                    third_p = elf;
                }
                current = 0;
            } else {
                let calories = current_line.parse::<i64>().unwrap();
                current += calories;
            }
        }
    }

    let res:i64 = vec![first, second, third].into_iter().sum();
    
    println!("Searched many {} elfs", elf);
    println!("Found at: {:?}", vec![first_p, second_p, third_p]);
    println!("Total Calories {}", res);

}


fn part_one() {
    println!("PART I");
    let content = read_lines(DAY).unwrap();

    let mut max:i64 = 0;
    let mut current : i64 = 0;
    let mut elf: i64 = 0;
    let mut position: i64 = 0;

    for line in content {
        if let Ok(current_line) = line {
            if current_line.is_empty() {
                elf += 1;
                if current > max {
                    max = current;
                    position = elf;
                }
                current = 0;
            } else {
                let calories = current_line.parse::<i64>().unwrap();
                current += calories;
            }
            
            
        }
    }
    println!("Searched many {} elfs", elf);
    println!("Found at: {}", position);
    println!("Max Calories {}", max);
    println!();
}