use aoc_2022::{read_lines, FileBuffer};
use std::time::Instant;

const DAY: &str = "day8.txt";

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


fn generate_map(content: FileBuffer) -> Vec<Vec<i32>> {
    let mut map: Vec<Vec<i32>> = vec![];
    content.for_each(| line | {
         let temp = line.unwrap();
         let row: Vec<i32> = temp
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i32)
            .collect();
        map.push(row)
    });
    map
}

fn find_visible(map: Vec<Vec<i32>>) -> usize {
    
}


fn find_tall(row: Vec<i32>) {
    let first = row.first().unwrap();
    let last = row.last().unwrap();
    let left = if first == 9 {
        0
    };
    let right = if last == 9 {
        0
    };
    left + right
}

#[cfg(test)]
mod test {
    use crate::*;
    
    const EXAMPLE_DAY: &str = "day8_example.txt";
    #[test]
    fn test_generate_map() {
        let content = read_lines(EXAMPLE_DAY).unwrap();
        let map = generate_map(content);
        assert_eq!(map.len(), 5);
        assert_eq!(map[0].len(), 5);
        assert_eq!(map[0][0], 3);
        assert_eq!(map[3][2], 5)
    }

    #[test]
    fn test_find_visible() {
        let content = read_lines(EXAMPLE_DAY).unwrap();
        let map = generate_map(content);
        let visible = find_visible(map);
        assert_eq!(visible, 21)
    }

    #[test]
    fn 
}
