use aoc_2022::read_lines;
use std::{
    collections::{HashSet, VecDeque},
    time::Instant,
};

const DAY: &str = "day6.txt";

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
    for line in content {
        if let Ok(current_line) = line {
            println!(
                "start-of-packetmarker: {}",
                search_sequence(current_line, 4)
            );
        }
    }
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();
    for line in content {
        if let Ok(current_line) = line {
            println!(
                "start-of-packetmarker: {}",
                search_sequence(current_line, 14)
            );
        }
    }
}

fn search_sequence(sequence: String, length: usize) -> i32 {
    let mut window: VecDeque<char> = VecDeque::<char>::new();
    let mut pos = 0;
    for c in sequence.chars() {
        window.push_back(c);
        let unique = HashSet::<char>::from_iter(window.clone());
        if unique.len() == length {
            pos += 1;
            break;
        }
        pos += 1;
        if window.len() == length {
            window.pop_front();
        }
    }
    pos
}

#[cfg(test)]
mod test {

    use crate::search_sequence;

    #[test]
    fn should_find_seven() {
        let sequence = "mjqjpqmgbljsphdztnvjfqwrcgsmlb".to_string();
        let res = search_sequence(sequence);
        assert_eq!(res, 7)
    }

    #[test]
    fn should_find_five() {
        let sequence = "bvwbjplbgvbhsrlpgdmjqwftvncz".to_string();
        let res = search_sequence(sequence);
        assert_eq!(res, 5)
    }
    #[test]
    fn should_find_ten() {
        let sequence = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".to_string();
        let res = search_sequence(sequence);
        assert_eq!(res, 10)
    }

    #[test]
    fn should_find_eleven() {
        let sequence = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".to_string();
        let res = search_sequence(sequence);
        assert_eq!(res, 11)
    }
}
