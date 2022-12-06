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
            let time_part_one = Instant::now();
            println!(
                "start-of-packetmarker: {}",
                search_sequence(current_line.as_str(), 4)
            );
            println!("Runtime: {:?}\n", time_part_one.elapsed());
        }
    }
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();
    for line in content {
        if let Ok(current_line) = line {
            let time_part_one = Instant::now();
            println!(
                "start-of-packetmarker: {}",
                search_sequence(current_line.as_str(), 14)
            );
            println!("Runtime: {:?}\n", time_part_one.elapsed());
        }
    }
}

fn search_sequence(sequence: &str, length: usize) -> i32 {
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
    const FOUR: usize = 4;
    const FOURTEEN: usize = 14;

    const SEQ1: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    const SEQ2: &str = "bvwbjplbgvbhsrlpgdmjqwftvncz";
    const SEQ3: &str = "nppdvjthqldpwncqszvftbrmjlhg";
    const SEQ4: &str = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    const SEQ5: &str = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
    
    #[test]
    fn four_should_find_seven() {
        let res = search_sequence(SEQ1, FOUR);
        assert_eq!(res, 7);
    }

    #[test]
    fn four_should_find_five() {
        let res = search_sequence(SEQ2,FOUR);
        assert_eq!(res, 5);
    }
    #[test]
    fn four_should_find_ten() {
        let res = search_sequence(SEQ4,FOUR);
        assert_eq!(res, 10);
    }

    #[test]
    fn four_should_find_eleven() {
        
        let res = search_sequence(SEQ5,FOUR);
        assert_eq!(res, 11);
    }
    
    #[test]
    fn fourteen_should_find_nineteen() {
        let res = search_sequence(SEQ1, FOURTEEN);
        assert_eq!(res, 19);
    }

    #[test]
    fn fourteen_should_find_twentytree_seq3() {
        let res = search_sequence(SEQ3, FOURTEEN);
        assert_eq!(res, 23);
    }

    #[test]
    fn fourteen_should_find_twentytree() {
        let res = search_sequence(SEQ2, FOURTEEN);
        assert_eq!(res, 23);
    }
    #[test]
    fn fourteen_should_find_twentynine() {
        
        let res = search_sequence(SEQ4, FOURTEEN);
        assert_eq!(res, 29);
    }

    #[test]
    fn fourteen_should_find_twentysix() {
        let res = search_sequence(SEQ5, FOURTEEN);
        assert_eq!(res, 26);
    }

}
