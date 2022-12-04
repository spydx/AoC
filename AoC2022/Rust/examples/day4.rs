use aoc_2022::{read_lines, FileBuffer};
use std::time::Instant;

const DAY: &str = "day4.txt";

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
    let _content = read_lines(DAY).unwrap();
    let number_of_pairs = process_lines(_content, range_overlap);
    println!("Number of pairs: {}", number_of_pairs)
}

fn part_two() {
    println!("PART II");
    let _content = read_lines(DAY).unwrap();
    let number_of_pairs = process_lines(_content, range_overlap_part2);
    println!("Number of pairs: {}", number_of_pairs)
}


fn process_lines(_content: FileBuffer, f: fn(l1: Vec<i32>, l2: Vec<i32>) -> bool) -> i32 {
    let mut number_of_pairs = 0;
    for line in _content {
        if let Ok(current_line) = line {
            let (first_pair, second_pair) = get_pair_range(current_line.as_str());
            if f(first_pair, second_pair) { 
                number_of_pairs += 1;
            }
        }
    }
    number_of_pairs
}

fn range_overlap(first_pair: Vec<i32>, second_pair: Vec<i32>) -> bool {
    //let mut common: Vec<i32> = vec![];
    let first_result = first_pair.clone()
        .into_iter()
        .map(|x| {
            if second_pair.contains(&x) {
                true
            } else {
                false
            }
        })
        .all(|value| value == true);

    let second_result = second_pair.clone()
        .into_iter()
        .map(|x| {
            if first_pair.contains(&x) {
                true
            } else {
                false
            }
        }).all(|value| value == true);
    
    first_result || second_result
}

fn range_overlap_part2(first_pair: Vec<i32>, second_pair: Vec<i32>) -> bool {
    let first_result = first_pair.clone()
    .into_iter()
    .map(|x| {
        if second_pair.contains(&x) {
            true
        } else {
            false
        }
    })
    .any(|value| value == true);

    let second_result = second_pair.clone()
        .into_iter()
        .map(|x| {
            if first_pair.contains(&x) {
                true
            } else {
                false
            }
        }).any(|value| value == true);
    first_result || second_result
}

fn get_number_range(range: Vec<&str>) -> (i32, i32) {
    let start = range.first().map(|x| x.parse::<i32>().unwrap()).unwrap();
    let end = range.last().map(|x| x.parse::<i32>().unwrap()).unwrap();
    (start, end)
}

fn get_pair_range(par: &str) -> (Vec<i32>, Vec<i32>) {
    let vector:Vec<&str> = par.split(',').collect();
    let first = vector.first().unwrap().split('-').collect();
    let second = vector.last().unwrap().split('-').collect();

    
    let first_range = get_number_range(first);
    let second_range = get_number_range(second);

    let first_pair = (first_range.0..first_range.1 + 1).map(|x| x as i32).collect();
    let second_pair = (second_range.0..second_range.1 + 1).map(|x| x as i32).collect();
    (first_pair, second_pair)
}


#[cfg(test)]
mod test {
    use crate::get_pair_range;
    use crate::range_overlap;
    use crate::range_overlap_part2;

    #[test]
    fn get_pair_range_from_input() {
        let pairstring = "2-4,6-8";
        let (firstrange, secondrange) = get_pair_range(pairstring);

        assert!(firstrange.contains(&2) && firstrange.contains(&3) && firstrange.contains(&4));
        assert!(secondrange.contains(&6) && secondrange.contains(&7) && secondrange.contains(&8));
    }
   
    #[test]
    fn get_pair_range_from_input_largenumbers() {
        let pairstring = "12-80,12-81";
        let (firstrange, secondrange) = get_pair_range(pairstring);

        assert!(firstrange.contains(&12) && firstrange.contains(&80));
        assert!(secondrange.contains(&12) && secondrange.contains(&81));
    }

    #[test]
    fn get_pair_range_from_short_input() {
        let pairstring = "2-2,6-8";
        let (firstrange, secondrange) = get_pair_range(pairstring);

        assert!(firstrange.contains(&2));
        assert!(secondrange.contains(&6) && secondrange.contains(&7) && secondrange.contains(&8));
    }

    #[test]
    fn should_contain_range() {
        let pairstring = "2-8,3-7";
        let (first_pair, second_pair) = get_pair_range(pairstring);
        let contains = range_overlap(first_pair, second_pair);
        assert_eq!(contains, true);
    }


    #[test]
    fn should_contain_range_single() {
        let pairstring = "6-6,4-6";
        let (first_pair, second_pair) = get_pair_range(pairstring);
        let contains = range_overlap(first_pair, second_pair);
        assert_eq!(contains, true);
    }

    #[test]
    fn should_not_contain_range() {
        let pairstring = "2-4,6-8";
        let (first_pair, second_pair) = get_pair_range(pairstring);
        let contains = range_overlap(first_pair, second_pair);
        assert_eq!(contains, false);
    }

    #[test]
    fn part2_should_overlap() {
        let pairstring = "5-7,7-9";
        let (first_pair, second_pair) = get_pair_range(pairstring);
        let contains = range_overlap_part2(first_pair, second_pair);
        assert_eq!(contains, true);
    }
}
