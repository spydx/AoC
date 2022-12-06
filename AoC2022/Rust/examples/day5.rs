use aoc_2022::read_lines;
use std::{collections::HashMap, time::Instant};

const DAY: &str = "day5.txt";

fn main() {
    let time_part_one = Instant::now();
    part_one(DAY, 10, make_move);
    println!("Runtime: {:?}\n", time_part_one.elapsed());
    println!();
    let time_part_two = Instant::now();
    part_one(DAY, 10, make_move_sameorder);
    println!("Runtime: {:?}", time_part_two.elapsed());
}

fn part_one(filename: &str, item: usize, f: fn(usize, usize, usize, HashMap<usize, String>) -> HashMap<usize, String>) {
    println!("PART I");
    let content = read_lines(filename).unwrap();

    let mut stack: Vec<String> = vec![];

    let mut map = HashMap::<usize, String>::new();
    let mut makemove = false;

    for line in content {
        if let Ok(current_line) = line {
            if !current_line.is_empty() && !makemove {
                stack.push(current_line);
            } else if current_line.is_empty() {
                stack.pop();
                map = build_map(&stack,item);
                makemove = true;
            } else {
                let (count, from, to) = get_move(current_line);
                map = f(count, from, to, map);
            }
        }
    }
    println!("{:?}", map);
    let res = get_result(map, item);
    println!("{:?}", res);
}

fn build_map(stack: &Vec<String>, items: usize) -> HashMap<usize, String> {
    let mut map = HashMap::<usize, String>::new();

    for i in 1..items {
        map.insert(i, "".to_string());
    }
    for line in stack {
        println!("{}", line);
        let content = extract_line(line.clone());
        for i in 1..items {
            map.entry(i).and_modify(|v| {
                if !content[i - 1].is_whitespace() {
                    v.push(content[i - 1])
                }
            });
        }
    }

    map
}

fn extract_line(line: String) -> Vec<char> {
    let linevec: Vec<char> = line.chars().collect();
    let mut value = vec![];
    for i in (1..linevec.len()).step_by(4) {
        value.push(linevec[i]);
    }
    value
}

fn get_move(line: String) -> (usize, usize, usize) {
    println!("{}",line);
    let numbers: Vec<i32> = line
        .split_whitespace()
        .filter_map(|s| {
            if s.starts_with("m") || s.starts_with("f") || s.starts_with("t") {
                None
            } else {
                Some(s.parse().expect("upsi"))
            }
        })
        .collect();
    (
        numbers[0] as usize,
        numbers[1] as usize,
        numbers[2] as usize,
    )
}

fn make_move(
    count: usize,
    from: usize,
    to: usize,
    mut map: HashMap<usize, String>,
) -> HashMap<usize, String> {
    println!("{:?}", map);
    let mut move_block: Vec<char> = vec![' '; count];
    let mut from_values: Vec<char> = map.get(&from).unwrap().chars().collect();
    move_block.swap_with_slice(&mut from_values[0..count]);

    let from_values: String = from_values
        .into_iter()
        .filter(|f| !f.is_whitespace())
        .collect();

    let mut to_values: Vec<char> = map.get(&to).unwrap().chars().rev().collect();

    to_values.append(&mut move_block);

    let to_values: String = to_values
        .into_iter()
        .rev()
        .collect();

    map.insert(from, from_values);
    map.insert(to, to_values);
    println!("{:?}", map);
    map

}

fn make_move_sameorder(
    count: usize,
    from: usize,
    to: usize,
    mut map: HashMap<usize, String>,
) -> HashMap<usize, String> {
    println!("{:?}", map);
    let mut move_block: Vec<char> = vec![' '; count];
    let mut from_values: Vec<char> = map.get(&from).unwrap().chars().collect();
    move_block.swap_with_slice(&mut from_values[0..count]);
    move_block.reverse();

    let from_values: String = from_values
        .into_iter()
        .filter(|f| !f.is_whitespace())
        .collect();

    let mut to_values: Vec<char> = map.get(&to).unwrap().chars().rev().collect();

    to_values.append(&mut move_block);

    let to_values: String = to_values
        .into_iter()
        .rev()
        .collect();

    map.insert(from, from_values);
    map.insert(to, to_values);
    println!("{:?}", map);
    map

}


fn get_result(map: HashMap<usize, String>, item: usize) -> String {
    let mut result = String::from("");
    for key in 1..item {
        let value = map.get(&key).unwrap();
        result.push(value.chars().rev().last().unwrap())
    }
    result
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{build_map, extract_line, get_move};

    #[test]
    fn one_line() {
        let stringvalue = "    [G] [R]                 [P]    ".to_string();
        let value = extract_line(stringvalue);
        println!("{:?}", value);
    }

    #[test]
    fn one_line_index() {
        let stringvalue = " 1   2   3   4   5   6   7   8   9 ".to_string();
        let value = extract_line(stringvalue);
        assert_eq!(value.first(), Some(&'1'));
        assert_eq!(value.last(), Some(&'9'));
    }

    #[test]
    fn from_string_to_hashmap() {
        let content = read_lines(DAY).unwrap();
        let mut stack: Vec<String> = vec![];

        let mut map = HashMap::<usize, String>::new();
        for line in content {
            if let Ok(current_line) = line {
                if !current_line.is_empty() {
                    stack.push(current_line);
                } else if current_line.is_empty() {
                    stack.pop();
                    map = build_map(&stack, 10);
                    break;
                }
            }
        }
        assert_eq!(map.get(&1).unwrap(), "LCGMQ");
    }

    #[test]
    fn get_move_test() {
        let move_line = "move 5 from 8 to 2".to_string();
        let (no, from, to) = get_move(move_line);
        assert_eq!(no, 5);
        assert_eq!(from, 8);
        assert_eq!(to, 2);
    }

    #[test]
    fn make_move_on_map() {
        let content = read_lines(DAY).unwrap();
        let mut stack: Vec<String> = vec![];

        let mut map = HashMap::<usize, String>::new();
        for line in content {
            if let Ok(current_line) = line {
                if !current_line.is_empty() {
                    stack.push(current_line);
                } else if current_line.is_empty() {
                    stack.pop();
                    map = build_map(&stack, 10);
                    break;
                }
            }
        }
        assert_eq!(map.get(&1).unwrap(), "LCGMQ");

        let move_line = "move 5 from 8 to 2".to_string();
        let (count, from, to) = get_move(move_line);
        map = make_move(count, from, to, map);
        let numbertwo = map.get(&2_usize).unwrap();
        let numberetigh = map.get(&8_usize).unwrap();

        assert_eq!(numbertwo, "VZNHPGHFTCLDR");
        assert_eq!(numberetigh, "GSJ");
        println!("{}", map.get(&2_usize).unwrap());
        println!("{}", map.get(&8_usize).unwrap());
        println!("{:?}", map);
    }

    #[test]
    fn get_last_char() {
        let content = read_lines(DAY).unwrap();
        let mut stack: Vec<String> = vec![];

        let mut map = HashMap::<usize, String>::new();
        for line in content {
            if let Ok(current_line) = line {
                if !current_line.is_empty() {
                    stack.push(current_line);
                } else if current_line.is_empty() {
                    stack.pop();
                    map = build_map(&stack, 10);
                    break;
                }
            }
        }
        assert_eq!(map.get(&1).unwrap(), "LCGMQ");
        let result = get_result(map, 10);
        assert_eq!(result, "LGRPTPTPG")
    }

    #[test]
    fn test_solve() {
        part_one("day5_example.txt", 4);
    }
}
