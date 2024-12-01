use std::collections::{HashMap, HashSet};

fn main() {
    let file = include_str!("day8.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(_file: &str) -> usize {
    let (instructions, tree) = _file.split_once("\n").unwrap();
    let mut map: HashMap<ParentKey, (Left, Right)> = HashMap::new();

    println!("{:?}", instructions);
    // println!("{:?}", tree);
    for line in tree.trim().split('\n') {
        let (key, value) = parse_line(line);
        map.insert(key, value);
    }

    println!("{}", map.len());

    let mut zzz = false;
    let mut current_node = "AAA";
    let mut sequence = vec![];

    while !zzz {
        // println!("Starting" );
        for c in instructions.chars() {

            // println!("{:?}", current_node);

            let (left, right) = match map.get(current_node) {
                Some(&(Left, Right)) => (Left, Right),
                None => panic!("Invalid node: {}", current_node),
            };

            // println!("{:?}: {:?}", left, right);
            if c == 'L' {
                // println!("Left");

                sequence.push(left);
                current_node = left;
            } else {
                // println!("Right");
                sequence.push(right);
                current_node = right;
            }

            if current_node == "ZZZ" {
                zzz = true;
            }
        }
    }


    println!("{:?}", sequence.len());

    sequence.len()
}


type ParentKey<'a> = &'a str;
type Left<'a> =  &'a str;
type Right<'a> = &'a str;


fn parse_line(line: &str) -> (ParentKey, (Left, Right)) {
    let (key, child) = line.split_once("=").unwrap();

    let (left_value, right_value) = child.split_once(", ").unwrap();

    let left = &left_value.trim()[1..];
    let right = &right_value.trim()[0..right_value.trim().len() - 1];

    (key.trim(), (left, right))
}

fn part2(_file: &str) -> usize {
    let (instructions, tree) = _file.split_once("\n").unwrap();
    let mut map: HashMap<ParentKey, (Left, Right)> = HashMap::new();

    println!("{:?}", instructions);
    // println!("{:?}", tree);
    for line in tree.trim().split('\n') {
        let (key, value) = parse_line(line);
        map.insert(key, value);
    }


    let mut starts: Vec<&str> = vec![];

    for key in map.keys() {
        if key.ends_with("A") {
            starts.push(key);
        }
    }

    println!("{:?}", starts.len());

    let mut zzz = false;
    let mut count = 0;

    while !zzz {
        for c in instructions.chars() {
            let round = starts.clone();
            starts.clear();
            println!("{}", starts.len());
            for node in round {
                let (left, right) = match map.get(node) {
                    Some(&(Left, Right)) => (Left, Right),
                    None => panic!("Invalid node: {}", node),
                };

                // println!("{:?}: {:?}", left, right);
                if c == 'L' {
                    println!("{}", left);
                    starts.push(left);
                } else {
                    println!("{}", right);
                    starts.push(right);
                }
            }

            count += 1;
            let res = starts.iter().all(|&key| if key.ends_with('Z') { true } else { false});

            if res {
                zzz = true;
                println!("Part 2: {}", count);
                panic!();
            }
        }
    }

    println!("{:?}", count);

    count
}

fn part2_linesolve(_line: &str) -> usize {
    0
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part1_input() {
        let input = "LLR

        AAA = (BBB, BBB)
        BBB = (AAA, ZZZ)
        ZZZ = (ZZZ, ZZZ)";
        let sum = part1(input);
        assert_eq!(sum, 6);

    }

    #[test]
    fn part1_input_a() {
        let input = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)";
        let sum = part1(input);
        assert_eq!(sum, 2);
    }

    #[test]
    fn part1_lineinput() {}

    #[test]
    fn part1_lineinput_falsy() {}

    #[test]
    fn part2_input() {
        let input = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)";
        let sum = part2(input);
        assert_eq!(sum, 6);
    }

    #[test]
    fn part2_lineinput() {}
}
