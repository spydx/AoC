use std::collections::{HashMap, HashSet};

fn main() {
    let file = include_str!("day4.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(_file: &str) -> usize {
    let sum = _file.lines().map(part1_linesolve).sum();
    sum
}

fn get_hashset(line: &str) -> HashSet<usize> {
    let temp: Vec<usize> = line
        .split(' ')
        .filter(|parts| parts.len() != 0)
        .map(|x| x.parse::<usize>().unwrap())
        .collect();
    HashSet::from_iter(temp.into_iter())
}

fn part1_linesolve(line: &str) -> usize {
    let mut parts: Vec<&str> = line.split(&[':', '|']).collect();

    let yours = get_hashset(parts.pop().unwrap());
    let winners = get_hashset(parts.pop().unwrap());

    let matches: Vec<&usize> = winners.intersection(&yours).collect();

    let mapped: Vec<usize> = matches.iter().map(|_| 1).collect();

    let sum = length_to_value(mapped.len());

    sum
}

fn length_to_value(length: usize) -> usize {
    match length {
        0 => 0,
        1 => 1,
        2 => 2,
        3 => 4,
        4 => 8,
        5 => 16,
        6 => 32,
        7 => 64,
        8 => 128,
        9 => 256,
        10 => 512,
        _ => panic!("Can't have more matches than winner numbers"),
    }
}

fn part2(_file: &str) -> usize {
    let mut _map = HashMap::<usize, usize>::new();
    for line in _file.lines() {
        part2_linesolve(&mut _map, line);
    }
    let res = _map.values().sum();
    res
}


fn part2_linesolve(map: &mut HashMap<usize, usize>, line: &str) {
    let mut parts: Vec<&str> = line.split(&[':', '|']).collect();

    let yours = get_hashset(parts.pop().unwrap());
    let winners = get_hashset(parts.pop().unwrap());
    let game: Vec<&str> = parts.pop().unwrap()
        .split(' ')
        .collect();
    let game_id: usize = game.last().unwrap().parse::<usize>().unwrap();

    let matches: Vec<usize> = winners.intersection(&yours).map(|_| 1_usize).collect();

    let target = game_id;

    let mut value = match map.get(&target) {
        Some(value) => value.clone(),
        None => 0_usize,
    };
    value += 1_usize;
    map.insert(target, value);

    let count = map.get(&target).unwrap_or(&0_usize).clone();
    for _r in 0..count {
        let mut local_target = target.clone();
        for one in matches.iter() {
            local_target += *one;
            let mut value = match map.get(&local_target) {
                Some(value) => value.clone(),
                None => 0_usize,
            };
            value += 1_usize;
            map.insert(local_target, value);
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use super::*;
    use rstest::rstest;

    #[test]
    fn part1_input() {
        let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
        let res = part1(input);
        assert_eq!(res, 13)
    }

    #[rstest]
    #[case("Card 1: 41 48 83 86 17 | 83 86  6 31 17 41 48 53", 16)]
    #[case("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", 8)]
    #[case("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", 2)]
    #[case("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 2)]
    #[case("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", 1)]
    #[case("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", 0)]
    #[case("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", 0)]
    fn part1_lineinput(#[case] input: &str, #[case] expected: usize) {
        let res = part1_linesolve(input);
        assert_eq!(res, expected);
    }

    #[test]
    fn part2_input() {
        let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
        let res = part2(input);
        assert_eq!(res, 30)
    }

    #[test]
    fn part2_lineinput() {
        let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
        let mut hashmap = HashMap::<usize, usize>::new();
        let _res = part2_linesolve(&mut hashmap, input);

        assert_eq!(hashmap.contains_key(&1), true);
        assert_eq!(hashmap.contains_key(&2), true);
        assert_eq!(hashmap.contains_key(&3), true);
        assert_eq!(hashmap.contains_key(&4), true);
        assert_eq!(hashmap.contains_key(&5), true);
        assert_eq!(hashmap.contains_key(&6), false);

        assert_eq!(hashmap.get(&1), Some(&1_usize));
        assert_eq!(hashmap.get(&2), Some(&1_usize));
        assert_eq!(hashmap.get(&3), Some(&1_usize));
        assert_eq!(hashmap.get(&4), Some(&1_usize));
        assert_eq!(hashmap.get(&5), Some(&1_usize));
        assert_eq!(hashmap.get(&6), None);
    }
}
