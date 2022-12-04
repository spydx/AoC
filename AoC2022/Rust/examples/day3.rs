use aoc_2022::read_lines;
use std::time::Instant;

const DAY: &str = "day3.txt";

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

    let mut sum = 0;
    for line in content {
        if let Ok(current_line) = line {
            let rucksack: Vec<char> = current_line.chars().collect();
            let found = find_item_type(rucksack);
            let value = get_prority(found);
            sum += value;
        }
    }

    println!("{}", sum);
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();

    let mut sum = 0;
    let mut rucksacks: Vec<Vec<char>> = vec![];
    for line in content {
        if let Ok(current_line) = line {
            let rucksack = current_line.chars().collect();
            rucksacks.push(rucksack);
        }

        if rucksacks.len() == 3 {
            let group_type = find_group_item(
                rucksacks[0].clone(),
                rucksacks[1].clone(),
                rucksacks[2].clone(),
            );
            let value = get_prority(group_type);
            sum += value;
            rucksacks.clear();
        }
    }

    println!("{}", sum);
}

fn find_item_type(rucksack: Vec<char>) -> char {
    let index = rucksack.len() / 2;

    let mut rucksack_c1 = rucksack.to_owned();
    let mut rucksack_c2: Vec<char> = rucksack_c1.split_off(index);
    rucksack_c1.dedup();
    rucksack_c2.dedup();

    let res1: Vec<char> = rucksack_c1
        .into_iter()
        .filter(|item| rucksack_c2.contains(item))
        .collect();

    // if res1.is_empty() {
    //     let res2:Vec<char> = rucksack_c2
    //         .into_iter()
    //         .filter(|item| rucksack_c1.contains(item))
    //         .collect();
    //     return *res2.first().unwrap()
    // }

    *res1.first().unwrap()
}

fn find_group_item(mut r1: Vec<char>, mut r2: Vec<char>, mut r3: Vec<char>) -> char {
    r1.dedup();
    r2.dedup();
    r3.dedup();

    let found: Vec<char> = r1
        .into_iter()
        .filter(|item| r2.contains(item) && r3.contains(item))
        .collect();
    *found.first().unwrap()
}

fn get_prority(letter: char) -> i32 {
    let value = if letter.is_ascii_uppercase() {
        letter as i32 - 38
    } else {
        letter as i32 - 96
    };
    value
}

#[allow(non_snake_case)]
#[cfg(test)]
mod test {
    use crate::find_group_item;
    use crate::find_item_type;
    use crate::get_prority;

    #[test]
    fn test_small_char_pri() {
        let value = get_prority('p');
        assert_eq!(value, 16);
        let value = get_prority('a');
        assert_eq!(value, 1);
        let value = get_prority('r');
        assert_eq!(value, 18);
    }

    #[test]
    fn test_large_char_pri() {
        let value = get_prority('A');
        assert_eq!(value, 27);
        let value = get_prority('L');
        assert_eq!(value, 38);
        let value = get_prority('Z');
        assert_eq!(value, 52);
    }

    #[test]
    fn test_get_priority() {
        let pritoryletters = vec![
            ('p', 16),
            ('L', 38),
            ('P', 42),
            ('v', 22),
            ('t', 20),
            ('s', 19),
        ];
        let mut sum = 0;
        for (letter, pri) in pritoryletters {
            let value = get_prority(letter);
            sum += pri;
            assert_eq!(value, pri);
        }
        assert_eq!(sum, 157);
    }

    #[test]
    fn test_find_item_type_should_be_p() {
        let rucksack: Vec<char> = "vJrwpWtwJgWrhcsFMMfFFhFp".chars().collect();
        let found_type = find_item_type(rucksack);
        assert_eq!(found_type, 'p');
    }

    #[test]
    fn test_find_item_type_should_be_L() {
        let rucksack = "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL".chars().collect();
        let found_type = find_item_type(rucksack);
        assert_eq!(found_type, 'L');
    }

    #[test]
    fn test_find_item_type_should_be_P() {
        let rucksack = "PmmdzqPrVvPwwTWBwg".chars().collect();
        let found_type = find_item_type(rucksack);
        assert_eq!(found_type, 'P');
    }

    #[test]
    fn test_find_item_type_should_be_v() {
        let rucksack = "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn".chars().collect();
        let found_type = find_item_type(rucksack);
        assert_eq!(found_type, 'v');
    }
    #[test]
    fn test_find_item_type_should_be_t() {
        let rucksack = "ttgJtRGJQctTZtZT".chars().collect();
        let found_type = find_item_type(rucksack);
        assert_eq!(found_type, 't');
    }

    #[test]
    fn test_find_item_type_should_be_s() {
        let rucksack = "CrZsJsPPZsGzwwsLwLmpwMDw".chars().collect();
        let found_type = find_item_type(rucksack);
        assert_eq!(found_type, 's');
    }

    #[test]
    fn test_group_type_should_be_r() {
        let rucksack1 = "vJrwpWtwJgWrhcsFMMfFFhFp".chars().collect();
        let rucksack2: Vec<char> = "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL".chars().collect();
        let rucksack3: Vec<char> = "PmmdzqPrVvPwwTWBwg".chars().collect();
        let found_type = find_group_item(rucksack1, rucksack2, rucksack3);
        assert_eq!(found_type, 'r')
    }

    #[test]
    fn test_group_type_should_be_Z() {
        let rucksack1 = "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn".chars().collect();
        let rucksack2: Vec<char> = "ttgJtRGJQctTZtZT".chars().collect();
        let rucksack3: Vec<char> = "CrZsJsPPZsGzwwsLwLmpwMDw".chars().collect();
        let found_type = find_group_item(rucksack1, rucksack2, rucksack3);
        assert_eq!(found_type, 'Z')
    }
}
