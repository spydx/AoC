use std::fmt::format;

fn main() {
    let file = include_str!("day1.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2 :{}", part2_res);
}

fn part1(file: &str) -> u32 {
    let numbers: Vec<u32> = file.lines()
        .map(|line| part1_linesolve(line))
        .collect();

    numbers.iter().sum()
}

fn part2(file: &str) -> u32 {
    let numbers: Vec<u32> = file.lines()
        .map(|line| part2_linesolve(line))
        .collect();

    numbers.iter().sum()
}

fn part1_linesolve(line: &str) -> u32 {
    let digits: Vec<char> = line.chars()
        .filter(|x| x.is_digit(10))
        .collect();

    let first = digits.first().unwrap();
    let last = digits.last().unwrap();
    let result = format!("{first}{last}");
    result.parse::<u32>().unwrap()
}

fn part2_linesolve(line: &str) -> u32 {

    let mut digits:Vec<&str> = vec![];

    for c in line.chars() {
        if c.is_alphabetic() {
            let word: &[char] = line.chars()
                .take_while(|c| c.is_alphabetic())
                .into_iter()
                .collect();

            digits.push(word.to_string().as_str());
        } else if c.is_digit(10) {
            let n = c.to_string().as_str();
            digits.push(n);
        }
    }

    let first = digits.first().unwrap();
    let last = digits.last().unwrap();
    let result = format!("{}{}", first, last);
    result.parse::<u32>().unwrap()

//     let strs: Vec<&str> = line
//         .split(&['0','1','2','3','4','5','6','7','8','9'])
//         .collect();
//
//     let numbers: Vec<&str> = strs
//         .into_iter()
//         .map(|n| str_to_number)
//         .collect();
//
//     let first = numbers.first();
//     let last = numbers.last();
//     let result = format!("{first}{last}");
//     result.parse::<u32>().unwrap()
}

fn str_to_number(n: &str) -> &str {
    match n {
        "zero"|"0" => "0",
        "one"|"1" => "1",
        "two"|"2" => "2",
        "tree"|"3" => "3",
        "four"|"4" => "4",
        "five"|"5" => "5",
        "six"|"6" => "6",
        "seven"|"7" => "7",
        "eight"|"8" => "8",
        "nine"|"9" => "9",
        _ => "",
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part1_example() {
        let file = "1abc2
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet";

        let result_part1 = part1(file);
        assert_eq!(result_part1, 142);
    }

    #[test]
    fn part1_linetest() {
        let number = part1_linesolve("1abc2");
        assert_eq!(number, 12);
    }

    #[test]
    fn part2_example() {
        let file = "two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
";
        let result_part2 = part2(file);
        assert_eq!(result_part2, 281);
    }


    #[test]
    fn part2_linetest() {
        let number = part2_linesolve("two1nine");
        assert_eq!(number, 29);
    }

    #[test]
    fn test() {
        let line = "two1nine";
        let collection: Vec<&str> = line.split_inclusive(&['1','2']).collect();
        assert_eq!(["two", "1", "nine"].to_vec(), collection);
    }
}