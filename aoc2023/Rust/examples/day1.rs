fn main() {
    let file = include_str!("day1.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(file: &str) -> u32 {
    let numbers: Vec<u32> = file
        .lines()
        .map(|line| part1_linesolve(line))
        .collect();

    numbers.iter().sum()
}

fn part2(file: &str) -> usize {
    let numbers: Vec<usize> = file.lines()
        .map(|line| part2_linesolve(line))
        .collect();

    numbers.iter().sum()
}

fn part1_linesolve(line: &str) -> u32 {
    let digits: Vec<char> = line
        .chars()
        .filter(|x| x.is_digit(10))
        .collect();

    let first = digits.first().unwrap();
    let last = digits.last().unwrap();
    let result = format!("{first}{last}");
    result.parse::<u32>().unwrap()
}

fn part2_linesolve(line: &str) -> usize {
    let pattern = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4",
        "5", "6", "7", "8", "9",
    ];
    let mut positions: Vec<(bool, usize, &str)> = pattern
        .iter()
        .map(|pattern| find_pattern(line, pattern))
        .flatten()
        .filter(|(map, _, _)| map == &true)
        .collect();

    positions.sort();

    let first = str_to_number(positions.first().unwrap().2);
    let last = str_to_number(positions.last().unwrap().2);
    let result = format!("{first}{last}");
    result.parse::<usize>().unwrap()
}

// find stops at first true, we need to find all true in a line
fn find_pattern<'a>(line: &str, pattern: &'a str) -> Vec<(bool, usize, &'a str)> {
    let mut postitions: Vec<(bool, usize, &'a str)> = vec![];

    if let Some(position) = line.find(&pattern) {
        postitions.push((true, position, pattern));
    }
    if let Some(position) = line.rfind(&pattern) {
        postitions.push((true, position, pattern));
    }

    postitions
}

fn str_to_number(n: &str) -> &str {
    match n {
        "one" | "1" => "1",
        "two" | "2" => "2",
        "three" | "3" => "3",
        "four" | "4" => "4",
        "five" | "5" => "5",
        "six" | "6" => "6",
        "seven" | "7" => "7",
        "eight" | "8" => "8",
        "nine" | "9" => "9",
        _ => "0",
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
        // let number = part2_linesolve("two1nine");
        // assert_eq!(number, 29);
        //
        // let number = part2_linesolve("sevenine");
        // assert_eq!(number, 79);
        //
        // let number = part2_linesolve("eighthree");
        // assert_eq!(number, 83);
        let number = part2_linesolve("'fournr8ltltldqsmcd5threetwothree");
        assert_eq!(number, 43);
    }
}
