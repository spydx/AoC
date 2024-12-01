fn main() {
    let file = include_str!("day1.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(file: &str) -> usize {
    let mut lhs = vec![];
    let mut rhs = vec![];
    for line in file.lines() {
        let (left, right) = part1_line_solve(&line);
        lhs.push(left);
        rhs.push(right);
    }

    lhs.sort();
    rhs.sort();

    let mut res: Vec<usize> = vec![];
    if lhs.len() != rhs.len() {
        panic!("Not the same length");
    }

    for i in 0..lhs.len() {
        let l = lhs[i];
        let r = rhs[i];
        let distance = l - r;
        res.push(distance.abs() as usize);
    }

    res.iter()
        .sum()
}

fn part1_line_solve(_line: &str) -> (i32, i32) {
    let parts = _line.split_ascii_whitespace()
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    (parts[0], parts[1])
}

fn part2(file: &str) -> i32 {
    let mut lhs = vec![];
    let mut rhs = vec![];
    for line in file.lines() {
        let (left, right) = part1_line_solve(&line);
        lhs.push(left);
        rhs.push(right);
    }

    let mut res: Vec<i32> = vec![];
    for i in 0..lhs.len() {
        let l = lhs[i];
        let matches: Vec<&i32>= rhs.iter()
            .filter(|&r| r == &l)
            .collect();

        let similarityscore = l * matches.len() as i32;
        res.push(similarityscore)

    }

    res.iter()
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part1_input() {
        let input = "3   4
4   3
2   5
1   3
3   9
3   3";
       let res = part1(input);
        assert_eq!(res, 11);
    }

    #[test]
    fn part1_line_input() {}

    #[test]
    fn part2_input() {
        let input = "3   4
4   3
2   5
1   3
3   9
3   3";
        let res = part2(input);
        assert_eq!(res, 31);
    }

    #[test]
    fn part2_line_input() {}
}
