fn main() {
    let file = include_str!("dayX.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(_file: &str) -> usize {
    0
}

fn part1_linesolve(_line: &str) -> usize {
    0
}

fn part2(_file: &str) -> usize {
    0
}

fn part2_linesolve(_line: &str) -> usize {
    0
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part1_input() {}

    #[test]
    fn part1_lineinput() {}

    #[test]
    fn part1_lineinput_falsy() {}

    #[test]
    fn part2_input() {}

    #[test]
    fn part2_lineinput() {}
}
