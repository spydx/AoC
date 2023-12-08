fn main() {
    let file = include_str!("day6.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(_file: &str) -> usize {
    let data: Vec<Vec<&str>> = _file.lines().map(part1_lineparse)
        .collect();

    let datastructure = data[0].iter().zip(data[1].iter());

    let mut results: Vec<usize> = vec![];

    for (i, (t, d)) in datastructure.enumerate() {
        // println!("{} : (t: {}, d: {})", i, t, d);
        if i == 0 {
            continue; // first part is a label
        }

        let time = t.parse::<usize>().unwrap();
        let distance = d.parse::<usize>().unwrap();
        let no_ways = way_to_win(time, distance);
        // println!("{}", no_ways);
        results.push(no_ways);
    }
    // println!("{:?}", results);

    if results.len() == 0 {
        return 0
    }
    let mut resvalue = 1_usize;

    for val in results.iter() {
        resvalue *= val;
    }

    resvalue

}

fn part1_lineparse(_line: &str) -> Vec<&str> {
    _line.split(' ')
        .filter(|x| !x.is_empty())
        .collect()
}

fn way_to_win(time: usize, distance: usize) -> usize {
    let mut ways: Vec<String> = vec![];
    let times: Vec<usize> = (0..time).collect();
    for speed in times {

        let remaining = time - speed;

        let calculated_distance = remaining * speed;
        if calculated_distance > distance {
            let r = format!("T: {}; I:{}; R:{}; D: {}", time,  speed, remaining, calculated_distance);
            ways.push(r)
        }
    }

    // println!("{:?}", ways);

    ways.iter().map(|_| 1).sum()
}

fn part2(_file: &str) -> usize {
    let data: Vec<Vec<&str>> = _file.lines()
        .map(part1_lineparse)
        .collect();

    // println!("{:?}", data[0]);
    let time_binding = data[0][1..].concat();
    let time = time_binding.as_str();
    let dest_biding = data[1][1..].concat();
    let destination = dest_biding.as_str();

    let time = time.parse::<usize>().unwrap();
    let destination = destination.parse::<usize>().unwrap();

    let res = way_to_win(time, destination);

    res
}


#[cfg(test)]
mod test {
    use rstest::rstest;
    use super::*;

    #[test]
    fn part1_input() {
        let input = "Time:      7  15   30
        Distance:  9  40  200";
        let res = part1(input);
        assert_eq!(res, 288);
    }

    #[rstest]
    #[case(7, 9, 4)]
    fn distance(#[case] t: usize, #[case] d: usize, #[case] expected: usize) {
        let distance = way_to_win(t, d);
        assert_eq!(distance, expected);
    }

    #[test]
    fn part2_input() {
        let input = "Time:      7  15   30
        Distance:  9  40  200";
        let res = part2(input);
        assert_eq!(res, 71503);
    }

}
