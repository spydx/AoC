fn main() {
    let file = include_str!("day2.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

fn part1(file: &str) -> usize {
    let mapped: Vec<Game> = file.lines()
        .map(part1_linesolve)
        .filter(|g| g.possible)
        .collect();

    let result = mapped.iter()
        .map(|g| g.id)
        .sum();

    result
}

#[derive(Debug)]
struct Game {
    id: usize,
    possible: bool,
}

#[derive(Debug)]
struct GameMinSize {
    red: usize,
    blue: usize,
    green: usize,
}

impl GameMinSize {
    fn power(&self) -> usize {
        self.red * self.blue * self.green
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct Cube {
    color: CubeColor,
    count: usize,
}

#[derive(Debug)]
enum CubeColor {
    Red,
    Blue,
    Green,
}

fn part1_linesolve(line: &str) -> Game {
    let splitstring: Vec<&str> = line.split(':').collect();

    let gameid: Vec<&str> = splitstring.first().unwrap().split(" ").collect();
    let gameid = gameid.last().unwrap().parse::<usize>().unwrap();

    let shown_cubes = find_cubes(splitstring.last().unwrap());

    let possible = shown_cubes.iter()
        .all(|(red, blue, green)| green.count <= 13 && blue.count <= 14 && red.count <= 12);

    Game {
        id: gameid,
        possible,
    }
}

fn find_cubes(remainder: &str) -> Vec<(Cube, Cube, Cube)> {
    let reveals: Vec<&str> = remainder.split([';']).collect();

    let mut shown_cubes: Vec<(Cube, Cube, Cube)> = vec![];

    for r in reveals {
        let string_cubes: Vec<&str> = r.split([',']).collect();

        let mut r = Cube { color: CubeColor::Red, count: 0 };
        let mut b = Cube { color: CubeColor::Blue, count: 0 };
        let mut g = Cube { color: CubeColor::Green, count: 0 };

        for string_cube in string_cubes {
            let mut cubedata: Vec<&str> = string_cube.split(" ").collect();
            cubedata.remove(0);
            let count = cubedata.first().unwrap().parse::<usize>().unwrap();

            match cubedata.last().unwrap() {
                &"red" => r.count = count,
                &"blue" => b.count = count,
                &"green" => g.count = count,
                _ => panic!("Unknown color")
            };
        }
        shown_cubes.push((r, b, g));
    }

    shown_cubes
}


fn part2(file: &str) -> usize {
    let res: usize = file.lines()
        .map(part2_linesolve)
        .map(|g| g.power())
        .into_iter()
        .sum();
    res
}


fn part2_linesolve(line: &str) -> GameMinSize {
    let splitstring: Vec<&str> = line.split(':').collect();

    let shown_cubes = find_cubes(splitstring.last().unwrap());

    let mut r = 0;
    let mut b = 0;
    let mut g = 0;

    for (red, blue, green) in shown_cubes {
        if red.count > r {
            r = red.count
        }
        if blue.count > b {
            b = blue.count;
        }

        if green.count > g {
            g = green.count;
        };
    }

    GameMinSize {
        red: r,
        blue: b,
        green: g,
    }
}

#[cfg(test)]
mod test {
    use super::*;


    // Only game 1, 2, 5 are good, sum game ids is 8.
    #[test]
    fn part1_input() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
        let number = part1(input);
        assert_eq!(number, 8)
    }

    #[test]
    fn part1_lineinput() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        let game = part1_linesolve(input);
        assert_eq!(game.id, 1);
        assert_eq!(game.possible, true);
    }

    #[test]
    fn part1_lineinput_falsy() {
        let input = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
        let game = part1_linesolve(input);
        assert_eq!(game.id, 3);
        assert_eq!(game.possible, false);
    }

    #[test]
    fn part2_input() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
        let number = part2(input);
        assert_eq!(number, 2286)
    }

    #[test]
    fn part2_lineinput() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        let game = part2_linesolve(input);
        assert_eq!(game.red, 4);
        assert_eq!(game.blue, 6);
        assert_eq!(game.green, 2);
    }
}