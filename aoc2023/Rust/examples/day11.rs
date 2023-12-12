fn main() {
    let file = include_str!("day11.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    assert_eq!(part1_res, 9609130);
    let part2_res = part2(file, 1_000_000);
    println!("Part2: {}", part2_res);
}

fn part1(_file: &str) -> usize {
    let n = 1;
    let universe = expand_to_universe(_file, n);
    // println!("{:?}", &universe.galaxies);
    let distances = universe.calculate_galaxy_distances();

    distances.iter().sum()
}

fn part2(_file: &str, n: usize) -> usize {
    let universe = expand_to_universe(_file, n);

    // print_universe(&universe.grid);
    let distances = universe.calculate_galaxy_distances();

    distances.iter().sum()
}

#[derive(Debug)]
struct Universe {
    grid: UniverseGrid,
    h_idx: Vec<usize>,
    w_idx: Vec<usize>,
    galaxies: Vec<Galaxy>,
}

type UniverseGrid = Vec<Vec<char>>;

#[allow(dead_code)]
fn print_universe(u: &UniverseGrid) {
    for h in 0..u.len() {
        print!("|");
        for w in 0..u[h].len() {
            print!(" {} ", u[h][w])
        }
        print!("|\n");
    }
}

impl Universe {
    fn create_map(grid: &str, n: usize) -> Universe {
        let mut universe: UniverseGrid = vec![];
        let mut height: Vec<usize> = vec![];

        let mut height_idx = 0;

        for l in grid.lines() {
            let charvec: Vec<char> = l.chars().collect();

            if l.contains('#') {
                height_idx += 1;
            } else {
                if n == 1 {
                    height_idx += n + 1;
                } else {
                    height_idx += n;
                }
            }
            universe.push(charvec);
            height.push(height_idx);
        }

        let mut width: Vec<usize> = vec![];

        let mut width_idx = 0;

        let width_vec_length = universe.first().unwrap().len();

        for w in 0..width_vec_length {
            let mut expand = true;
            for h in 0..universe.len() {
                if universe[h][w] == '#' {
                    expand = false;
                }
            }
            if expand == true {
                if n == 1 {
                    width_idx += n + 1;
                } else {
                    width_idx += n;
                }
            } else {
                width_idx += 1;
            }
            width.push(width_idx);
        }

        Universe {
            grid: universe,
            h_idx: height,
            w_idx: width,
            galaxies: vec![],
        }
    }

    fn find_galaxies(&self) -> Vec<Galaxy> {
        let mut galaxies: Vec<Galaxy> = vec![];

        for h in 0..self.h_idx.len() {
            for w in 0..self.w_idx.len() {
                // println!("h: {}, w: {} ", h, w);
                // println!("h_idx: {}, w_idx: {} ", self.h_idx[h], self.w_idx[w]);
                if self.grid[h][w] == '#' {
                    let id = galaxies.len() + 1;
                    let height = self.h_idx[h];
                    let width = self.w_idx[w];
                    galaxies.push(Galaxy { id, w: width, h: height })
                }
            }
        }

        galaxies
    }

    fn calculate_galaxy_distances(&self) -> Vec<usize> {
        let mut dis = vec![0_usize];

        for source in self.galaxies.iter() {
            for target in self.galaxies.iter().rev() {
                if source == target {
                    break;
                }
                let d = shortest_path(source, target);
                dis.push(d);
            }
        }

        dis
    }
}

#[derive(Debug, PartialEq)]
struct Galaxy {
    id: usize,
    h: usize,
    w: usize,
}

fn expand_to_universe(_map: &str, n: usize) -> Universe {
    println!("Universe: BANG BANG!");
    let mut universe = Universe::create_map(_map, n);
    println!("And so it was, created");
    println!("But the universe traveled some time and expanded");
    println!("Some brave men went in search for planets..");
    let galaxies = universe.find_galaxies();
    println!("They found a few galaxies: {}", galaxies.len());
    universe.galaxies = galaxies;
    universe
}

fn shortest_path(source: &Galaxy, target: &Galaxy) -> usize {
    // println!("Source: {:?}", source);
    // println!("Target: {:?}", target);

    let x = (source.w as i32 - target.w as i32).abs();
    let y = (source.h as i32 - target.h as i32).abs();
    let res = (x + y) as usize;

    // println!("x: {}, y: {}, distance: {}", x, y, res);
    res
}

#[cfg(test)]
mod test {
    use rstest::rstest;
    use super::*;

    #[test]
    fn part1_input() {
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let res = part1(input);
        assert_eq!(res, 374)
    }

    #[test]
    // #[rstest]
    // #[case(10, 1030)]
    // #[case(100, 8410)]
    // fn part2_input(#[case]n: usize, #[case] expected: usize) {
    fn part2_10x() {
        let n = 10;
        let expected = 1030;
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let res = part2(input, n);
        assert_eq!(res, expected)
    }

    #[test]
    fn test_expand_map() {
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let universe = expand_to_universe(input, 1);
        println!("h: {:?}", universe.h_idx);
        println!("w: {:?}", &universe.w_idx);
        assert_eq!(universe.w_idx[2], 4);
        assert_eq!(universe.h_idx[3], 5);
        assert_eq!(universe.galaxies.len(), 9);
    }

    #[rstest]
    #[case(5, 9, 9)]
    #[case(1, 7, 15)]
    #[case(3, 6, 17)]
    #[case(8, 9, 5)]
    fn distance_between(#[case]source_id: usize, #[case]target_id: usize, #[case] expected_distance: usize) {
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let universe = expand_to_universe(input, 1);
        print_universe(&universe.grid);
        let source: &Galaxy = universe.galaxies.iter().find(|g| g.id == source_id).unwrap();
        let target: &Galaxy = universe.galaxies.iter().find(|g| g.id == target_id).unwrap();
        let calculated_distance = shortest_path(source, target);
        assert_eq!(calculated_distance, expected_distance)
    }
}
