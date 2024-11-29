fn main() {
    let file = include_str!("day3.txt");
    let part1_res = part1(file);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

type Grid = Vec<Vec<char>>;
#[derive(Debug)]
struct EngineSchema {
    grid: Grid
}
#[derive(Debug)]
struct SymbolLocation {
    id: usize,
    c: char,
    h: usize,
    w: usize
}
fn part1(_file: &str) -> usize {
    let grid: Grid = _file
        .lines()
        .map(|l| l.chars().collect())
        .collect();

    let mut symbol_location: Vec<SymbolLocation> = vec![];

    for h in 0..grid.len() {
        for w in 0..grid[0].len() {
            // println!("h: {}, w: {} ", h, w);
            // println!("h_idx: {}, w_idx: {} ", self.h_idx[h], self.w_idx[w]);
            if !grid[h][w].is_digit(10) && grid[h][w] != '.' {
                let id = symbol_location.len() + 1;
                let c = grid[h][w];
                symbol_location.push(SymbolLocation { id, c, w, h,})
            }
        }
    }

    let es = EngineSchema { grid };

    let mut numbers: Vec<Number> = vec![];
    for s in symbol_location {
        println!("{:?}", s);
        if let n = es.search_number(s) {
            println!("Found: {:?}",n);
            numbers.push(n);
        }
    }



    // println!("{:?}", symbol_location);
    0
}

impl EngineSchema {
    fn search_number(&self, symbol_location: SymbolLocation) -> Number {
        let h = self.grid.len();
        let w = self.grid[0].len();

        let start_w = symbol_location.w-1;
        let start_h = symbol_location.h-1;

        for i in start_h..start_h+3 {
            for j in start_w..start_w+3 {
                println!("Looking: {},{}",i+1,j+1);
                let val = self.grid[i][j];
                if val.is_digit(10) {
                    println!("{} at ({},{})",val, i+1,j+1);
                }
            }
        }

        Number {
            value: 0,
            length: 0,
            x: 0,
            y: 0,
        }
    }
    // fn parse_number(&self, h: usize, w: usize) -> Number {
    //
    //     let mut numer = vec![];
    //     let mut number_start = (h,w);
    //     let mut number_end = (0,0);
    //
    //     for i in w..self.grid[h].len() {
    //         let value = self.grid[h][i];
    //         if value.is_digit(10) {
    //             numer.push(value);
    //         } else {
    //             break;
    //         }
    //     }
    //
    //     for i in 0..w.rev() {
    //         let value = self.grid[h][i];
    //         if value.is_digit(10) {
    //             numer.push(value);
    //         } else {
    //             break;
    //         }
    //     }
    //
    //
    //
    //     Number {
    //         value: 0,
    //         length: 0,
    //         x: 0,
    //         y: 0,
    //     }
    // }
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

#[derive(Debug)]
struct Number {
    value: usize,
    length: usize,
    x: usize,
    y: usize,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part1_input() {
        let input =
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";
        let res = part1(input);
        assert_eq!(res, 4361)
    }

    #[test]
    fn part2_input() {}

    #[test]
    fn part2_lineinput() {}
}
