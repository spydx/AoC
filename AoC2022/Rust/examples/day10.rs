use aoc_2022::{read_lines, FileBuffer};
use std::{time::Instant, collections::{HashMap, btree_map::Range}};

const DAY: &str = "day10.txt";

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
    let map = solve_part_one(content);
    println!("{:?}", map);
    let res: i32 = map.into_iter().map(|(k,v)| k * v).sum();
    println!("Total sum: {}", res);

}

fn solve_part_one (content: FileBuffer) -> HashMap<i32, i32> {
    let mut cycle = 0;
    let mut signal = 1;
    let mut cyclemap = HashMap::<i32, i32>::new();
    let cycles : Vec<i32> = vec![20, 60, 100, 140, 180, 220];

    content.for_each(|l|  {
        let line = l.unwrap();
        // println!("{}", line);
        // println!("Cycle: {}, Signal: {}", cycle, signal);
        let (cmd, val) = parse_line(line);
        if cmd.eq("noop") {
            // println!("noop");
            cycle += 1;
            if cycles.contains(&cycle) {
                cyclemap.insert(cycle, signal);
            }
        } else {
            for _i in 0..2 {
                //println!("{}", _i);
                cycle += 1;
                if cycles.contains(&cycle) {
                    cyclemap.insert(cycle, signal);
                }
            }
            signal += val;    
        }
        // println!("Cycle: {}, Signal: {}", cycle, signal);
    });

    cyclemap
}


fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();
    solve_part_two(content);
}



fn solve_part_two(content: FileBuffer) {
    let mut cycle = 0;
    let mut sprite = 1;
    
    content.for_each(|l| {
        let line = l.unwrap();
        let (cmd, val) = parse_line(line);
        if cmd.starts_with("noop") {
            cycle +=1;
            print(cycle, sprite);
            
        } else {
            for _i in 0..2 {
                cycle += 1;
                print(cycle, sprite)
            }
            sprite += val;
        }
    })

}

fn print(cycle: i32, sprite: i32) {
    let pos = cycle % 40;

    if pos == sprite || pos == sprite + 1 || pos == sprite + 2 {
        print!("#")
    } else {
        print!(".")
    }
    if pos == 0 {
        println!()
    }
}

fn parse_line(line: String) -> (String, i32) {
    let temp: Vec<_> = line.split_ascii_whitespace().collect();
    if temp[0] == "noop" {
        return (temp[0].to_string(), 0)
    }
    let value = temp[1].parse::<i32>().unwrap();
    (temp[0].to_string(), value)
}


#[cfg(test)]
mod test {
    use crate::parse_line;
    use crate::solve_part_one;
    use crate::solve_part_two;
    use crate::read_lines;

    #[test]
    fn test_solve_example() {
        let content = read_lines("day10_example.txt").unwrap();
        let map = solve_part_one(content);

        println!("{:?}", map);
        assert_eq!(map.get(&20).unwrap(), &21);
        assert_eq!(map.get(&60).unwrap(), &19);
        assert_eq!(map.get(&100).unwrap(), &18);
        assert_eq!(map.get(&140).unwrap(), &21);
        assert_eq!(map.get(&180).unwrap(), &16);
        assert_eq!(map.get(&220).unwrap(), &18)
    }

    #[test]
    fn test_solve_two() {
        let content = read_lines("day10_example.txt").unwrap();
        solve_part_two(content);
    }


    #[test]
    fn test_parse_noop() {
        let line: String = String::from("noop");
        let (cmd, val) = parse_line(line);
        assert_eq!(cmd, "noop");
        assert_eq!(val, 0);
    }

    #[test]
    fn test_parse_addx() {
        let line: String = String::from("addx -5");
        let (cmd, val) = parse_line(line);
        assert_eq!(cmd, "addx");
        assert_eq!(val, -5);
    }

}

