use aoc_2022::{read_lines, FileBuffer};
use std::{time::Instant, collections::HashMap};

const DAY: &str = "day7.txt";
const FOLDER_SIZE_100K: i32 = 100000;

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

    let res = solve_part_one(content);
    println!("Total folder sizes: {}" , res);
}

fn solve_part_one(content: FileBuffer) -> i32 {

    let mut folder_size = 0;
    let mut path: Vec<String> = vec![];
    let mut folder_map = HashMap::<String,i32>::new();

    for line in content {
        if let Ok(current_line) = line {
            let (cmd, val) = parse_command(&current_line);
            match (cmd, val) {
                ("cd", "..") => {
                    let current_folder = path.pop().unwrap();
                    let entering_folder = path.pop().unwrap();
                    println!("Exiting {}", current_folder);
                    println!("Entering {}", entering_folder);
                    
                    let value = folder_map.get(&entering_folder.to_string());
                    folder_map.insert(entering_folder.to_string(), value.unwrap() + folder_size);
                    path.push(entering_folder);
                },
                ("cd", dir) => {
                    println!("Entering folder: {}", dir);
                    path.push(dir.to_string());
                    folder_map.insert(dir.to_string(), folder_size);
                    folder_size = 0;
                },
                ("size", val) => {
                    println!("Size: {}" , val);
                    let size = val.parse::<i32>().unwrap();
                    folder_size += size;
                },
                ("ls", _) => {

                    //set counter 
                },
                _ => panic!("something went wrong!")
            }
        }
        println!("Path: {:?}", path);
       
    }
    println!("{:?}", folder_map);
    0
}

fn parse_command(line: &str) -> (&str, &str) {
    let temp: Vec<_> = line.split_ascii_whitespace().collect();
    let (cmd, val) = match temp[..] {
        ["$", "cd", ".."] => ("cd", ".."),
        ["$", "cd", dir] => ("cd", dir),
        ["$", "ls"] => ("ls", ""),
        [val, _] => {
            let val = if val.starts_with("dir") {
                "0"
            } else {
                val
            };
            ("size", val)
        },  
        _ => panic!("Ups, wrong input"),
    };

    (cmd, val)
    
}

fn part_two() {
    println!("PART II");
    let _content = read_lines(DAY).unwrap();
    todo!()
}

#[cfg(test)]
mod test {
    use crate::*;

    const INPUT: &str = 
r#"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"#;

    #[test]
    fn take_input() {
        let mut count = 0;
        for (i, _line )in INPUT.lines().enumerate() {
            count = i;
        }
        assert_eq!(count,22);
    }

    #[test]
    fn at_most_100_000_size() {
        let content = read_lines("day7_example.txt").unwrap();

        let size = solve_part_one(content);
        assert_eq!(size, 95437);
    }

    #[test]
    fn parse_command_dir() {
        let line_input = "$ cd /";
        let (cmd, dir) = parse_command(line_input);

        assert_eq!(cmd, "cd");
        assert_eq!(dir, "/")
    }

    #[test]
    fn parse_command_cd() {
        let line_input = "$ cd ..";
        let (cmd, dir) = parse_command(line_input);

        assert_eq!(cmd, "cd");
        assert_eq!(dir, "")
    }


    #[test]
    fn parse_command_ls() {
        let line_input = "$ ls";
        let (cmd, dir) = parse_command(line_input);

        assert_eq!(cmd, "ls");
        assert_eq!(dir, "")
    }

    #[test]
    fn parse_command_size() {
        let line_input = "14848514 b.txt";
        let (cmd, number) = parse_command(line_input);
        let res = number.parse::<i32>().unwrap();

        assert_eq!(cmd, "size");
        assert_eq!(res, 14848514)
    }

}
