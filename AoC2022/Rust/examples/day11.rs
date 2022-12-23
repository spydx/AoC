use aoc_2022::read_lines;
use std::time::Instant;

const DAY: &str = "day11.txt";
const ROUNDS: i32 = 20;

struct Monkey {
    id: i32,
    items: Vec<i32>,
    inspections: i32,
}
struct Game {
    rounds: i32,
    players: Vec<Monkey>
}

impl Game {
    fn new(rounds: i32) -> Game {
        Game {
            rounds: rounds,
            players: Vec::<Monkey>::new(),
        }
    }
    fn add_player(&mut self, monkey: Monkey) {
        self.players.push(monkey);
    }
}

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
    todo!()
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();
    todo!()
}



#[cfg(test)]
mod test {
    use crate::*;


    #[test]
    fn parse_a_monkey() {
        let content = read_lines("day11_example.txt").unwrap();
        let monkey: Vec<String> = content.take(6).map(|l| l.unwrap()).collect();
        println!("{:?}", monkey);
    }
    #[test]
    fn parse_input_monkeys() {
        let content = read_lines("day11_example.txt").unwrap();
        let mut game = Game::new(ROUNDS);
        


    }
}
