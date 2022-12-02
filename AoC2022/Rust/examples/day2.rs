use aoc_2022::read_lines;

const DAY: &str = "day2.txt";

fn main() {
    part_one();

    part_two();
}


fn part_one() {
    println!("PART I");
    let content = read_lines(DAY).unwrap();
    // A - Rock - X = 1
    // B - Paper - Y = 2
    // C - Sciccor - Z = 3
    // Loss = 0
    // Draw = 3
    // Win = 6
    let mut score = vec![0];
    for line in content {
        if let Ok(current_line) = line {
            let oponent = current_line.chars().nth(0).unwrap();
            let me = current_line.chars().last().unwrap();
            score.push(score_game(oponent, me));
        }
    }
    let res: i32 = score.iter().sum();
    println!("Sum: {}", res);
}

fn score_game(p1: char, p2: char) -> i32 {
    
    let winner = match (p1, p2) { 
        ('A','X') => 1+3,
        ('B','X') => 1+0,
        ('C','X') => 1+6,
        ('A','Y') => 2+6,
        ('B','Y') => 2+3,
        ('C','Y') => 2+0,
        ('A','Z') => 3+0,
        ('B','Z') => 3+6,
        ('C','Z') => 3+3,
        _ => panic!("Something went wrong")
    };
    winner
}

fn get_move(p1: char, plan: char) -> char {
    match (p1, plan) {
        ('A','X') => 'Z',
        ('A','Y') => 'X',
        ('A','Z') => 'Y',
        ('B','X') => 'X',
        ('B','Y') => 'Y',
        ('B','Z') => 'Z',
        ('C','X') => 'Y',
        ('C','Y') => 'Z',
        ('C','Z') => 'X',
        _ => panic!("Something went wrong")
    }
}

fn part_two() {
    println!("PART II");
    let content = read_lines(DAY).unwrap();
    let mut score = vec![0];
    for line in content {
        if let Ok(current_line) = line {
            let oponent = current_line.chars().nth(0).unwrap();
            let plan = current_line.chars().last().unwrap();
            let me = get_move(oponent, plan);
            score.push(score_game(oponent, me));
        }
    }
    let res: i32 = score.iter().sum();
    println!("Sum: {}", res);
}
