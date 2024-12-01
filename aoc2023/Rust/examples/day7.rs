use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hash;
use HandResult::{HighCard, OnePair, ThreeOfKind, FiveOfKind, FourOfKind, FullHouse, TwoPair};
use Card::{Joker, Two, Three, Four, Five, Ten, Queen, King, Ace, Six, Seven, Eight, Nine, Jack};
use crate::HandResult::Invalid;

fn main() {
    let file = include_str!("day7.txt");
    let part1_res = part1(file);
    assert_eq!(part1_res, 253910319);
    println!("Part1: {}", part1_res);
    let part2_res = part2(file);
    println!("Part2: {}", part2_res);
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Eq, Ord, Hash)]
enum Card {
    Zero,
    Joker,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
enum HandResult {
    Invalid(),
    HighCard(Card, Card, Card, Card, Card),
    OnePair(Card, Card, Card, Card, Card),
    TwoPair(Card, Card, Card, Card, Card),
    ThreeOfKind(Card, Card, Card, Card, Card),
    FullHouse(Card, Card, Card, Card, Card),
    FourOfKind(Card, Card, Card, Card, Card),
    FiveOfKind(Card, Card, Card, Card, Card),
}


impl TryInto<Vec<Card>> for &HandResult {
    type Error = ();

    fn try_into(self) -> Result<Vec<Card>, Self::Error> {
        let res = match self {
            Invalid() => { None }
            HighCard(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
            OnePair(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
            TwoPair(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
            ThreeOfKind(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
            FullHouse(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
            FourOfKind(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
            FiveOfKind(c1, c2, c3, c4, c5) => { Some(vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]) }
        };
        if res.is_none() {
            Err(())
        } else {
            Ok(res.unwrap())
        }
    }
}

impl Ord for HandResult {
    fn cmp(&self, other: &Self) -> Ordering {
        let hand: Vec<Card> = self.try_into().unwrap();

        let other: Vec<Card> = other.try_into().unwrap();
        for i in 0..5 {
            if hand[i] < other[i] {
                return Ordering::Less;
            } else if hand[i] > other[i] {
                return Ordering::Greater;
            }
        }
        Ordering::Equal
    }
}

trait ResultingHand {
    fn to_result(self) -> HandResult;
    fn to_result2(self) -> HandResult;
}

impl ResultingHand for Vec<Card> {
    fn to_result(self) -> HandResult {
        let mut map: HashMap<Card, usize> = HashMap::new();

        for card in &self {
            match map.get(&card) {
                Some(v) => map.insert(card.clone(), v + 1),
                None => map.insert(card.clone(), 1),
            };
        }

        let mut handresult = Invalid();

        if map.len() == 1 {
            handresult = FiveOfKind(self[0], self[1], self[2], self[3], self[4])
        } else if map.len() == 2 {
            // four of a kind
            // full house

            let mut four = Card::Zero;
            // println!("{:?}", &map);
            for (card_key, value) in map {
                if value == 4 {
                    four = card_key;
                }
            }

            handresult = if four != Card::Zero {
                FourOfKind(self[0], self[1], self[2], self[3], self[4])
            } else {
                FullHouse(self[0], self[1], self[2], self[3], self[4])
            };
        } else if map.len() == 3 {
            // three of a kind
            // two pairs

            let mut one = vec![];
            let mut two = vec![];
            let mut three = vec![];

            for (card_key, value) in map {
                match value {
                    1 => one.push(card_key),
                    2 => two.push(card_key),
                    3 => three.push(card_key),
                    _ => panic!("not possible"),
                }
            }

            handresult = if three.len() == 1 {
                ThreeOfKind(self[0], self[1], self[2], self[3], self[4])
            } else {
                TwoPair(self[0], self[1], self[2], self[3], self[4])
            };
        } else if map.len() == 4 {
            // one pair
            handresult = OnePair(self[0], self[1], self[2], self[3], self[4])
        } else if map.len() == 5 {
            // high card
            handresult = HighCard(self[0], self[1], self[2], self[3], self[4])
        }

        handresult
    }

    fn to_result2(self) -> HandResult {
        let joker_count = if self.contains(&Joker) {
            self.iter()
                .filter(|c| c == &&Joker)
                .map(|_| 1)
                .sum()
        } else {
            0
        };

        let mut map: HashMap<Card, usize> = HashMap::new();

        for card in &self {
            match map.get(&card) {
                Some(v) => map.insert(card.clone(), v + 1),
                None => map.insert(card.clone(), 1),
            };
        }

        let mut handresult = Invalid();

        if map.len() == 1 {
            handresult = FiveOfKind(self[0], self[1], self[2], self[3], self[4])
        } else if map.len() == 2 {
            // four of a kind
            // full house
            let mut four = Card::Zero;
            // println!("{:?}", &map);
            for (card_key, value) in map {
                if value == 4 {
                    four = card_key;
                }
            }
            handresult = match joker_count {
                0 => {
                    if four != Card::Zero {
                        FourOfKind(self[0], self[1], self[2], self[3], self[4])
                    } else {
                        FullHouse(self[0], self[1], self[2], self[3], self[4])
                    }
                }
                _ => FiveOfKind(self[0], self[1], self[2], self[3], self[4]),
            }
        } else if map.len() == 3 {
            // three of a kind
            // two pairs

            let mut no_cards = 0;
            for (_, v) in map {
                if v > no_cards {
                    no_cards = v;
                }
            }

            handresult = match joker_count {
                2 | 3 => FourOfKind(self[0], self[1], self[2], self[3], self[4]),
                1  => {
                    if no_cards == 2 {
                        FullHouse(self[0], self[1], self[2], self[3], self[4])
                    } else {
                        FourOfKind(self[0], self[1], self[2], self[3], self[4])
                    }
                }
                0 => TwoPair(self[0], self[1], self[2], self[3], self[4]),
                _ => panic!("To many cards in for 3 length"),
            };
        } else if map.len() == 4 {
            handresult = match joker_count {
                1 | 2 => ThreeOfKind(self[0], self[1], self[2], self[3], self[4]),
                0 => OnePair(self[0], self[1], self[2], self[3], self[4]),
                _ => panic!("To many cards in for 4 length"),
            };
        } else if map.len() == 5 {
            // high card

            handresult = match joker_count {
                1 => OnePair(self[0], self[1], self[2], self[3], self[4]),
                _ => HighCard(self[0], self[1], self[2], self[3], self[4]),
            };
        }

        handresult
    }
}

fn str_to_hand(hand: &str, partone: bool) -> Vec<Card> {
    let mut cards: Vec<Card> = vec![];

    for c in hand.chars() {
        let card = match c {
            '2' => Two,
            '3' => Three,
            '4' => Four,
            '5' => Five,
            '6' => Six,
            '7' => Seven,
            '8' => Eight,
            '9' => Nine,
            'T' => Ten,
            'J' => {
                if partone {
                    Jack
                } else {
                    Joker
                }
            }
            'Q' => Queen,
            'K' => King,
            'A' => Ace,
            _ => panic!("What card is this?")
        };
        cards.push(card);
    }

    cards
}

fn part1(_file: &str) -> usize {
    let mut hands_and_bets: Vec<(HandResult, usize)> = _file
        .lines()
        .map(part1_linesolve)
        .collect();

    hands_and_bets.sort();
    // println!("{:?}", hands_and_bets);
    let mut sum = 0;
    for i in 1..hands_and_bets.len() + 1 {
        let value = hands_and_bets[i - 1].1;
        // println!("+ {}*{} ", i, value);
        sum += i * value;
    }
    sum
}


fn part1_linesolve(_line: &str) -> (HandResult, usize) {
    let parts: Vec<&str> = _line
        .split(' ')
        .collect();

    let cards: &str = parts.first().unwrap();
    let hand = str_to_hand(cards, true).to_result();

    let bet = parts.last().unwrap()
        .parse::<usize>().unwrap();

    (hand, bet)
}

fn part2(_file: &str) -> usize {
    let mut hands_and_bets: Vec<(HandResult, usize)> = _file
        .lines()
        .map(part2_linesolve)
        .collect();
    println!("{:?}", hands_and_bets);
    hands_and_bets.sort();
    for hand in &hands_and_bets {
        println!("{:?}", hand.0);
    }
    // println!("{:?}", hands_and_bets);
    let mut sum = 0;
    for i in 1..hands_and_bets.len() + 1 {
        let value = hands_and_bets[i - 1].1;
        // println!("+ {}*{} ", i, value);
        sum += i * value;
    }
    sum
}

fn part2_linesolve(_line: &str) -> (HandResult, usize) {
    let parts: Vec<&str> = _line
        .split(' ')
        .collect();

    let cards: &str = parts.first().unwrap();
    let hand = str_to_hand(cards, false).to_result2();

    let bet = parts.last().unwrap()
        .parse::<usize>().unwrap();

    (hand, bet)
}

#[cfg(test)]
mod test {
    use rstest::rstest;
    use super::*;

    #[test]
    fn part1_input() {
        let input = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";
        let res = part1(input);
        assert_eq!(res, 6440)
    }

    #[test]
    fn part1_input_sort() {
        let input = "33332 10
2AAAA 2
77888 1
77788 3";
        let res = part1(input);
        assert_eq!(res, 51)
    }


    #[test]
    fn part2_input_sort() {
        let input = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";
        let res = part2(input);
        assert_eq!(res, 5905)
    }

    #[rstest]
    #[case("32T3K 765", OnePair(Three, Two, Ten, Three, King), 765_usize)]
    #[case("T55J5 684", ThreeOfKind(Ten, Five, Five, Jack, Five), 684_usize)]
    fn part1_line_input(#[case] line: &str, #[case]expected_hand: HandResult, #[case]expected: usize) {
        let solve = part1_linesolve(line);
        let res = solve.0;
        let bet = solve.1;
        assert_eq!(res, expected_hand);
        assert_eq!(bet, expected)
    }

    #[rstest]
    #[case("234TK 765", HighCard(Two, Three, Four, Ten, King), 765_usize)]
    #[case("234TJ 765", OnePair(Two, Three, Four, Ten, Joker), 765_usize)]
    #[case("32T3K 765", OnePair(Three, Two, Ten, Three, King), 765_usize)]
    #[case("2345J 1", OnePair(Two, Three, Four, Five, Joker), 1)]
    #[case("AA223 1", TwoPair(Ace, Ace, Two, Two, Three), 1)]
    #[case("A223J 1", ThreeOfKind(Ace, Two, Two, Three, Joker), 1)]
    #[case("QJ42J 1", ThreeOfKind(Queen, Joker, Four, Two, Joker), 1)]
    #[case("234JJ 1", ThreeOfKind(Two, Three, Four, Joker, Joker), 1)]
    #[case("AA222 1", FullHouse(Ace, Ace, Two, Two, Two), 1)]
    #[case("AA22J 1", FullHouse(Ace, Ace, Two, Two, Joker), 1)]
    #[case("AA2JJ 1", FourOfKind(Ace, Ace, Two, Joker, Joker), 1)]
    #[case("T5555 684", FourOfKind(Ten, Five, Five, Five, Five), 684_usize)]
    #[case("T55J5 684", FourOfKind(Ten, Five, Five, Joker, Five), 684_usize)]
    #[case("JJQKK 1", FourOfKind(Joker, Joker, Queen, King, King), 1)]
    #[case("AJJ2J 1", FourOfKind(Ace, Joker, Joker, Two, Joker), 1)]
    #[case("23JJJ 1", FourOfKind(Two, Three, Joker, Joker, Joker), 1)]
    #[case("AAAAA 1", FiveOfKind(Ace, Ace, Ace, Ace, Ace), 1)]
    #[case("AAAAJ 1", FiveOfKind(Ace, Ace, Ace, Ace, Joker), 1)]
    #[case("2JJJJ 1", FiveOfKind(Two, Joker, Joker, Joker, Joker), 1)]
    #[case("JJJJJ 1", FiveOfKind(Joker, Joker, Joker, Joker, Joker), 1)]
    fn part2_line_input(#[case] line: &str, #[case]expected_hand: HandResult, #[case]expected: usize) {
        let solve = part2_linesolve(line);
        let res = solve.0;
        let bet = solve.1;
        assert_eq!(res, expected_hand);
        assert_eq!(bet, expected)
    }

    //
    // #[rstest]
    // #[case(vec ! [Card::Three, Card::Two, Card::Ten, Card::Jack, Card::King], HandResult::HighCard(Card::King))]
    // #[case(vec ! [Card::Ace, Card::Ace, Card::Ace, Card::Ace, Card::Ace], HandResult::FiveOfKind(Card::Ace))]
    // #[case(vec ! [Card::Three, Card::Two, Card::Ten, Card::Three, Card::King], HandResult::OnePair(Card::Three, Card::King, Card::Ten, Card::Two))]
    // #[case(vec ! [Card::Three, Card::Two, Card::King, Card::Three, Card::Three], HandResult::ThreeOfKind(Card::Three, Card::King, Card::Two))]
    // #[case(vec ! [Card::Three, Card::Two, Card::Two, Card::Three, Card::King], HandResult::TwoPair(Card::Three, Card::Two, Card::King))]
    // #[case(vec ! [Card::Three, Card::Three, Card::Three, Card::Three, Card::King], HandResult::FourOfKind(Card::Three, Card::King))]
    // #[case(vec ! [Card::Three, Card::Three, Card::Three, Card::King, Card::King], HandResult::FullHouse(Card::Three, Card::King))]
    // fn cards_to_result(#[case] cards: Vec<Card>, #[case] expected: HandResult) {
    //     let res = cards.to_result();
    //     assert_eq!(res, expected);
    // }

    #[test]
    fn part2_lineinput() {}
}
