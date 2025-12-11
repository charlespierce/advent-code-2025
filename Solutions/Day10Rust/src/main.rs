use good_lp::{
    default_solver,
    variable::{variable, ProblemVariables},
    Expression, Solution, SolverModel,
};
use nom::{
    bytes::complete::{tag, take_until},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, pair, preceded, separated_pair},
    IResult, Parser,
};
use std::fs::read_to_string;

struct Button {
    indexes: Vec<usize>,
}

struct Joltages {
    values: Vec<usize>,
}

fn parse_button(s: &str) -> IResult<&str, Button> {
    map(
        delimited(
            tag("("),
            separated_list1(tag(","), nom::character::complete::usize),
            tag(")"),
        ),
        |indexes| Button { indexes },
    )
    .parse(s)
}

fn parse_joltage(s: &str) -> IResult<&str, Joltages> {
    map(
        delimited(
            tag("{"),
            separated_list1(tag(","), nom::character::complete::usize),
            tag("}"),
        ),
        |values| Joltages { values },
    )
    .parse(s)
}

fn parse_machine(s: &str) -> IResult<&str, Machine> {
    map(
        preceded(
            pair(take_until(" "), tag(" ")),
            separated_pair(
                separated_list1(tag(" "), parse_button),
                tag(" "),
                parse_joltage,
            ),
        ),
        |(buttons, joltages)| Machine { buttons, joltages },
    )
    .parse(s)
}

fn parse_input(s: &str) -> IResult<&str, Puzzle> {
    map(separated_list1(tag("\n"), parse_machine), |machines| {
        Puzzle { machines }
    })
    .parse(s.trim())
}

struct Machine {
    buttons: Vec<Button>,
    joltages: Joltages,
}

struct Puzzle {
    machines: Vec<Machine>,
}

impl Machine {
    fn solve(&self) -> usize {
        let mut problem = ProblemVariables::new();

        let coefficients = self
            .buttons
            .iter()
            .map(|button| {
                let variable = problem.add(variable().integer().min(0));

                (variable, button)
            })
            .collect::<Vec<_>>();

        let mut expression = Expression::with_capacity(coefficients.len());

        for (variable, _) in &coefficients {
            expression.add_mul(1, variable);
        }

        let mut model = problem.minimise(&expression).using(default_solver);

        for (index, joltage) in self.joltages.values.iter().enumerate() {
            let mut constraint_expr = Expression::with_capacity(coefficients.len());

            for (variable, button) in &coefficients {
                if button.indexes.iter().find(|i| **i == index).is_some() {
                    constraint_expr.add_mul(1, variable);
                }
            }

            model.add_constraint(constraint_expr.eq(*joltage as u32));
        }

        let solution = model.solve().unwrap();

        let value = solution.eval(expression) as usize;

        println!("Value: {value}");

        value
    }
}

impl Puzzle {
    fn solve(&self) -> usize {
        self.machines
            .iter()
            .map(|machine| {
                let result = machine.solve();

                println!("Intermediate Result: {result}");

                result
            })
            .sum()
    }
}

fn main() {
    let example = r#"
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"#;

    println!(
        "Example Answer: {}",
        parse_input(example).unwrap().1.solve()
    );

    let input = read_to_string("../../input/2025/10.txt").unwrap();

    println!("Actual Answer: {}", parse_input(&input).unwrap().1.solve());
}
