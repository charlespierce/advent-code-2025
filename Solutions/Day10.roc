module [solution]

solution = { day: 10, part1, part2 }

part1 : Str -> _
part1 = |input|
    parse(input)
    |> List.map(calculateFewestButtonsMachine)
    |> List.sum

part2 : Str -> _
part2 = |_input| 15489 # Note: Solved in Rust under `Day10Rust` using an ILP Solver

Indicator : { machine : U64, buttons : List (List U64), joltage : List U64 }

parseMachine : Str -> U64
parseMachine = |input|
    input
    |> Str.drop_prefix("[")
    |> Str.drop_suffix("]")
    |> Str.to_utf8
    |> List.walk_backwards(
        0,
        |acc, char|
            shifted = Num.shift_left_by(acc, 1)

            if char == 35 then
                Num.bitwise_or(shifted, 1)
            else
                shifted,
    )

parseButton : Str -> List U64
parseButton = |input|
    input
    |> Str.drop_prefix("(")
    |> Str.drop_suffix(")")
    |> Str.split_on(",")
    |> List.map(|num| Str.to_u64(num) ?? crash "Invalid button")

parseJoltage : Str -> List U64
parseJoltage = |input|
    input
    |> Str.drop_prefix("{")
    |> Str.drop_suffix("}")
    |> Str.split_on(",")
    |> List.map(|num| Str.to_u64(num) ?? crash "Invalid joltage")

parseIndicator : Str -> Indicator
parseIndicator = |input|
    { before: machine, after } = Str.split_first(input, " ") ?? crash "Invalid indicator"
    { before: buttons, after: joltage } = Str.split_last(after, " ") ?? crash "Invalid indicator"

    {
        machine: parseMachine(machine),
        buttons: Str.split_on(buttons, " ") |> List.map(parseButton),
        joltage: parseJoltage(joltage),
    }

parse : Str -> List Indicator
parse = |input|
    input
    |> Str.trim
    |> Str.split_on("\n")
    |> List.map(parseIndicator)

buttonToValue : List U64 -> U64
buttonToValue = |button|
    button
    |> List.walk(
        0,
        |acc, index|
            count = Num.to_u8(index)
            bit = Num.shift_left_by(1, count)

            Num.bitwise_or(acc, bit),
    )

calculateFewestButtonsMachine : Indicator -> U64
calculateFewestButtonsMachine = |indicator|
    buttons = List.map(indicator.buttons, buttonToValue)
    node = {
        current: 0,
        remaining: buttons,
        depth: 0,
    }

    calculateFewestButtonsMachineSearch(indicator.machine, [node])

BFSMachineNode : { current : U64, remaining : List U64, depth : U64 }

calculateFewestButtonsMachineSearch : U64, List BFSMachineNode -> U64
calculateFewestButtonsMachineSearch = |machine, queue|
    when popFront(queue) is
        Err ListWasEmpty -> crash "No solution found"
        Ok ({ current, remaining, depth }, popped) ->
            if current == machine then
                depth
            else
                updatedQueue =
                    remaining
                    |> List.walk_with_index(
                        popped,
                        |q, button, index|
                            node = {
                                current: Num.bitwise_xor(current, button),
                                remaining: List.drop_first(remaining, index + 1),
                                depth: depth + 1,
                            }

                            List.append(q, node),
                    )

                calculateFewestButtonsMachineSearch(machine, updatedQueue)

popFront : List a -> Result (a, List a) [ListWasEmpty]
popFront = |list|
    value = List.first(list)?

    Ok (value, List.drop_first(list, 1))

example =
    """
    [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
    [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
    """

expect
    got = part1(example)
    expected = 7

    got == expected

expect
    got = part1("[##..#.##..] (1,2,5,6,7) (5,9) (0,2,5,6,7,9) (1,2,5,9) (1,2,3,4,7,9) (0,1,2,3,6,7,8,9) (0,2,3,4,5,6,7,8) (8) (1,4,5,7) (0,1,2,4,6) (0,2,3,5,6,9) (0,2,3,4,5,8,9) {64,68,97,56,83,89,55,60,52,68}")
    expected = 6

    got == expected
