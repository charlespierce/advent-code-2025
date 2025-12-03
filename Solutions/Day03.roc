module [solution]

solution = { day: 3, part1, part2 }

part1 : Str -> _
part1 = |input|
    input
    |> parse
    |> List.map(bestJoltage(2))
    |> List.sum

part2 : Str -> _
part2 = |input|
    input
    |> parse
    |> List.map(bestJoltage(12))
    |> List.sum

Bank : List U8

parse : Str -> List Bank
parse = |input|
    input
    |> Str.trim
    |> Str.split_on("\n")
    |> List.map(parseBank)

parseBank : Str -> Bank
parseBank = |line|
    line
    |> Str.to_utf8
    |> List.map(|byte| byte - 48)

combine : List U8 -> U64
combine = |digits|
    digits
    |> List.walk(0, |sum, digit| sum * 10 + Num.to_u64(digit))

DigitState : { digits : List U8, count : U64 }

maximize : List U8 -> List U8
maximize = |list|
    list
    |> List.map_with_index(|_, index| list |> List.drop_at(index))
    |> List.walk(
        [],
        |current, next|
            curValue = combine(current)
            nextValue = combine(next)

            if nextValue > curValue then
                next
            else
                current,
    )

bestJoltage : U64 -> (Bank -> U64)
bestJoltage = |count|
    |bank|
        bank
        |> List.walk_backwards({ digits: [], count }, handleDigit)
        |> .digits
        |> combine

handleDigit : DigitState, U8 -> DigitState
handleDigit = |state, digit|
    if state.count > List.len(state.digits) then
        digits = state.digits |> List.prepend(digit)
        { state & digits }
    else
        first = List.first(state.digits) ?? 0
        if digit >= first then
            digits =
                state.digits
                |> List.prepend(digit)
                |> maximize
            { state & digits }
        else
            state

example =
    """
    987654321111111
    811111111111119
    234234234234278
    818181911112111
    """

expect
    got = part1(example)
    expected = 357

    got == expected

expect
    got = part2(example)
    expected = 3121910778619

    got == expected
