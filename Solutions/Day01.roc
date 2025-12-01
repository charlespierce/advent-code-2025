module [solution]

solution = { day: 1, part1, part2 }

part1 : Str -> _
part1 = |input|
    parse(input)
    |> List.walk(
        { zeroes: 0, position: 50 },
        |state, move|
            next = position(state.position + move)

            if next == 0 then
                { zeroes: state.zeroes + 1, position: 0 }
            else
                { state & position: next },
    )
    |> .zeroes

part2 : Str -> _
part2 = |input|
    parse(input)
    |> List.walk(
        { zeroes: 0, position: 50 },
        |state, move|
            next = state.position + move
            zeroes = state.zeroes + countZeroes(state.position, move)
            new = position(next)

            { zeroes, position: new },
    )
    |> .zeroes

parse : Str -> List I64
parse = |input|
    Str.split_on(input, "\n")
    |> List.map(parseMove)

parseMove : Str -> I64
parseMove = |move|
    when Str.split_first(move, "L") is
        Ok { after } -> Str.to_i64(after) |> Result.with_default(0) |> Num.mul(-1)
        Err _ ->
            when Str.split_first(move, "R") is
                Ok { after } -> Str.to_i64(after) |> Result.with_default(0)
                Err _ -> 0

position : I64 -> I64
position = |unedited|
    mod = unedited % 100

    if mod < 0 then
        mod + 100
    else
        mod

countZeroes : I64, I64 -> I64
countZeroes = |start, move|
    next = start + move
    zeroes = Num.div_trunc(next, 100) |> Num.abs

    if start == 0 then
        zeroes
    else if next <= 0 then
        1 + zeroes
    else
        zeroes

example =
    """
    L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
    """

expect
    actual = part1(example)

    actual == 3

expect
    actual = part2(example)

    actual == 6
