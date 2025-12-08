module [solution]

solution = { day: 6, part1, part2 }

part1 : Str -> _
part1 = |input|
    parse(input)
    |> calculate

part2 : Str -> _
part2 = |input|
    parseCephalopodMath(input)
    |> calculateCephalopodMath

Operator : [Add, Multiply]

MathWork : { operators : List Operator, operands : List (List U64) }

parseOperators : Str -> List Operator
parseOperators = |input|
    Str.split_on(input, " ")
    |> List.drop_if(Str.is_empty)
    |> List.map(
        |chr|
            if chr == "*" then
                Multiply
            else
                Add,
    )

parseOperands : Str -> List (List U64)
parseOperands = |input|
    Str.split_on(input, "\n")
    |> List.map(
        |line|
            Str.split_on(line, " ")
            |> List.drop_if(Str.is_empty)
            |> List.map(
                |num|
                    Str.to_u64(num) ?? 0,
            ),
    )

parse : Str -> MathWork
parse = |input|
    trimmed = Str.trim(input)
    parts =
        Str.split_last(trimmed, "\n")
        ?? crash "Invalid input"

    operands = parseOperands(parts.before)
    operators = parseOperators(parts.after)

    { operands, operators }

parseCephalopodMath : Str -> MathWork
parseCephalopodMath = |input|
    trimmed = Str.trim(input)
    parts =
        Str.split_last(trimmed, "\n")
        ?? crash "Invalid input"

    operands = parseCephalopodOperands(parts.before)
    operators = parseOperators(parts.after)

    { operands, operators }

parseCephalopodOperands : Str -> List (List U64)
parseCephalopodOperands = |input|
    chars =
        Str.split_on(input, "\n")
        |> List.map(Str.to_utf8)

    columns = List.get(chars, 0) |> Result.map_ok(List.len) |> Result.with_default(0)

    List.range({ start: At 0, end: Before columns })
    |> List.map(
        |column|
            chars
            |> List.map(
                |row|
                    List.get(row, column)
                    |> Result.with_default(32),
            )
            |> Str.from_utf8
            |> Result.with_default("")
            |> Str.trim
            |> Str.to_u64
            |> Result.with_default(0),
    )
    |> List.split_on(0)

identities : List Operator -> List U64
identities = |operators|
    operators
    |> List.map(
        |op|
            when op is
                Add -> 0
                Multiply -> 1,
    )

calculate : MathWork -> U64
calculate = |work|
    start = identities(work.operators)

    work.operands
    |> List.walk(
        start,
        |outputs, line|
            line
            |> List.walk_with_index(
                [],
                |acc, number, index|
                    previous = List.get(outputs, index) ?? crash "Invalid number of items"

                    when List.get(work.operators, index) is
                        Ok Add -> List.append(acc, previous + number)
                        Ok Multiply -> List.append(acc, previous * number)
                        Err _ -> crash "Invalid number of operators",
            ),
    )
    |> List.sum

calculateCephalopodMath : MathWork -> U64
calculateCephalopodMath = |work|
    List.map2(
        work.operands,
        work.operators,
        |operands, operator|
            start =
                when operator is
                    Add -> 0
                    Multiply -> 1

            List.walk(
                operands,
                start,
                |acc, number|
                    when operator is
                        Add -> acc + number
                        Multiply -> acc * number,
            ),
    )
    |> List.sum

example =
    """
    123 328  51 64 
     45 64  387 23 
      6 98  215 314
    *   +   *   +  
    """

expect
    got = part1(example)
    expected = 4277556

    got == expected

expect
    got = part2(example)
    expected = 3263827

    got == expected
