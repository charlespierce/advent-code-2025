module [solution]

solution = { day: 2, part1, part2 }

part1 : Str -> _
part1 = |input|
    Str.trim(input)
    |> parse
    |> List.map(sumInvalidInRange)
    |> List.sum

part2 : Str -> _
part2 = |input|
    Str.trim(input)
    |> parse
    |> List.map(sumInvalidInRangePart2)
    |> List.sum

Range : { start : U64, end : U64 }

parseId : Str -> U64
parseId = |num|
    when Str.to_u64(num) is
        Ok(value) -> value
        Err(_) ->
            crash "Invalid Number"

parseRange : Str -> Range
parseRange = |input|
    when Str.split_first(input, "-") is
        Ok({ before, after }) ->
            { start: parseId(before), end: parseId(after) }

        Err(_) -> crash "Invalid range"

parse : Str -> List Range
parse = |input|
    Str.split_on(input, ",") |> List.map(parseRange)

digitCount : U64 -> U64
digitCount = |number|
    digitCountHelper(number, 0)

digitCountHelper : U64, U64 -> U64
digitCountHelper = |number, count|
    if number == 0 then
        count
    else
        digitCountHelper(Num.div_trunc(number, 10), count + 1)

isInvalid : U64 -> Bool
isInvalid = |number|
    digits = digitCount(number)

    if Num.is_even(digits) then
        order = Num.pow_int(10, Num.div_trunc(digits, 2))

        Num.div_trunc(number, order) == Num.rem(number, order)
    else
        Bool.false

sumInvalidInRange : Range -> U64
sumInvalidInRange = |range|
    sumInvalidInRangeHelper(range, 0)

sumInvalidInRangeHelper : Range, U64 -> U64
sumInvalidInRangeHelper = |range, sum|
    newSum = if isInvalid(range.start) then
        sum + range.start
    else
        sum

    if range.start == range.end then
        newSum
    else
        nextRange = { range & start: range.start + 1 }
        sumInvalidInRangeHelper(nextRange, newSum)

sumInvalidInRangePart2 : Range -> U64
sumInvalidInRangePart2 = |range|
    sumInvalidInRangePart2Helper(range, 0)

sumInvalidInRangePart2Helper : Range, U64 -> U64
sumInvalidInRangePart2Helper = |range, sum|
    newSum = if isInvalidPart2(range.start) then
        sum + range.start
    else
        sum

    if range.start == range.end then
        newSum
    else
        nextRange = { range & start: range.start + 1 }
        sumInvalidInRangePart2Helper(nextRange, newSum)

generateDuplicates : U64, U64, U64 -> U64
generateDuplicates = |base, order, count|
    generateDuplicatesHelper(base, order, count, 0)

generateDuplicatesHelper : U64, U64, U64, U64 -> U64
generateDuplicatesHelper = |base, order, count, sum|
    if count == 0 then
        sum
    else
        newSum = (sum * order) + base

        generateDuplicatesHelper(base, order, count - 1, newSum)

isInvalidPart2 : U64 -> Bool
isInvalidPart2 = |number|
    digits = digitCount(number)
    isInvalidPart2Helper(number, digits, 10)

isInvalidPart2Helper : U64, U64, U64 -> Bool
isInvalidPart2Helper = |number, digits, divisor|
    if divisor == 1 then
        Bool.false
    else if Num.rem(digits, divisor) == 0 then
        order = Num.pow_int(10, Num.div_trunc(digits, divisor))
        base = Num.rem(number, order)
        compare = generateDuplicates(base, order, divisor)

        if number == compare then
            Bool.true
        else
            isInvalidPart2Helper(number, digits, divisor - 1)
    else
        isInvalidPart2Helper(number, digits, divisor - 1)

example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

expect
    got = part1(example)
    expected = 1227775554

    got == expected

expect
    got = part2(example)
    expected = 4174379265

    got == expected
