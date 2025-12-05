module [solution]

solution = { day: 5, part1, part2 }

part1 : Str -> _
part1 = |input|
    data = parse(input)

    data.ingredients
    |> List.walk(
        0,
        |count, ingredient|
            if isFresh(ingredient, data.ranges) then
                count + 1
            else
                count,
    )

part2 : Str -> _
part2 = |input|
    input
    |> parse
    |> .ranges
    |> iterateMerging
    |> List.map(rangeLength)
    |> List.sum

Range : { start : U64, end : U64 }

parse : Str -> { ranges : List Range, ingredients : List U64 }
parse = |input|
    trimmed = Str.trim(input)
    values = Str.split_first(trimmed, "\n\n") ?? crash "Invalid input"

    ranges = parseRanges(values.before)
    ingredients = parseIngredients(values.after)

    { ranges, ingredients }

parseRanges : Str -> List Range
parseRanges = |input|
    input
    |> Str.split_on("\n")
    |> List.map(
        |line|
            values = Str.split_first(line, "-") ?? crash "Invalid line"

            start = Str.to_u64(values.before) ?? crash "Invalid value"
            end = Str.to_u64(values.after) ?? crash "Invalid value"

            { start, end },
    )

parseIngredients : Str -> List U64
parseIngredients = |input|
    input
    |> Str.split_on("\n")
    |> List.map(|line| Str.to_u64(line) ?? crash "Invalid value")

inRange : U64, Range -> Bool
inRange = |value, range| value >= range.start and value <= range.end

rangeLength : Range -> U64
rangeLength = |range| range.end - range.start + 1

isFresh : U64, List Range -> Bool
isFresh = |ingredient, ranges|
    ranges
    |> List.any(|range| inRange(ingredient, range))

shouldMerge : Range, Range -> Bool
shouldMerge = |range1, range2|
    range1.start <= range2.end and range1.end >= range2.start

merge : Range, Range -> Range
merge = |range1, range2|
    start = Num.min(range1.start, range2.start)
    end = Num.max(range1.end, range2.end)

    { start, end }

mergeRanges : List Range -> (Bool, List Range)
mergeRanges = |ranges|
    ranges
    |> List.walk(
        (Bool.false, []),
        |(merged, updated), existing|
            (didMerge, newUpdated) = addOrMerge(updated, existing)

            (didMerge or merged, newUpdated),
    )

addOrMerge : List Range, Range -> (Bool, List Range)
addOrMerge = |ranges, range|
    (didMerge, newRanges) =
        ranges
        |> List.walk(
            (Bool.false, []),
            |(merged, updated), existing|
                if shouldMerge(existing, range) then
                    (Bool.true, List.append(updated, merge(existing, range)))
                else
                    (merged, List.append(updated, existing)),
        )

    if didMerge then
        (Bool.true, newRanges)
    else
        (Bool.false, List.append(newRanges, range))

iterateMerging : List Range -> List Range
iterateMerging = |ranges|
    (merged, updated) = mergeRanges(ranges)

    if merged then
        iterateMerging(updated)
    else
        updated

example =
    """
    3-5
    10-14
    16-20
    12-18

    1
    5
    8
    11
    17
    32
    """

expect
    got = part1(example)
    expected = 3

    got == expected

expect
    got = part2(example)
    expected = 14

    got == expected
