module [solution]

solution = { day: 4, part1, part2 }

part1 : Str -> _
part1 = |input|
    input
    |> parse
    |> removeRolls
    |> .count

part2 : Str -> _
part2 = |input|
    input
    |> parse
    |> iterateRemove(0)

Point : { x : I64, y : I64 }

Cell : [Empty, Roll]

Grid : Dict Point Cell

parse : Str -> Grid
parse = |input|
    input
    |> Str.trim
    |> Str.split_on("\n")
    |> List.map_with_index(parseLine)
    |> List.join
    |> Dict.from_list

parseLine : Str, U64 -> List (Point, Cell)
parseLine = |line, y|
    line
    |> Str.to_utf8
    |> List.map_with_index(
        |chr, x|
            point = { x: Num.to_i64(x), y: Num.to_i64(y) }

            if chr == 64 then
                (point, Roll)
            else
                (point, Empty),
    )

getCell : Grid, Point -> Cell
getCell = |grid, point|
    grid
    |> Dict.get(point)
    |> Result.with_default(Empty)

neighbors : Point -> List Point
neighbors = |point| [
    { x: point.x - 1, y: point.y - 1 },
    { x: point.x, y: point.y - 1 },
    { x: point.x + 1, y: point.y - 1 },
    { x: point.x - 1, y: point.y },
    { x: point.x + 1, y: point.y },
    { x: point.x - 1, y: point.y + 1 },
    { x: point.x, y: point.y + 1 },
    { x: point.x + 1, y: point.y + 1 },
]

accessible : Grid, Point -> Bool
accessible = |grid, point|
    neighbors(point)
    |> List.keep_if(|neighbor| getCell(grid, neighbor) == Roll)
    |> List.len
    |> Num.is_lt(4)

removeRolls : Grid -> { count : U64, remaining : Grid }
removeRolls = |grid|
    grid
    |> Dict.walk(
        { count: 0, remaining: Dict.empty({}) },
        |state, point, cell|
            when cell is
                Empty -> { state & remaining: Dict.insert(state.remaining, point, Empty) }
                Roll ->
                    if accessible(grid, point) then
                        { count: state.count + 1, remaining: Dict.insert(state.remaining, point, Empty) }
                    else
                        { state & remaining: Dict.insert(state.remaining, point, Roll) },
    )

iterateRemove : Grid, U64 -> U64
iterateRemove = |grid, count|
    removed = removeRolls(grid)

    if removed.count == 0 then
        count
    else
        iterateRemove(removed.remaining, count + removed.count)

example =
    """
    ..@@.@@@@.
    @@@.@.@.@@
    @@@@@.@.@@
    @.@@@@..@.
    @@.@@@@.@@
    .@@@@@@@.@
    .@.@.@.@@@
    @.@@@.@@@@
    .@@@@@@@@.
    @.@.@@@.@.
    """

expect
    got = part1(example)
    expected = 13

    got == expected

expect
    got = part2(example)
    expected = 43

    got == expected
