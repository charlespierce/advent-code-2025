module [solution]

solution = { day: 7, part1, part2 }

part1 : Str -> _
part1 = |input|
    parse(input)
    |> propagateTachyons

part2 : Str -> _
part2 = |input|
    parse(input)
    |> propagateQuantumTachyons

Cell : [Empty, Splitter]

Point : { x : U64, y : U64 }

Grid : { cells : Dict Point Cell, height : U64 }

parse : Str -> (Grid, Point)
parse = |input|
    Str.trim(input)
    |> Str.split_on("\n")
    |> List.map(Str.to_utf8)
    |> List.walk_with_index(
        ({ cells: Dict.empty({}), height: 0 }, { x: 0, y: 0 }),
        |(grid, start), row, y|
            (updatedGrid, updatedStart) =
                row
                |> List.walk_with_index(
                    (grid, start),
                    |(gridInner, startInner), char, x|
                        when char is
                            83 ->
                                (
                                    { gridInner &
                                        cells: Dict.insert(gridInner.cells, { x, y }, Empty),
                                    },
                                    { x, y },
                                )

                            46 ->
                                (
                                    { gridInner &
                                        cells: Dict.insert(gridInner.cells, { x, y }, Empty),
                                    },
                                    startInner,
                                )

                            _ ->
                                (
                                    { gridInner &
                                        cells: Dict.insert(gridInner.cells, { x, y }, Splitter),
                                    },
                                    startInner,
                                ),
                )
            (
                { updatedGrid & height: updatedGrid.height + 1 },
                updatedStart,
            ),
    )

propagateTachyons : (Grid, Point) -> U64
propagateTachyons = |(grid, start)|
    propagateTachyonsHelper(grid, [start.x], 1, 0)

propagateTachyonsHelper : Grid, List U64, U64, U64 -> U64
propagateTachyonsHelper = |grid, tachyonCols, row, splitCount|
    if row >= grid.height then
        splitCount
    else
        (updatedCols, updatedSplitCount) =
            tachyonCols
            |> List.walk(
                ([], splitCount),
                |(cols, count), col|
                    when Dict.get(grid.cells, { x: col, y: row }) is
                        Err _ -> crash "Invalid point somehow"
                        Ok Empty ->
                            if List.last(cols) == Ok col then
                                (cols, count)
                            else
                                (List.append(cols, col), count)

                        Ok Splitter ->
                            if List.last(cols) == Ok (col - 1) then
                                (List.append(cols, col + 1), count + 1)
                            else
                                (cols |> List.append(col - 1) |> List.append(col + 1), count + 1),
            )

        propagateTachyonsHelper(grid, updatedCols, row + 1, updatedSplitCount)

propagateQuantumTachyons : (Grid, Point) -> U64
propagateQuantumTachyons = |(grid, start)|
    startCounts =
        Dict.empty({})
        |> Dict.insert(start.x, 1)

    endCounts = propagateQuantumTachyonsHelper(grid, startCounts, 1)

    Dict.values(endCounts)
    |> List.sum

propagateQuantumTachyonsHelper : Grid, Dict U64 U64, U64 -> Dict U64 U64
propagateQuantumTachyonsHelper = |grid, columnCounts, row|
    if row >= grid.height then
        columnCounts
    else
        updatedCounts =
            columnCounts
            |> Dict.walk(
                Dict.empty({}),
                |newCounts, col, oldCount|
                    when Dict.get(grid.cells, { x: col, y: row }) is
                        Err _ -> crash "Invalid point somehow"
                        Ok Empty -> updateCount(newCounts, col, oldCount)
                        Ok Splitter ->
                            newCounts
                            |> updateCount(col - 1, oldCount)
                            |> updateCount(col + 1, oldCount),
            )

        propagateQuantumTachyonsHelper(grid, updatedCounts, row + 1)

updateCount : Dict U64 U64, U64, U64 -> Dict U64 U64
updateCount = |dict, col, count|
    Dict.update(
        dict,
        col,
        |existing|
            when existing is
                Ok value -> Ok (value + count)
                Err Missing -> Ok count,
    )

example =
    """
    .......S.......
    ...............
    .......^.......
    ...............
    ......^.^......
    ...............
    .....^.^.^.....
    ...............
    ....^.^...^....
    ...............
    ...^.^...^.^...
    ...............
    ..^...^.....^..
    ...............
    .^.^.^.^.^...^.
    ...............
    """

expect
    got = part1(example)
    expected = 21

    got == expected

expect
    got = part2(example)
    expected = 40

    got == expected
