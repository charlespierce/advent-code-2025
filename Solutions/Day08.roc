module [solution]

solution = { day: 8, part1, part2 }

part1 : Str -> _
part1 = |input| executePart1(input, 1000)

part2 : Str -> _
part2 = |input|
    junctions = parse(input)
    pairs = calculateDistances(junctions)
    circuits = generateCircuits(junctions)

    { pair: (junction1, junction2) } = connectAll(circuits, pairs)

    junction1.x * junction2.x

Junction : { x : U64, y : U64, z : U64 }

JunctionPair : { pair : (Junction, Junction), distance : F64 }

distance : Junction, Junction -> F64
distance = |point1, point2|
    fx1 = Num.to_f64(point1.x)
    fx2 = Num.to_f64(point2.x)
    fy1 = Num.to_f64(point1.y)
    fy2 = Num.to_f64(point2.y)
    fz1 = Num.to_f64(point1.z)
    fz2 = Num.to_f64(point2.z)

    diffx = Num.pow(fx2 - fx1, 2.0)
    diffy = Num.pow(fy2 - fy1, 2.0)
    diffz = Num.pow(fz2 - fz1, 2.0)

    Num.sqrt(diffx + diffy + diffz)

sortedPair : Junction, Junction -> (Junction, Junction)
sortedPair = |junction1, junction2|
    if junction1.x < junction2.x then
        (junction1, junction2)
    else if junction1.x > junction2.x then
        (junction2, junction1)
    else if junction1.y < junction2.y then
        (junction1, junction2)
    else if junction1.y > junction2.y then
        (junction2, junction1)
    else if junction1.z < junction2.z then
        (junction1, junction2)
    else if junction1.z > junction2.z then
        (junction2, junction1)
    else
        (junction1, junction2)

parse : Str -> List Junction
parse = |input|
    Str.trim(input)
    |> Str.split_on("\n")
    |> List.map(
        |row|
            values = Str.split_on(row, ",") |> List.map(|num| Str.to_u64(num) ?? crash "Invalid coordinate")
            when values is
                [x, y, z] -> { x, y, z }
                _ -> crash "Incorrect number of coordinates",
    )

calculateDistances : List Junction -> List JunctionPair
calculateDistances = |junctions|
    junctions
    |> List.map_with_index(
        |junction1, index|
            junctions
            |> List.drop_first(index + 1)
            |> List.map(
                |junction2|
                    { pair: sortedPair(junction1, junction2), distance: distance(junction1, junction2) },
            ),
    )
    |> List.join
    |> List.sort_with(
        |pair1, pair2|
            if pair1.distance > pair2.distance then
                LT
            else if pair1.distance < pair2.distance then
                GT
            else
                EQ,
    )

generateCircuits : List Junction -> List (List Junction)
generateCircuits = |junctions|
    List.map(junctions, List.single)

pop : List a -> Result (a, List a) [ListWasEmpty]
pop = |list|
    List.last(list)
    |> Result.map_ok(
        |value|
            (
                value,
                List.drop_last(list, 1),
            ),
    )

connectJunctions : List (List Junction), (Junction, Junction) -> List (List Junction)
connectJunctions = |circuits, (junction1, junction2)|
    circuits
    |> List.walk(
        { circuits: [], holding: None },
        |updated, circuit|
            (contains1, contains2) =
                circuit
                |> List.walk(
                    (Bool.false, Bool.false),
                    |(c1, c2), junc|
                        if junc == junction1 then
                            (Bool.true, c2)
                        else if junc == junction2 then
                            (c1, Bool.true)
                        else
                            (c1, c2),
                )

            if contains1 == contains2 then
                { updated & circuits: List.append(updated.circuits, circuit) }
            else
                when updated.holding is
                    None -> { updated & holding: Some circuit }
                    Some previous ->
                        merged = List.concat(circuit, previous)
                        { circuits: List.append(updated.circuits, merged), holding: None },
    )
    |> .circuits

iterateConnect : List (List Junction), List JunctionPair, U64 -> List (List Junction)
iterateConnect = |circuits, pairs, count|
    List.range({ start: At 0, end: Before count })
    |> List.walk(
        { circuits, pairs },
        |old, _|
            (pair, newPairs) = pop(old.pairs) ?? crash "Invalid iteration count"
            newCircuits = connectJunctions(old.circuits, pair.pair)

            { circuits: newCircuits, pairs: newPairs },
    )
    |> .circuits

connectAll : List (List Junction), List JunctionPair -> JunctionPair
connectAll = |circuits, pairs|
    (pair, newPairs) = pop(pairs) ?? crash "Invalid puzzle"
    newCircuits = connectJunctions(circuits, pair.pair)

    if List.len(newCircuits) == 1 then
        pair
    else
        connectAll(newCircuits, newPairs)

calculateValue : List (List Junction) -> U64
calculateValue = |circuits|
    circuits
    |> List.sort_with(
        |c1, c2|
            if List.len(c1) > List.len(c2) then
                LT
            else if List.len(c1) < List.len(c2) then
                GT
            else
                EQ,
    )
    |> List.take_first(3)
    |> List.map(List.len)
    |> List.walk(1, |acc, len| acc * len)

executePart1 : Str, U64 -> U64
executePart1 = |input, count|
    junctions = parse(input)
    pairs = calculateDistances(junctions)
    circuits = generateCircuits(junctions)

    iterateConnect(circuits, pairs, count)
    |> calculateValue

example =
    """
    162,817,812
    57,618,57
    906,360,560
    592,479,940
    352,342,300
    466,668,158
    542,29,236
    431,825,988
    739,650,466
    52,470,668
    216,146,977
    819,987,18
    117,168,530
    805,96,715
    346,949,466
    970,615,88
    941,993,340
    862,61,35
    984,92,344
    425,690,689
    """

expect
    got = executePart1(example, 10)
    expected = 40

    got == expected

expect
    got = part2(example)
    expected = 25272

    got == expected
