module [solution]

solution = { day: 11, part1, part2 }

part1 : Str -> _
part1 = |input|
    parse(input)
    |> traverse("you", "out")

part2 : Str -> _
part2 = |input|
    parse(input)
    |> findPaths("svr", "fft", "dac", "out")

Graph : { nodes : Dict Str (List Str) }

parseNode : Str -> (Str, List Str)
parseNode = |input|
    { before: id, after: linksStr } = Str.split_first(input, ": ") ?? crash "Invalid node"
    links = Str.split_on(linksStr, " ")

    (id, links)

parse : Str -> Graph
parse = |input|
    nodes =
        Str.trim(input)
        |> Str.split_on("\n")
        |> List.map(parseNode)
        |> Dict.from_list

    { nodes }

traverse : Graph, Str, Str -> U64
traverse = |graph, current, end|
    traverseMemoized(graph, current, end, Dict.empty({})).0

traverseMemoized : Graph, Str, Str, Dict Str U64 -> (U64, Dict Str U64)
traverseMemoized = |graph, current, end, memo|
    when Dict.get(memo, current) is
        Ok value -> (value, memo)
        Err _ ->
            (result, updatedMemo) = traverseMemoizedCalculate(graph, current, end, memo)
            (result, Dict.insert(updatedMemo, current, result))

traverseMemoizedCalculate : Graph, Str, Str, Dict Str U64 -> (U64, Dict Str U64)
traverseMemoizedCalculate = |graph, current, end, memo|
    if current == end then
        (1, memo)
    else
        graph.nodes
        |> Dict.get(current)
        |> Result.with_default([])
        |> List.walk(
            (0, memo),
            |(count, m), id|
                (inner, newMemo) = traverseMemoized(graph, id, end, m)
                (inner + count, newMemo),
        )

findPaths : Graph, Str, Str, Str, Str -> U64
findPaths = |graph, start, through1, through2, end|
    # start -> 1 -> 2 -> end
    path1part1 = traverse(graph, start, through1)
    path1part2 = traverse(graph, through1, through2)
    path1part3 = traverse(graph, through2, end)

    # start -> 2 -> 1 -> end
    path2part1 = traverse(graph, start, through2)
    path2part2 = traverse(graph, through2, through1)
    path2part3 = traverse(graph, through1, end)

    (path1part1 * path1part2 * path1part3) + (path2part1 * path2part2 * path2part3)

example =
    """
    aaa: you hhh
    you: bbb ccc
    bbb: ddd eee
    ccc: ddd eee fff
    ddd: ggg
    eee: out
    fff: out
    ggg: out
    hhh: ccc fff iii
    iii: out
    """

expect
    got = part1(example)
    expected = 5

    got == expected

example2 =
    """
    svr: aaa bbb
    aaa: fft
    fft: ccc
    bbb: tty
    tty: ccc
    ccc: ddd eee
    ddd: hub
    hub: fff
    eee: dac
    dac: fff
    fff: ggg hhh
    ggg: out
    hhh: out
    """

expect
    got = part2(example2)
    expected = 2

    got == expected
