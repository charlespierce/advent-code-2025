module [solution]

solution = { day: 9, part1, part2 }

part1 : Str -> _
part1 = |input|
    parse(input)
    |> pairwise
    |> List.map(|(p1, p2)| rectangleArea(p1, p2))
    |> List.max
    |> Result.with_default(0)

part2 : Str -> _
part2 = |input|
    corners = parse(input)
    edges = getEdges(corners)

    pairwise(corners)
    |> List.keep_oks(
        |(p1, p2)|
            if rectangleInPolygon(p1, p2, edges) then
                Ok rectangleArea(p1, p2)
            else
                Err NotEnclosed,
    )
    |> List.max
    |> Result.with_default(0)

Point : { x : U64, y : U64 }

Edge : [
    Vertical { bottom : U64, top : U64, x : U64 },
    Horizontal { left : U64, right : U64, y : U64 },
]

parse : Str -> List Point
parse = |input|
    Str.trim(input)
    |> Str.split_on("\n")
    |> List.map(parsePoint)

parsePoint : Str -> Point
parsePoint = |input|
    when Str.split_on(input, ",") is
        [xStr, yStr] ->
            x = Str.to_u64(xStr) ?? crash "Invalid coordinate"
            y = Str.to_u64(yStr) ?? crash "Invalid coordinate"

            { x, y }

        _ -> crash "Invalid point input"

getEdges : List Point -> List Edge
getEdges = |points|
    wrapped = List.append_if_ok(points, List.first(points))

    List.drop_first(wrapped, 1)
    |> List.map_with_index(
        |point1, index|
            point2 = List.get(wrapped, index) ?? crash "Can't happen"

            if point1.x == point2.x then
                bottom = Num.min(point1.y, point2.y)
                top = Num.max(point1.y, point2.y)

                Vertical { bottom, top, x: point1.x }
            else
                left = Num.min(point1.x, point2.x)
                right = Num.max(point1.x, point2.x)

                Horizontal { left, right, y: point1.y },
    )

rectangleInPolygon : Point, Point, List Edge -> Bool
rectangleInPolygon = |point1, point2, edges|
    if point1.x == point2.x or point1.y == point2.y then
        !edgeCrossesRectangle(point1, point2, edges)
    else
        offsetPoint = { x: Num.min(point1.x, point2.x) + 1, y: Num.min(point1.y, point2.y) + 1 }

        pointInPolygon(offsetPoint, edges) and !edgeCrossesRectangle(point1, point2, edges)

pointInPolygon : Point, List Edge -> Bool
pointInPolygon = |point, edges|
    filtered = edgesCrossedHorizontal(point, edges)

    if List.len(filtered) % 2 == 0 then
        List.any(filtered, |e| onEdge(point, e))
    else
        Bool.true

edgeCrossesRectangle : Point, Point, List Edge -> Bool
edgeCrossesRectangle = |point1, point2, edges|
    minX = Num.min(point1.x, point2.x)
    maxX = Num.max(point1.x, point2.x)
    minY = Num.min(point1.y, point2.y)
    maxY = Num.max(point1.y, point2.y)

    edges
    |> List.any(
        |edge|
            when edge is
                Horizontal { left, right, y } ->
                    minY < y and maxY > y and left < maxX and right > minX

                Vertical { top, bottom, x } ->
                    minX < x and maxX > x and bottom < maxY and top > minY,
    )

edgesCrossedHorizontal : Point, List Edge -> List Edge
edgesCrossedHorizontal = |point, edges|
    edges
    |> List.keep_if(
        |edge|
            when edge is
                Horizontal { left, y } -> point.y == y and point.x >= left
                Vertical { top, bottom, x } -> point.x >= x and point.y <= top and point.y >= bottom,
    )

onEdge : Point, Edge -> Bool
onEdge = |point, edge|
    when edge is
        Horizontal { left, right, y } -> point.y == y and point.x >= left and point.x <= right
        Vertical { top, bottom, x } -> point.x == x and point.y >= bottom and point.y <= top

rectangleArea : Point, Point -> U64
rectangleArea = |point1, point2|
    (Num.abs_diff(point1.x, point2.x) + 1) * (Num.abs_diff(point1.y, point2.y) + 1)

pairwise : List a -> List (a, a)
pairwise = |list|
    list
    |> List.map_with_index(
        |item1, index|
            list
            |> List.drop_first(index + 1)
            |> List.map(|item2| (item1, item2)),
    )
    |> List.join

example =
    """
    7,1
    11,1
    11,7
    9,7
    9,5
    2,5
    2,3
    7,3
    """

expect
    got = part1(example)
    expected = 50

    got == expected

expect
    got = part2(example)
    expected = 24

    got == expected
