module [solution]

solution = { day: 12, part1, part2 }

part1 : Str -> _
part1 = |input|
    (tiles, regions) = parse(input)

    regions
    |> List.keep_if(|region| areaPossible(region, tiles))
    |> List.len

part2 : Str -> _
part2 = |_input| TODO

parseTileSize : Str -> U64
parseTileSize = |input|
    input
    |> Str.split_on("\n")
    |> List.drop_first(1)
    |> List.map(parseLineSize)
    |> List.sum

parseLineSize : Str -> U64
parseLineSize = |line|
    line
    |> Str.to_utf8
    |> List.walk(
        0,
        |count, char|
            if char == 35 then
                count + 1
            else
                count,
    )

parseTiles : Str -> List U64
parseTiles = |input|
    input
    |> Str.split_on("\n\n")
    |> List.map(parseTileSize)

Region : { width : U64, height : U64, shapes : List U64 }

parseRegion : Str -> Region
parseRegion = |input|
    { before: dimensions, after: shapeCounts } = Str.split_first(input, ": ") ?? crash "Invalid region"
    { before: widthStr, after: heightStr } = Str.split_first(dimensions, "x") ?? crash "Invalid region"

    width = Str.to_u64(widthStr) ?? crash "Invalid dimension"
    height = Str.to_u64(heightStr) ?? crash "Invalid dimension"

    shapes =
        shapeCounts
        |> Str.split_on(" ")
        |> List.map(|num| Str.to_u64(num) ?? crash "Invalid count")

    { width, height, shapes }

parseRegions : Str -> List Region
parseRegions = |input|
    input
    |> Str.split_on("\n")
    |> List.map(parseRegion)

parse : Str -> (List U64, List Region)
parse = |input|
    trimmed = Str.trim(input)
    { before: tiles, after: regions } = Str.split_last(trimmed, "\n\n") ?? crash "Invalid input"

    (
        parseTiles(tiles),
        parseRegions(regions),
    )

# Guess from looking at the input: Several of the groups have a total area that is smaller than
# the raw space taken up by the listed tiles (even assuming the tiles fit perfectly), so they
# trivially can't fit
areaPossible : Region, List U64 -> Bool
areaPossible = |region, tiles|
    totalArea =
        List.map2(region.shapes, tiles, |count, size| count * size)
        |> List.sum

    available = region.height * region.width

    totalArea <= available
