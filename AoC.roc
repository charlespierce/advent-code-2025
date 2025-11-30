module [solve!]

import pf.Dir
import pf.File
import pf.Http
import pf.Stdout
import pf.Utc

Solution ans1 ans2 : {
    day : U8,
    part1 : Str -> ans1,
    part2 : Str -> ans2,
} where ans1 implements Inspect, ans2 implements Inspect

solve! : U16, Solution _ _ => Result {} _
solve! = |year, solution|
    input = getInput!({ year, day: solution.day })?

    startPart1 = Utc.now!({})
    part1 = solution.part1(input)
    endPart1 = Utc.now!({})
    part2 = solution.part2(input)
    endPart2 = Utc.now!({})

    part1Duration = formatDuration(startPart1, endPart1)
    part2Duration = formatDuration(endPart1, endPart2)

    Stdout.line!("AOC ${Num.to_str(year)} - Day ${Num.to_str(solution.day)}")?
    Stdout.line!("        Part 1 : ${Inspect.to_str(part1)}")?
    Stdout.line!("        runtime: ${part1Duration}")?
    Stdout.line!("")?
    Stdout.line!("        Part 2 : ${Inspect.to_str(part2)}")?
    Stdout.line!("        runtime: ${part2Duration}")

formatDuration : Utc.Utc, Utc.Utc -> Str
formatDuration = |start, end|
    nanos = deltaAsNanos(start, end)

    if nanos > 1_000_000 then
        millis = Num.div_ceil(nanos, 1_000_000)
        "${Num.to_str(millis)}ms"
    else if nanos > 1_000 then
        micros = Num.div_ceil(nanos, 1_000)
        "${Num.to_str(micros)}μs"
    else
        "${Num.to_str(nanos)}ns"

deltaAsNanos : Utc.Utc, Utc.Utc -> U64
deltaAsNanos = |start, end|
    startNanos = Num.to_u64(Utc.to_nanos_since_epoch(start))
    endNanos = Num.to_u64(Utc.to_nanos_since_epoch(end))

    Num.abs_diff(startNanos, endNanos)

Puzzle : { year : U16, day : U8 }

getInput! : Puzzle => Result Str _
getInput! = |puzzle|
    when readInputCache!(puzzle) is
        Ok(input) -> Ok(input)
        Err(_) ->
            input = fetchInput!(puzzle)?
            writeInputCache!(puzzle, input)?
            Ok(input)

inputCacheDir : Puzzle -> Str
inputCacheDir = |{ year }| "input/${Num.to_str(year)}"

inputCacheFile : Puzzle -> Str
inputCacheFile = |puzzle| "${inputCacheDir(puzzle)}/${Num.to_str(puzzle.day)}.txt"

readInputCache! : Puzzle => Result Str _
readInputCache! = |puzzle| File.read_utf8!(inputCacheFile(puzzle))

writeInputCache! : Puzzle, Str => Result {} _
writeInputCache! = |puzzle, input|
    Dir.create_all!(inputCacheDir(puzzle))?

    File.write_utf8!(input, inputCacheFile(puzzle))

fetchInput! : Puzzle => Result Str _
fetchInput! = |puzzle|
    response = Http.send!(prepareInputRequest!(puzzle)?)?

    Str.from_utf8(response.body)

prepareInputRequest! : Puzzle => Result Http.Request _
prepareInputRequest! = |puzzle|
    sessionCookie = File.read_utf8!("credential.txt")?
    uri = "https://adventofcode.com/${Num.to_str(puzzle.year)}/day/${Num.to_str(puzzle.day)}/input"
    headers = [
        Http.header(("cookie", "session=${sessionCookie}")),
        Http.header(("user-agent", "github.com/charlespierce/advent-code-2025 by chuck@charlespierce.dev")),
    ]

    Ok({ Http.default_request & uri, headers })
