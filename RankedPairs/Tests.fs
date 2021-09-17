module Tests

let inline (>>>) func1 func2 x y = func2 (func1 x y)

let pairWise vote =
    let rec addRemaingVotes' remainingVote pairWiseRankings =
        match remainingVote with
        | [] -> pairWiseRankings
        | head :: tail ->
            tail
            |> List.map (fun x -> ((head, x), 1), ((x, head), 0))
            |> List.fold (fun state (x, y) -> x :: y :: state) []
            |> List.append pairWiseRankings
            |> addRemaingVotes' tail

    addRemaingVotes' vote [] |> Map.ofList

let addOrCombine map key value =
    Map.change key ((Option.fold (+) >>> Some) value) map

let tally =
    List.map pairWise
    >> List.fold (Map.fold addOrCombine) Map.empty

let sort tally =
    tally
    |> Map.toList
    |> List.sortWith
        (fun ((x, y), vxy) ((z, w), vzw) ->
            if vxy > vzw
               || ((vxy = vzw)
                   && Map.find (w, z) tally > Map.find (y, x) tally) then
                -1
            else
                1)

let trimMinorities sortedTally =
    sortedTally
    |> List.fold
        (fun agg ((x, y), wins) ->
            Map.tryFind (y, x) agg
            |> function
                | None -> Map.add (x, y) wins agg
                | Some n ->
                    if wins > n then
                        Map.remove (y, x) agg |> Map.add (x, y) wins
                    else
                        agg)
        Map.empty
    |> Map.toList
    |> List.sortByDescending snd

let add node parent graph =
    graph
    |> Map.change
        node
        (function
        | None -> [ parent ] |> Some
        | Some oldParentValue -> parent :: oldParentValue |> Some)
    |> Map.change
        parent
        (function
        | None -> [] |> Some
        | Some alreadyExists -> Some alreadyExists)

let getParents node graph =
    graph
    |> Map.tryFind node
    |> function
        | None -> []
        | Some parents -> parents

let isCyclic graph =
    let rec isCyclic' current visited =
        let parents = getParents current graph

        if Set.contains current visited then
            true
        else if List.isEmpty parents then
            false
        else
            List.exists (fun p -> isCyclic' p (Set.add current visited)) parents

    Map.exists (fun node _ -> isCyclic' node Set.empty) graph

let lockTally tally =
    let rec lockTally' graph remaining =
        match remaining with
        | [] -> graph
        | ((winner, loser), _) :: tail ->
            let updatedGraph = add loser winner graph

            if (isCyclic updatedGraph) then
                lockTally' graph tail
            else
                lockTally' updatedGraph tail

    lockTally' Map.empty tally

let map1 =
    [ ("A", "B"), 1
      ("B", "A"), 0
      ("A", "C"), 1
      ("C", "A"), 0
      ("B", "C"), 1
      ("C", "B"), 0 ]
    |> Map.ofList

let map2 =
    [ ("B", "A"), 1
      ("A", "B"), 0
      ("B", "C"), 1
      ("C", "B"), 0
      ("A", "C"), 1
      ("C", "A"), 0 ]
    |> Map.ofList

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Convert ranked vote into pairwise map`` () =
    pairWise [ "A"; "B"; "C" ] |> should equal map1

[<Fact>]
let ``Combine 2 pairwise maps`` () =
    let expected =
        [ ("A", "B"), 1
          ("B", "A"), 1
          ("A", "C"), 2
          ("C", "A"), 0
          ("B", "C"), 2
          ("C", "B"), 0 ]
        |> Map.ofList

    Map.fold addOrCombine map1 map2
    |> should equal expected

[<Fact>]
let ``Tally a list of votes`` () =
    let expected =
        [ ("A", "B"), 1
          ("B", "A"), 1
          ("A", "C"), 2
          ("C", "A"), 0
          ("B", "C"), 2
          ("C", "B"), 0 ]
        |> Map.ofList

    tally [ [ "A"; "B"; "C" ]
            [ "B"; "A"; "C" ] ]
    |> should equal expected

[<Fact>]
let ``Sort tally by majorities`` () =
    let tally =
        [ ("A", "B"), 1
          ("B", "A"), 1
          ("A", "C"), 2
          ("C", "A"), 0
          ("B", "C"), 2
          ("C", "B"), 0 ]
        |> Map.ofList

    let expected =
        [ (("A", "C"), 2)
          (("B", "C"), 2)
          (("A", "B"), 1)
          (("B", "A"), 1)
          (("C", "A"), 0)
          (("C", "B"), 0) ]

    sort tally |> should equal expected

[<Fact>]
let ``Trim minorities`` () =
    let sortedTally =
        [ (("A", "C"), 2)
          (("B", "C"), 2)
          (("A", "B"), 1)
          (("B", "A"), 1)
          (("C", "A"), 0)
          (("C", "B"), 0) ]

    let expected =
        [ (("A", "C"), 2)
          (("B", "C"), 2)
          (("A", "B"), 1) ]

    trimMinorities sortedTally
    |> should equal expected

[<Fact>]
let ``Add arc to graph`` () =
    let expected =
        [ ("C", [ "B" ])
          ("B", [ "A" ])
          ("A", []) ]
        |> Map.ofList

    add "C" "B" Map.empty
    |> add "B" "A"
    |> should equal expected

[<Fact>]
let ``Get parents`` () =
    let graph = add "C" "B" Map.empty |> add "B" "A"

    let expected = [ "A" ]

    getParents "B" graph |> should equal expected

[<Fact>]
let ``Is cyclic`` () =
    let acyclicGraph = add "C" "B" Map.empty |> add "B" "A"
    let cyclicGraph = acyclicGraph |> add "A" "C"

    isCyclic acyclicGraph |> should be False
    isCyclic cyclicGraph |> should be True

[<Fact>]
let ``Lock`` () =
    let tally =
        [ (("Chattanooga", "Knoxville"), 83)
          (("Nashville", "Knoxville"), 68)
          (("Nashville", "Chattanooga"), 68)
          (("Nashville", "Memphis"), 58)
          (("Chattanooga", "Memphis"), 58)
          (("Knoxville", "Memphis"), 58) ]

    let expected =
        [ ("Nashville", [])
          ("Chattanooga", [ "Nashville" ])
          ("Knoxville", [ "Nashville"; "Chattanooga" ])
          ("Memphis",
           [ "Knoxville"
             "Chattanooga"
             "Nashville" ]) ]
        |> Map.ofList

    lockTally tally |> should equal expected
