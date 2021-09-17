#r "nuget: xunit"
#r "nuget: FsUnit.xUnit"
#load "Tests.fs"

open Tests

let acyclicGraph = add "C" "B" Map.empty |> add "B" "A"
let cyclicGraph = acyclicGraph |> add "A" "C"
