let readInput filename =
    System.IO.File.ReadAllLines(filename) |> Array.toSeq