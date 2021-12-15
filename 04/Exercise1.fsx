#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

module String =
    let split (separator: char) (str: string) =
        str.Split [|separator|]
        |> Array.toList

type Board = int list list

type BingoState = {
    Numbers: int list;
    CalledNumbers: int list;
    Boards: Board list;
}

let parseBoardLine (line: string) =
    line
    |> String.split ' '
    |> List.choose (fun str ->
        match str with
        | "" -> None
        | num -> Some (int num))

let parseBingoInput lines =
    let numbers =
        lines
        |> List.head
        |> String.split ','
        |> List.map int
    let boards =
        lines
        |> List.tail
        |> List.filter (fun l -> l <> "")
        |> List.map parseBoardLine
        |> List.windowed 5 // TODO - this doesn't actually split the list
    { Numbers = numbers; Boards = boards; CalledNumbers = [] }

let isWinningBoard (calledNumbers: int list) (board: Board) =
    let completedSets (sets: int list list) =
        sets
        |> List.map (fun set -> set |> List.except calledNumbers) 
        |> List.filter (fun remainingNumbers -> remainingNumbers.Length = 0)

    let rows = board
    let winningHorizontalPositions = rows |> completedSets

    // [[ a b c d e ]
    //  [ f g h i j ]
    //  [ k l m n o ]
    //  [ p q r s t ]
    //  [ u v w x y ]] -->
    //                    |
    // [[ a f k p u ]  <--
    //  [ b g l q v ]
    //  [ c h m r w ]
    //  [ d i n s x ]
    //  [ e j o t y ]]
    let columns = seq {
        for column in 0 .. 4 do
            board
            |> Seq.map (fun row -> row.[column])
            |> Seq.toList
    }
    let winningVerticalPositions = columns |> Seq.toList |> completedSets
    winningHorizontalPositions.Length > 0 || winningVerticalPositions.Length > 0

let getWinningBoards (calledNumbers: int list) (candidateBoards: Board list) =
    candidateBoards |> List.filter (isWinningBoard calledNumbers)

let callNextNumber (state: BingoState) =
    match state.Numbers with
    | nextNumber::remainingNumbers ->
        let calledNumbers = nextNumber::state.CalledNumbers
        let updatedState = { state with Numbers = remainingNumbers; CalledNumbers = calledNumbers }
        Some updatedState
    | [] ->
        None

let startGame (initialState: BingoState) =
    let rec play (state: BingoState) =
        callNextNumber state
        |> Option.bind(fun newState ->
            let winningBoards = getWinningBoards newState.CalledNumbers newState.Boards
            if winningBoards.Length > 0 then
                Some (newState, winningBoards)
            else
                play newState)

    play initialState

let lastNumberCalled (state: BingoState) =
    List.head state.CalledNumbers

let calculateScore finalNumber (winningBoards: Board list) =
    0 // TODO

let solve () =
    let input = readInput "input.txt"
    let initialState = input |> Seq.toList |> parseBingoInput
    let result = startGame initialState
    match result with
    | Some (finalState, winningBoards) ->
        calculateScore (lastNumberCalled finalState) winningBoards
    | None ->
        0

let solution = solve ()
printfn "%d" solution
