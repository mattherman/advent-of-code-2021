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
        |> List.windowed 5
    { Numbers = numbers; Boards = boards; CalledNumbers = [] }

let isWinningBoard (calledNumbers: int list) (board: Board) =
    false // TODO

let getWinningBoards (calledNumbers: int list) (candidateBoards: Board list) =
    candidateBoards |> List.filter (isWinningBoard calledNumbers)

let callNextNumber (state: BingoState) =
    match state.Numbers with
    | nextNumber::remainingNumbers ->
        let calledNumbers = nextNumber::state.CalledNumbers
        let updatedState = { state with Numbers = remainingNumbers; CalledNumbers = calledNumbers }
        Some (nextNumber, updatedState)
    | [] ->
        None

let startGame (initialState: BingoState) =
    let rec play (state: BingoState) =
        callNextNumber state
        |> Option.bind(fun (calledNumber, newState) ->
            let winningBoards = getWinningBoards newState.CalledNumbers newState.Boards
            if winningBoards.Length > 0 then
                Some (calledNumber, winningBoards)
            else
                play newState)

    play initialState

let calculateScore finalNumber (winningBoards: Board list) =
    0 // TODO

let solve () =
    let input = readInput "input.txt"
    let initialState = input |> Seq.toList |> parseBingoInput
    let result = startGame initialState
    match result with
    | Some (finalNumber, winningBoards) ->
        calculateScore finalNumber winningBoards
    | None ->
        0

let solution = solve ()
printfn "%d" solution
