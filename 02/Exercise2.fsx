#!/usr/bin/dotnet fsi

#load "../Common.fsx"
open Common

module Seq =
    let chooseResult (chooser: 'a -> Result<'b, 'c>) sequence =
        sequence
        |> Seq.choose (fun result ->
            match chooser result with
            | Ok value -> Some value
            | Error _ -> None)

type SubmarinePosition = {
    Aim: int;
    Horizontal: int;
    Depth: int;
}

type Command =
    | Forward of int
    | AimDown of int
    | AimUp of int

let applyCommand (command: Command) (position: SubmarinePosition) =
    match command with
    | Forward distance ->
        let newHorizontal = position.Horizontal + distance
        let newDepth = position.Depth + (distance * position.Aim)
        { position with Horizontal = newHorizontal; Depth = newDepth }
    | AimDown value ->
        { position with Aim = position.Aim + value }
    | AimUp value ->
        { position with Aim = position.Aim - value }

let rec followCourse (position: SubmarinePosition) (commands: Command list) =
    match commands with
    | [] -> position
    | current::remaining ->
        let newPosition = position |> applyCommand current
        followCourse newPosition remaining

let parseCommand (text: string) =
    let parts = text.Split([| ' ' |])
    match parts with
    | [| name; value |] ->
        match name with
        | "forward" -> Ok (Forward (int value))
        | "down"    -> Ok (AimDown (int value))
        | "up"      -> Ok (AimUp (int value))
        | _         -> Error "Command name must match 'forward', 'down', or 'up'"
    | [||] -> Error "Empty line"
    | _ -> Error "Expected format to match '<name> <value>"

let solve () =
    let commands =
        readInput "input.txt"
        |> Seq.chooseResult parseCommand 
        |> Seq.toList

    let initialPosition = {
        Horizontal = 0
        Depth = 0
        Aim = 0
    }
    let finalPosition = followCourse initialPosition commands
    finalPosition.Horizontal * finalPosition.Depth

let solution = solve ()
printfn "%d" solution