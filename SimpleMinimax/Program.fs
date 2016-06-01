﻿type territory = 
    struct
        val ID: int
        val Connections: List<int>
        val mutable Player: int
        val mutable Armies: int
        new(id: int, player: int, armies: int, connections: List<int>) = {ID = id; Player = player; Armies = armies; Connections = connections}
    end

type attackPair =
    struct
        val FromTerritory: territory
        val ToTerritory: territory
        new(fromTerritory: territory, toTerritory: territory) = { FromTerritory = fromTerritory; ToTerritory = toTerritory} 
    end

let board = [
    new territory(0, 0, 1, [1; 4]); new territory(1, 0, 10, [0; 5; 2]); new territory(2, 1, 5, [1; 6; 3]); new territory(3, 1, 1, [2; 7]); 
    new territory(4, 0, 1, [0; 5]); new territory(5, 0, 10, [4; 1; 6]); new territory(6, 1, 5, [5; 2; 7]); new territory(7, 1, 1, [6; 3]);]

let currentPlayer = 0
//initalization done

let createAttackPair(terr: territory, x: int) =
    if terr.Player <> board.Item(x).Player && terr.Armies > board.Item(x).Armies 
    then [attackPair(terr, board.Item(x))] 
    else []

let attacksFromHere(terr: territory) =
    terr.Connections
    |> List.collect(fun x -> createAttackPair(terr, x))

let validAttacks = 
    board 
    |> Seq.filter(fun terr -> terr.Player = currentPlayer)
    |> Seq.map(attacksFromHere)













[<EntryPoint>]
let main argv = 
    validAttacks
    |> Seq.iter(fun x -> x |> Seq.iter( fun y -> printfn "%A" y.FromTerritory.ID))
    0 // return an integer exit code