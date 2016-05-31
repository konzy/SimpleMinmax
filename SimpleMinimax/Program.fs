
type territory = 
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

let territoriesHaveOpposingArmies(terr1: territory, terr2: territory) =
    if terr1.Player = terr2.Player then
        false
    else
        terr1.Connections
        |> Seq.contains terr2.ID

let attacksFromHere(fromTerr: territory) =
    fromTerr.Connections
    |> List.map(fun toIndex -> if territoriesHaveOpposingArmies(fromTerr, board.Item(toIndex)) && fromTerr.Armies > board.Item(toIndex).Armies then attackPair(fromTerr, board.Item(toIndex)) |> ignore)//type mismatch
    
let validAttacks = 
    board 
    |> Seq.filter(fun terr -> terr.Player = currentPlayer)
    |> Seq.map(attacksFromHere)













[<EntryPoint>]
let main argv = 
    validAttacks
    |> Seq.iter(fun x -> printfn "%A" x)
    0 // return an integer exit code
