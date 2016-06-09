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

    //node has value, (player), game state, available attacks
type node =
    struct
        val PlayersTurn: int
        val mutable Value: int
        val CurrentBoard: List<territory>
        val AvailableAttacks: List<attackPair>
        val mutable resultingStates: List<node>
        new(playersTurn: int, currentBoard: List<territory>, availableAttacks: List<attackPair>) = 
            { PlayersTurn = playersTurn; CurrentBoard = currentBoard; AvailableAttacks = availableAttacks; Value = 0; resultingStates = []}
    end

let board = [
    new territory(0, 0, 1, [1; 4]); new territory(1, 0, 10, [0; 5; 2]); new territory(2, 1, 5, [1; 6; 3]); new territory(3, 1, 1, [2; 7]); 
    new territory(4, 0, 1, [0; 5]); new territory(5, 0, 10, [4; 1; 6]); new territory(6, 1, 5, [5; 2; 7]); new territory(7, 1, 1, [6; 3]);]

let currentPlayer = 0
let numberOfPlayers = 2
let passTerritory = new territory(-1, -1, -1, [])
let passAttackPair = new attackPair(passTerritory, passTerritory)
let passAttackPairSeq = seq<attackPair> [passAttackPair]
//initalization done

let createAttackPair(terr: territory, x: int) =
    if terr.Player <> board.Item(x).Player && terr.Armies > board.Item(x).Armies 
    then [attackPair(terr, board.Item(x))]
    else []

let attacksFromHere(terr: territory) =
    terr.Connections
    |> Seq.collect(fun x -> createAttackPair(terr, x))
    

let validAttacks(player: int) = 
    board 
    |> List.filter(fun terr -> terr.Player = player)
    |> List.map(attacksFromHere)
    |> List.append([passAttackPairSeq])

let switchPlayers =
    currentPlayer = (currentPlayer + 1) % numberOfPlayers

let doAllTerritoritiesMatchCurrentPlayer =
    board
    |> Seq.forall(fun x -> x.Player = currentPlayer)

let haveOnlyPassingMoves =
    [0..numberOfPlayers] 
    |> Seq.forall(fun x -> validAttacks(x).Length = 1)

let hasCurrentPlayerWon = 
    doAllTerritoritiesMatchCurrentPlayer || haveOnlyPassingMoves

let resultOfAttackOnThisTerritory(terr: territory, attack: attackPair) =
    if terr.ID = attack.FromTerritory.ID && attack <> passAttackPair then
        new territory(terr.ID, attack.FromTerritory.Player, 1, terr.Connections)
    elif terr.ID = attack.ToTerritory.ID && attack <> passAttackPair then
        new territory(terr.ID, attack.FromTerritory.Player, attack.FromTerritory.Armies - attack.ToTerritory.Armies - 1, terr.Connections)
    else
        terr

let executeAttack(currentBoard: List<territory>, attack: attackPair) =
    currentBoard
    |> Seq.map(fun x -> resultOfAttackOnThisTerritory(x, attack))




[<EntryPoint>]
let main argv = 
    validAttacks(currentPlayer)
    |> Seq.iter(fun x -> x |> Seq.iter( fun y -> printfn "%A" y.FromTerritory.ID))
    0 // return an integer exit code