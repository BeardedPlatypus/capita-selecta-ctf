module GalacticWar.PlayerInteraction exposing ( .. )


import String


type Action = Respawn
            | ChallengePlayer Int
            | ChallengeNode   Int
            | NoAction


actionToString : Action -> String
actionToString action =
  case action of
    Respawn                   -> "Respawn"
    ChallengePlayer player_id -> "Challenge Player " ++ toString(player_id)
    ChallengeNode   node_id   -> "Challenge Node " ++ toString(node_id)
    NoAction                  -> "No Action"


resultToAction : String -> Action
resultToAction result =
  if result == "respawn" then
    Respawn
  else if String.startsWith "player" result then
    case ( String.toInt ( String.right 1 result )) of
      Result.Ok val -> ChallengePlayer val
      _             -> NoAction
  else if String.startsWith "node" result then
    case ( String.toInt ( String.right 2 result )) of
      Result.Ok val -> ChallengeNode val
      _             -> NoAction
  else
    NoAction
