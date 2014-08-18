import Mouse
import WebSocket

main : Signal Element
main = lift (flow down) (foldp step [(asText "connecting ...")] socket)

step : String -> [Element] -> [Element]
step a b = (b ++ [asText a])

socket = WebSocket.connect "ws://127.0.0.1:3000" (constant "no name")