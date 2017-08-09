
import Control.Concurrent.FreeActor

import Control.Monad (forever)

printActor :: Actor String ()
printActor = forever $ do
    str <- receive
    embedIO $ putStrLn str

main :: IO ()
main = start $ do
    printer <- spawn printActor
    send printer "Hello World!"
