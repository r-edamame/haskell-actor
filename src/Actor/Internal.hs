
{-# LANGUAGE ExistentialQuantification #-}

module Actor.Internal where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan(..), newChan, writeChan, readChan)

import Control.Monad.Free

-- alias Chan to MailBox
newtype MailBox a = MailBox (Chan a)

sendMail :: MailBox a -> a -> IO ()
sendMail (MailBox c) v = writeChan c v
readMail :: MailBox a -> IO a
readMail (MailBox c) = readChan c
newMailBox :: IO (MailBox a)
newMailBox = MailBox <$> newChan

-- Actor DSL Construction
data Actor' rec a
  = forall snd. Send (MailBox snd) snd a
  | Receive (rec -> a)
  | Self (MailBox rec -> a)
  | EmbedIO (IO a)

instance Functor (Actor' rec) where
  fmap f (Send mb v a) = Send mb v (f a)
  fmap f (Receive g) = Receive (f . g)
  fmap f (Self g) = Self (f . g)
  fmap f (EmbedIO m) = EmbedIO (f <$> m)

type Actor rec a = Free (Actor' rec) a


-- Actor DSL methods
send :: MailBox snd -> snd -> Actor rec ()
send mb v = Free $ Send mb v (Pure ())

receive :: Actor rec rec
receive = Free $ Receive Pure

self :: Actor rec (MailBox rec)
self = Free $ Self Pure

embedIO :: IO a -> Actor rec a
embedIO m = Free $ EmbedIO $ return <$> m

sendWithSelf :: MailBox b -> (MailBox rec -> b) -> Actor rec ()
sendWithSelf dest trans = trans <$> self >>= send dest


-- translate Actor DSL to IO
start :: Actor rec () -> IO ()
start actor = do
  mb <- newMailBox
  runActor mb actor

-- make Actor 
spawn :: Actor rec' () -> Actor rec (MailBox rec')
spawn actor = do
    mb <- embedIO newMailBox
    embedIO $ forkIO (runActor mb actor)
    return mb

-- interpret Actor DSL with MailBox
runActor :: MailBox rec -> Actor rec a -> IO a
runActor mb (Pure a) = return a
runActor mb (Free (Send b v a)) = sendMail b v >> runActor mb a
runActor mb (Free (Receive f)) = readMail mb >>= runActor mb . f
runActor mb (Free (Self f)) = runActor mb (f mb)
runActor mb (Free (EmbedIO m)) = m >>= runActor mb

