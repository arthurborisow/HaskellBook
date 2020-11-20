{-# LANGUAGE OverloadedStrings #-}

module Chapter26.Scotty where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Control.Monad.IO.Class (MonadIO(liftIO))

data Config = Config {
-- that's one, one click!
-- two...two clicks!
-- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case M.lookup k m of
                    Just a  -> (M.adjust (+1) k m, a + 1)
                    Nothing -> (M.insert k 1 m, 1)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    pref <- lift $ asks prefix
    counts' <- lift $ asks counts


    let key' = mappend pref unprefixed
    (updatedMap, newInteger) <- liftIO $ bumpBoomp key' <$> readIORef counts'
    liftIO $ writeIORef counts' updatedMap

    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty

  let config = Config { counts = counter, prefix = TL.pack prefixArg }
      runR = flip runReaderT config

  scottyT 3000 runR app
