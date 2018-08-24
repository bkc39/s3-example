{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Network.AWS.Data
import Network.AWS.S3
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.AWS
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

getFile r b k f = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r
    runResourceT . runAWST env $ do
        rs <- send (getObject b k)
        view gorsBody rs `sinkBody` CB.sinkFile f
        say $ "Successfully Download: "
          <> toText b <> " - " <> toText k <> " to " <> toText f


say :: MonadIO m => Text -> m ()
say =
  liftIO . Text.putStrLn

someFunc :: IO ()
someFunc =
  getFile NorthVirginia (BucketName "haskell-aws") (ObjectKey "cloud-remote.pdf") "test.pdf"
