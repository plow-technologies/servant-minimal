{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes           #-}


module Main where

import           Data.Maybe                  (fromMaybe)
import           Servant
import           Text.Read
import           Network.Wai.Handler.Warp              (run, defaultSettings, Settings, setHost, setPort, HostPreference)
import           Network.Wai.Middleware.RequestLogger


main :: IO ()
main = run 3000 $ logStdoutDev $ serve (Proxy :: Proxy SmallApi) smallServer






-- | 20 dummy routes
type SmallApi =
                Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int
           :<|> Post '[JSON] Int


-- | 20 dummy routes
smallServer :: Server SmallApi
smallServer =
              (return 1   :: Handler Int)
         :<|> (return 2   :: Handler Int)
         :<|> (return 3   :: Handler Int)
         :<|> (return 4   :: Handler Int)
         :<|> (return 5   :: Handler Int)
         :<|> (return 6   :: Handler Int)
         :<|> (return 7   :: Handler Int)
         :<|> (return 8   :: Handler Int)
         :<|> (return 9   :: Handler Int)
         :<|> (return 10   :: Handler Int)
         :<|> (return 11   :: Handler Int)
         :<|> (return 12   :: Handler Int)
         :<|> (return 13   :: Handler Int)
         :<|> (return 14   :: Handler Int)
         :<|> (return 15   :: Handler Int)
         :<|> (return 16   :: Handler Int)
         :<|> (return 17   :: Handler Int)
         :<|> (return 18   :: Handler Int)
         :<|> (return 19   :: Handler Int)
         :<|> (return 20   :: Handler Int)
         :<|> (return 21   :: Handler Int)
         :<|> (return 22   :: Handler Int)
         :<|> (return 23   :: Handler Int)
         :<|> (return 24   :: Handler Int)
         :<|> (return 25   :: Handler Int)
         :<|> (return 26   :: Handler Int)
         :<|> (return 27   :: Handler Int)
         :<|> (return 28   :: Handler Int)
         :<|> (return 29   :: Handler Int)
         :<|> (return 30   :: Handler Int)
         :<|> (return 31   :: Handler Int)
         :<|> (return 32   :: Handler Int)
         :<|> (return 33   :: Handler Int)
         :<|> (return 34   :: Handler Int)
         :<|> (return 35   :: Handler Int)
         :<|> (return 36   :: Handler Int)
         :<|> (return 37   :: Handler Int)
         :<|> (return 38   :: Handler Int)
         :<|> (return 39   :: Handler Int)
         :<|> (return 40   :: Handler Int)
         :<|> (return 41   :: Handler Int)
         :<|> (return 42   :: Handler Int)
         :<|> (return 43   :: Handler Int)
         :<|> (return 44   :: Handler Int)
         :<|> (return 45   :: Handler Int)
         :<|> (return 46   :: Handler Int)
         :<|> (return 47   :: Handler Int)
         :<|> (return 48   :: Handler Int)
         :<|> (return 49   :: Handler Int)
         :<|> (return 50   :: Handler Int)
         :<|> (return 51   :: Handler Int)
         :<|> (return 52   :: Handler Int)
         :<|> (return 53   :: Handler Int)
         :<|> (return 54   :: Handler Int)
         :<|> (return 55   :: Handler Int)
         :<|> (return 56   :: Handler Int)
         :<|> (return 57   :: Handler Int)
         :<|> (return 58   :: Handler Int)
         :<|> (return 59   :: Handler Int)
         :<|> (return 60   :: Handler Int)
         :<|> (return 61   :: Handler Int)
         :<|> (return 62   :: Handler Int)
         :<|> (return 63   :: Handler Int)
         :<|> (return 64   :: Handler Int)
         :<|> (return 65   :: Handler Int)
         :<|> (return 66   :: Handler Int)
         :<|> (return 67   :: Handler Int)
         :<|> (return 68   :: Handler Int)
         :<|> (return 69   :: Handler Int)
         :<|> (return 70   :: Handler Int)
         :<|> (return 71   :: Handler Int)
         :<|> (return 72   :: Handler Int)
         :<|> (return 73   :: Handler Int)
         :<|> (return 74   :: Handler Int)
         :<|> (return 75   :: Handler Int)
         :<|> (return 76   :: Handler Int)
         :<|> (return 77   :: Handler Int)
         :<|> (return 78   :: Handler Int)
         :<|> (return 79   :: Handler Int)
         :<|> (return 80   :: Handler Int)
         :<|> (return 81   :: Handler Int)
         :<|> (return 82   :: Handler Int)
         :<|> (return 83   :: Handler Int)
         :<|> (return 84   :: Handler Int)
         :<|> (return 85   :: Handler Int)
         :<|> (return 86   :: Handler Int)
         :<|> (return 87   :: Handler Int)
         :<|> (return 88   :: Handler Int)
         :<|> (return 89   :: Handler Int)
         :<|> (return 90   :: Handler Int)

