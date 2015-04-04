{-# LANGUAGE OverloadedStrings #-}
import Data.Hash.MD5
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Web.Scotty
import Network.HTTP.Types
import Control.Monad.Trans (liftIO)
import qualified Data.Text.Lazy.Encoding as TL
import Data.Tuple (swap)
import Data.List (unfoldr)

shorten :: String -> Int -> String
shorten = shorten' (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
 
shorten' :: String -> String -> Int -> String
shorten' charset url len = toCharset charset (convertToBase urlhash ((fromIntegral . length) charset))
  where
    urlhash = md5i (Str url) `mod` (fromIntegral (length charset) ^ len)

    toCharset :: Integral a => String -> [a] -> String
    toCharset ch = map ((ch!!) . fromIntegral)

    convertToBase :: Integral a => a -> a -> [a]
    convertToBase n b = unfoldr (tomb . swap . (`divMod` b)) n
      where tomb x@(0, 0) = Nothing
            tomb x = Just x
    -- convertToBase n b = map snd $ takeWhile (\(a, b) -> a /= 0 || b /= 0) h
    --   where h = divMod n b : map (\(a, _) -> divMod a b) h


addUrl :: String -> IO (Maybe String)
addUrl url = do conn <- R.connect R.defaultConnectInfo
                R.runRedis conn $ do
                  u <- R.get (B.pack shortUrl)
                  case u of
                    Right Nothing -> do R.set (B.pack shortUrl) (B.pack url)
                                        return (Just shortUrl)
                    Right (Just a) -> if a == B.pack url
                                       then return (Just shortUrl)
                                       else return Nothing
                    otherwise -> return Nothing
  where shortUrl = shorten url 3
        
runServer :: IO ()
runServer = scotty 3000 $ do
  get "/:short" $ do
    short <- param "short"
    con <- liftIO $ R.connect R.defaultConnectInfo
    u <- liftIO $ R.runRedis con (R.get short)
    case u of
      Right (Just url) -> redirect (TL.decodeUtf8 (BL.fromStrict url))
      otherwise -> do liftIO $ putStrLn "not found"
                      status status404
                      html "404 - not found"
