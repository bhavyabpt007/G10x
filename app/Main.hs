{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -XTypeOperators #-}
module Main where

import Data.Maybe
import qualified Data.List.Split as DL
import qualified Data.Scientific as DS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.Vector as DV
import qualified Data.Text as DT
import           Types
import           Servant.API as S
import           Servant.Server
import           Data.Proxy
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    run 8080 app

-- web application setup

type ServerApi = "test" :> Get '[JSON] A.Value
            :<|> "parseJSON" :>ReqBody '[JSON] ParseJSONBody :> Post '[JSON] A.Value

testApiHandler :: Handler A.Value
testApiHandler = pure (A.String (DT.pack ("TEST ROUTE") ))

parseJsonHandler :: ParseJSONBody -> Handler A.Value
parseJsonHandler req = pure (parseJSON (encodedVal req))

server :: Server ServerApi
server = testApiHandler
    :<|> parseJsonHandler

serverApi :: Proxy ServerApi
serverApi = Proxy

app :: Application
app = serve serverApi server

--  top level handler
parseJSON:: String -> A.Value
parseJSON customEncodedVal = do
    let keyValList = filter (/="") (DL.splitOn "#" customEncodedVal)
        accJSON  = (AKM.empty :: (AKM.KeyMap A.Value))
        json = foldl  valueParser accJSON keyValList
    A.Object json
-- mid level parser a.k.a key value parsers
-- accepts value for a & b and calls terminal parsers
-- type agnosttic conttroller function
-- a = 1 -> the type is a comma separated array
-- a = 0 -> single type
-- b = 0 -> Date
-- b = 1 -> Number
-- b = 2 -> String
-- b = 3 -> Bool
valueParser :: AKM.KeyMap A.Value -> String -> AKM.KeyMap A.Value
valueParser keymap keyAndValue = do
    let a = take 1 keyAndValue
        b = take 1 (drop 1 keyAndValue)
        keyValList = DL.splitOn "|" (drop 2 keyAndValue)
        key = (fromMaybe "" (safeHead(take 1 keyValList)))
        encval = (fromMaybe "" (safeHead(take 1 ((drop 1 keyValList)))))
        val = case (a,b) of 
                (("1"), ("1")) -> (A.Array  (valueParserNumber a encval))
                (("1"), ("3")) -> (A.Array  (valueParserBool a encval))
                (("1"), _)     -> (A.Array  (valueParserString a encval))
                (("0"),("1"))  -> (A.Number (read encval))
                (("0"),("3"))  -> (A.Bool   (parseBool encval))
                (("0"),_)      -> (A.String (DT.pack encval))
                (_,_)          -> (A.String (DT.pack encval))
    AKM.insert (AK.fromString key) val keymap

-- type specific functions
valueParserString :: String -> String -> DV.Vector A.Value
valueParserString ("1") val = DV.fromList (map A.String (map DT.pack (arrayParseHelper val)))
valueParserString _     val = DV.fromList (map A.String (map DT.pack [val]))

valueParserNumber :: String -> String -> DV.Vector A.Value
valueParserNumber ("1") val = DV.fromList (map A.Number (arrayParseHelperNumber val))
valueParserNumber  _    val = DV.fromList (map A.Number [(read val)])

valueParserBool :: String -> String -> DV.Vector A.Value
valueParserBool ("1") val = DV.fromList (map A.Bool (arrayParseHelperBool val))
valueParserBool _     val = DV.fromList (map A.Bool [(parseBool val)])


-- terminal parsers
-- accepts the terminal values as strings and converts them to required type
arrayParseHelper :: String -> [String]
arrayParseHelper value = (DL.splitOn "," value)

-- in case of empty strig it will throw an error
-- what is expected behaviour in that case default value or error ?
arrayParseHelperNumber :: String -> [DS.Scientific]
arrayParseHelperNumber value = map read (DL.splitOn "," value)

arrayParseHelperBool :: String -> [Bool]
arrayParseHelperBool value = map parseBool (DL.splitOn "," value)

parseBool :: String -> Bool
parseBool ("n") = False
parseBool ("N") = False
parseBool ("f") = False
parseBool ("F") = False
parseBool ("y") = True
parseBool ("Y") = True
parseBool ("t") = True
parseBool ("T") = True
parseBool _     = False

safeHead :: [String] -> Maybe String
safeHead (x:_) = Just x
safeHead _     = Nothing

input :: String 
input = "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"