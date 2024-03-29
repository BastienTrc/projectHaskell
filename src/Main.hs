module Main where

import Parser
 
import Data.Aeson (eitherDecode, FromJSON, Value, encode)

import Web.Scotty
import GHC.Base (build)
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import qualified Data.ByteString.Char8 as Bs
import Control.Monad.IO.Class (liftIO)
import System.Environment
import Data.Maybe (fromMaybe)
import System.Exit
import GHC.Generics
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as ByteString -- Correction ici
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status200, status400)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (FilePath)
import GHC.Generics (Generic)
newtype Config = Config {
    portNumber :: Int
} deriving (Show, Generic)

instance FromJSON Config


readConfig :: FilePath -> IO (Either String Config)
readConfig filePath = eitherDecode <$> ByteString.readFile filePath


main :: IO ()
main = do
    configResult <- readConfig "config.json"
    case configResult of
      Left err -> putStrLn $ "Error parsing config: " ++ err
      Right config -> do
          let port = portNumber config
          scotty port $ do
              get "/ui" $ do
                file "./src/templates/index.html"
              
              post "/convert" $ do
                inputStr <- formParam "s-expression"
                let input = inputStr
                let parsedExprs = parseInput input
                case parsedExprs of
                    Right json -> do
                        status status200
                        let content = "<p style='text-align:center; color:green;'>Succès :\n" ++ (( T.unpack . decodeUtf8 . encode) json) ++ "<p>" -- remplacer par le contenu JSON
                        let toReplace = "<p id='succes'></p>"
                        replaceHtml "./src/templates/index.html" toReplace content
                        
                    Left err -> do
                        status status400
                        let errorMsg = "<p style='text-align:center; color:red;'>Erreur :"++ err ++"<p>" -- remplacer par le message d'erreur
                        let toReplace = "<p id='error'></p>"
                        replaceHtml "./src/templates/index.html" toReplace errorMsg
                        
                   
        
-- Fonction pour remplacer le contenu HTML dans un fichier donné par un nouveau contenu 
replaceHtml :: FilePath -> String -> String -> ActionM ()
replaceHtml filePath toReplace newContent = do
    fileContent <- liftIO $ TIO.readFile filePath
    let fileContentWithError = T.replace (T.pack toReplace) (T.pack newContent) fileContent
    html fileContentWithError
