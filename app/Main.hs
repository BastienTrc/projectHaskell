module Main where

import Data.Aeson
import Web.Scotty
import GHC.Base (build)
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import qualified Data.ByteString.Char8 as Bs
import Control.Monad.IO.Class
import System.Environment
import Data.Maybe (fromMaybe)
import System.Exit
import qualified Data.Text as Text
import GHC.Generics
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status200, status400)
import Data.Text.Lazy.Encoding (decodeUtf8)
main :: IO ()
main = do
    let port = 3000 :: Int -- Lire le numéro de port depuis les arguments de ligne de commande
    scotty port $ do
      get "/ui" $ do
        file "./app/templates/index.html"
       
      post "/convert" $ do
        inputStr <- body
        let input = decodeUtf8 inputStr
        if "hello" `T.isInfixOf` input
        then do
            status status200
            let content = "<p style='text-align:center; color:green;'>Succes : 'hello' trouvé<p>"
            let toReplace = "<p id='succes'></p>"
            replaceHtml "./app/templates/index.html" toReplace content
        else do
            status status400
            let errorMsg = "<p style='text-align:center; color:red;'>Erreur : 'hello' non trouvé<p>"
            let toReplace = "<p id='error'></p>"
            replaceHtml "./app/templates/index.html" toReplace errorMsg
      
replaceHtml :: FilePath -> String -> String -> ActionM ()
replaceHtml filePath toReplace newContent = do
    fileContent <- liftIO $ TIO.readFile filePath
    let fileContentWithError = T.replace (T.pack toReplace) (T.pack newContent) fileContent
    html fileContentWithError
