import Syntax
import Analyser (isValid)
import Parser (parseRust)
import Typer (typeRust)
import Control.Applicative ((<$>))

main = do
    rust <- parseRust <$> getContents
    let output = case rust of
                    Left  e -> show e
                    Right c -> handleAnalysis c
    putStrLn output

handleAnalysis c =
    case isValid c of
        Left e  -> "Error: " ++ e
        Right a -> typeRust c
