import Parser
import Control.Applicative ((<$>))

main = do
    output <- parseRust <$> getContents
    putStrLn $ pretty output

pretty (Left e) = show e
pretty (Right s) = show s
