import Parser
import StackMachine

main = do
    line <- getLine
    let parsed = parse line
        translated = translate parsed
    putStrLn translated
