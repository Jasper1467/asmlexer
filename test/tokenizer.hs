import Tokenizer

main :: IO ()
main = do
    let code = "mov R1, #10"
    let tokens = tokenize code
    print tokens

main