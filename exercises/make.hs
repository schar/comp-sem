import System.Process
import Data.List

dir :: String
dir = "srcs"  -- The directory where you'd like
              -- your .lhs files to live.

main = do
  putStrLn "Which file to process to .lhs (ends in .md)?"
  xs <- getLine
  callCommand $
    "mkdir "++ dir ++";\
    \ pandoc "++ xs ++" -t markdown+lhs -o "++ dir ++"/"++ mdlhs xs
  putStrLn "Success!"

mdlhs :: String -> String
mdlhs xs
  | revTails!!3 == ".md" = revInits!!3 ++".lhs"
  | True = error "Not an .md file!"
    where revTails = reverse . tails $ xs
          revInits = reverse . inits $ xs
