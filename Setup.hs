import Distribution.Simple
import System.Cmd
import Control.Monad

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{
    preConf = \args confFlags -> do
      buildParser
      preConf simpleUserHooks args confFlags
}

buildParser = system "./build-parser.sh"
