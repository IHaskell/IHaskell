import Distribution.Simple
import System.Cmd

main = defaultMainWithHooks simpleUserHooks{
    preConf = \args confFlags -> do
      system "./build-parser.sh"
      preConf simpleUserHooks args confFlags
}
