import Distribution.Simple
import System.Cmd
import Control.Monad

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{
    preConf = \args confFlags -> do
      buildParser
      preConf simpleUserHooks args confFlags,
    postInst = \args flags descr buildInfo -> do
      postInst simpleUserHooks args flags descr buildInfo
      system "IHaskell setup"
      system "cp ./images/ihaskell-logo.png `ipython locate profile haskell`/static/base/images/ipynblogo.png"
      return ()
}

buildParser = system "./build-parser.sh"
