module SideEffects where
import Prelude hiding (readFile, writeFile, print)

data RealWorld = RealWorld

internalReadFile :: FilePath -> RealWorld -> (RealWorld, String)
internalReadFile = undefined

internalWriteFile :: FilePath -> String -> RealWorld -> RealWorld
internalWriteFile = undefined

internalPrint :: String -> RealWorld -> RealWorld
internalPrint = undefined

data SideEffect a =
  SideEffect { runSideEffects :: RealWorld -> (RealWorld, a) }

joinSideEffects :: SideEffect (SideEffect a) -> SideEffect a
joinSideEffects outerSideEffect = SideEffect $ \world ->
  let (world', innerSideEffect) = runSideEffects outerSideEffect world
  in runSideEffects innerSideEffect world'

sequenceSideEffects :: SideEffect a -> (a -> SideEffect b) -> SideEffect b
sequenceSideEffects sideEffect makeNextSideEffect =
  joinSideEffects $ SideEffect $ \world ->
    let (world', val) = runSideEffects sideEffect world
    in (world', makeNextSideEffect val)

-- sequenceSideEffects :: SideEffect a -> (a -> SideEffect b) -> SideEffect b
-- sequenceSideEffects firstEffect newEffect =
--   SideEffect $ \world ->
--     let (world', result) = runSideEffects firstEffect world
--     in runSideEffects (newEffect result) world'

readFile :: FilePath -> SideEffect String
readFile filename = SideEffect $ \realWorld ->
  let (realWorld', contents) = internalReadFile filename realWorld
  in (realWorld', contents)

writeFile :: FilePath -> String -> SideEffect ()
writeFile filename contents = SideEffect $ \realWorld ->
  let realWorld' = internalWriteFile filename contents realWorld
  in (realWorld', ())

print :: String -> SideEffect ()
print message = SideEffect $ \realWorld ->
  let realWorld' = internalPrint message realWorld
  in (realWorld', ())

writeReadFile :: SideEffect ()
writeReadFile =
  writeFile "example.txt" "Hello, Haskell"
  `sequenceSideEffects` (\_ -> readFile "example.txt")
  `sequenceSideEffects` (\contents -> print contents)
