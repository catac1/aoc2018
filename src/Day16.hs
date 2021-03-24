module Day16
  ( test
  , part1
  , part2
  , part2'
  ) where


import Lib
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Bits
import Data.Set qualified as S
import Data.Set (Set)
import Data.Map qualified as M
import Data.Map (Map)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class


data Opcode = O
  { opIns :: Int
  , opLhs :: Int
  , opRhs :: Int
  , opOut :: Int
  } -- deriving stock Show

instance Show Opcode where
  show (O ins lhs rhs out) = printf "O(%2d %2d %2d %2d)" ins lhs rhs out


data Mode = Imm | Rel deriving stock Show

data Func = F
  { fFunc     :: Vector Int -> Vector Int
  , fName     :: String
  }

instance Show Func where
  show (F _ n) = printf "%s" n


instance Eq Func where
  (F _ n1) == (F _ n2) = n1 == n2


instance Ord Func where
  compare (F _ n1) (F _ n2) = compare n1 n2


opCFuncMap :: String -> Opcode -> Func
opCFuncMap s = case s of
  "addi" -> addi
  "addr" -> addr
  "muli" -> muli
  "mulr" -> mulr
  "bani" -> bani
  "banr" -> banr
  "bori" -> bori
  "borr" -> borr
  "gtri" -> gtri
  "gtir" -> gtir
  "gtrr" -> gtrr
  "eqri" -> eqri
  "eqir" -> eqir
  "eqrr" -> eqrr
  "seti" -> seti
  "setr" -> setr
  _      -> error "Unknown func"


opCFuncs :: [Opcode -> Func]
opCFuncs = [ addi, addr, muli, mulr, bani, banr, bori, borr
           , gtri, gtir, gtrr, eqri, eqir, eqrr, seti, setr ]


addi :: Opcode -> Func
addi opCode = F (twoOperandF opCode (+) Rel Imm) "addi"


addr :: Opcode -> Func
addr opCode = F (twoOperandF opCode (+) Rel Rel) "addr"


muli :: Opcode -> Func
muli opCode = F (twoOperandF opCode (*) Rel Imm) "muli"


mulr :: Opcode -> Func
mulr opCode = F (twoOperandF opCode (*) Rel Rel) "mulr"


bani :: Opcode -> Func
bani opCode = F (twoOperandF opCode (.&.) Rel Imm) "bani"


banr :: Opcode -> Func
banr opCode = F (twoOperandF opCode (.&.) Rel Rel) "banr"


bori :: Opcode -> Func
bori opCode = F (twoOperandF opCode (.|.) Rel Imm) "bori"


borr :: Opcode -> Func
borr opCode = F (twoOperandF opCode (.|.) Rel Rel) "borr"


gtF :: Int -> Int -> Int
gtF a b = if a > b then 1 else 0


gtir :: Opcode -> Func
gtir opCode = F (twoOperandF opCode gtF Imm Rel) "gtir"


gtri :: Opcode -> Func
gtri opCode = F (twoOperandF opCode gtF Rel Imm) "gtri"


gtrr :: Opcode -> Func
gtrr opCode = F (twoOperandF opCode gtF Rel Rel) "gtrr"


eqF :: Int -> Int -> Int
eqF a b = if a == b then 1 else 0


eqir :: Opcode -> Func
eqir opCode = F (twoOperandF opCode eqF Imm Rel) "eqir"


eqri :: Opcode -> Func
eqri opCode = F (twoOperandF opCode eqF Rel Imm) "eqri"


eqrr :: Opcode -> Func
eqrr opCode = F (twoOperandF opCode eqF Rel Rel) "eqrr"


seti :: Opcode -> Func
seti opCode = F (oneOperandF opCode Imm) "seti"


setr :: Opcode -> Func
setr opCode = F (oneOperandF opCode Rel) "setr"


testOp :: IO ()
testOp = do
  let opCode = O 2 2 3 2
      f      = fFunc $ addi opCode
      v      = V.fromList [0, 0, 0, 0]
      v'     = f v
  printf "%s %s -> %s\n" (show opCode) (show v) (show v')


oneOperandF
  :: Opcode
  -> Mode
  -> Vector Int
  -> Vector Int
oneOperandF (O ins lhs _ out) lm regs =
  let lhsVal = getOperand lm lhs
  in  regs V.// [(out, lhsVal)]
  where
    getOperand mode x = case mode of
      Imm -> x
      Rel -> regs V.! x


twoOperandF
  :: Opcode
  -> (Int -> Int -> Int)
  -> Mode
  -> Mode
  -> Vector Int
  -> Vector Int
twoOperandF (O inst lhs rhs out) f lm rm regs =
  let lhsVal = getOperand lm lhs
      rhsVal = getOperand rm rhs
  in  -- trace (show inst <> " " <> show lhsVal <> " " <> show rhsVal) $
      regs V.// [(out, f lhsVal rhsVal)]
  where
    getOperand mode x = case mode of
      Imm -> x
      Rel -> regs V.! x


fn :: Opcode -> Vector Int -> Vector Int -> (Int, [Func])
fn opCode bRegs aRegs =
  let functions = fmap ($ opCode) opCFuncs
      output    = fmap (updateWithF bRegs) functions
  in  -- trace (foldMap (\o -> show o <> "\n") output)  $
      (opIns opCode, fmap fst $ filter ((== aRegs) . snd) output)
  where
    updateWithF v func =  (func, fFunc func v)


part1 :: IO ()
part1 = interactSS "input16.txt" $ \inpss -> do
  let inputs    = fmap parseEntry $ (takeWhile (not . null) inpss)
      results   = fmap (\(op, br, ar) -> fn op br ar) inputs
      overThree = length $ filter ((>= 3) . length . snd) results
  printf "Part1: Possible matches over 3 => %d\n" overThree


identifyInstructions :: Map Int (Set Func) -> Map Int String
identifyInstructions m0 = M.fromList $ go m0
  where
    go m =
      let (mSingles, mRest) = M.partition ((== 1) . S.size) m
          identified        = M.toList $ fmap (fName . S.elemAt 0) mSingles
          sFuncs            = foldl1' S.union $ M.elems mSingles
          m'                = fmap (\s -> s `S.difference` sFuncs) mRest
      in  if M.null m' then identified else identified <> go m'


identifyInstructions' :: Map Int (Set Func) -> (Vector String)
identifyInstructions' m0 = head $ flip evalStateT S.empty $
  V.generateM 16 $ \i -> do
    unseen <- gets (m0 M.! i `S.difference`)
    pick   <- lift $ S.toList unseen
    modify (S.insert pick)
    pure (fName pick)


class Container a e | a -> e where
  type Key a
  getElem :: Key a -> a -> e


instance Container (Vector e) e where
  type Key (Vector e) = Int
  getElem k v = v V.! k


instance Container (Map Int e) e where
  type Key (Map Int e) = Int
  getElem k m = m M.! k


runProgram
  :: (Container a String, Key a ~ Int)
  => a
  -> [String]
  -> [(String, Opcode, Vector Int)]
runProgram m ss =
  let v0   = V.fromList [0, 0, 0, 0]
      ops0 = fmap parseOpcode ss
  in  go ops0 v0
  where
    go []        v = []
    go (op:ops)  v =
      let name    = getElem (opIns op) m
          opCfunc = opCFuncMap name
          v'      = fFunc (opCfunc op) v
      in  (name, op, v') : go ops v'


part2 :: IO ()
part2 = part2Internal identifyInstructions


part2' :: IO ()
part2' = part2Internal identifyInstructions'


part2Internal :: (Container a String, Key a ~ Int) => (Map Int (Set Func) -> a) -> IO ()
part2Internal idInst = interactSS "input16.txt" $ \inpss -> do
  let (s1, s2)     = break null inpss
      inpSample    = s1
      program      :: [String] = concat $ dropWhile null s2
      inputs       = fmap parseEntry inpSample
      results      = fmap (\(op, br, ar) -> fn op br ar) inputs
      exactMatches = fmap (\(ins, funcs) -> (ins, S.fromList funcs)) results
      mFn          = idInst $ M.fromListWith (S.union) exactMatches
      reg          = runProgram mFn program
      (_, _, v)    = last reg
      ans          = v V.! 0
  -- mapM_ print (M.toList mFn)
  -- mapM_ print reg
  printf "Part2: %d\n" ans
  pure ()


parseEntry :: [String] -> (Opcode, Vector Int, Vector Int)
parseEntry ss =
  let [beforeS, opS, afterS] = ss
      opCode                 = parseOpcode opS
      beforeReg              = V.fromList $ fmap read $ extractNums beforeS
      afterReg               = V.fromList $ fmap read $ extractNums afterS
  in  (opCode, beforeReg, afterReg)
  where
    extractNums s = splitOn ", " $ init $ tail $ dropWhile (/= '[') s


parseOpcode :: String -> Opcode
parseOpcode s =
  let [ins, lhs, rhs, out] = fmap read $ splitOn " " s
  in  O ins lhs rhs out


test :: IO ()
test = do
  let inp =
        [ "Before: [3, 2, 1, 1]"
        , "9 2 1 2"
        , "After:  [3, 2, 2, 1]" ]
      (op,br,ar) = parseEntry inp
      result = fn op br ar
  mapM_ print result
  pure ()


testParse :: IO ()
testParse = do
  let inp =
        [ "Before: [3, 2, 1, 1]"
        , "9 2 1 2"
        , "After:  [3, 2, 2, 1]" ]
      (op,br,ar) = parseEntry inp
  printf "%s| %s -> %s\n" (show op) (show br) (show ar)
  pure ()
