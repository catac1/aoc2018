module Day19
  ( test
  , part1
  , part2
  ) where

{-
  To model registers, we can use vector-sized package.
  There are six registers.
  The zero register is for instruction pointer manipulation.
-}

import Lib
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Map qualified as M
import Data.Map (Map)
import Data.Bits


data Opcode = O
  { opLhs :: Int
  , opRhs :: Int
  , opOut :: Int
  } deriving stock Eq

instance Show Opcode where
  show (O lhs rhs out) = printf "O(%2d %2d %2d)" lhs rhs out


data Mode = Imm | Rel deriving stock Show


unary
  :: Mode
  -> Opcode
  -> Vector Int
  -> Vector Int
unary lm (O lhs _ out) regs =
  let lhsVal = getOperand lm lhs
  in  regs V.// [(out, lhsVal)]
  where
    getOperand mode x = case mode of
      Imm -> x
      Rel -> regs V.! x


binary
  :: (Int -> Int -> Int)
  -> Mode
  -> Mode
  -> Opcode
  -> Vector Int
  -> Vector Int
binary f lm rm (O lhs rhs out) regs =
  let lhsVal = getOperand lm lhs
      rhsVal = getOperand rm rhs
  in  -- trace (show inst <> " " <> show lhsVal <> " " <> show rhsVal) $
      regs V.// [(out, f lhsVal rhsVal)]
  where
    getOperand mode x = case mode of
      Imm -> x
      Rel -> regs V.! x


instructionSet :: Map String (Opcode -> Vector Int -> Vector Int)
instructionSet = M.fromList $ zip names functions
  where
    names =
      [ "addi" , "addr" , "muli" , "mulr"
      , "bani" , "banr" , "bori" , "borr"
      , "gtri" , "gtir" , "gtrr"
      , "eqri" , "eqir" , "eqrr"
      , "seti" , "setr" ]
    functions =
      [ addi , addr , muli , mulr
      , bani , banr , bori , borr
      , gtri , gtir , gtrr
      , eqri , eqir , eqrr
      , seti , setr ]


data MachineState = MS
  { mIp :: Int
  , mRegs :: Vector Int
  }

instance Show MachineState where
  show (MS ip regs) = printf "%2d| %18s" ip (show regs)


showRegs :: Vector Int -> String
showRegs v =
  let [a, b, c, d, e, f] = V.toList v in
  printf "%3d %9d %3d %9d %3d %9d" a b c d e f


interpret :: Bool -> String -> Vector Int -> Vector Int
interpret b s regs =
  let [instruction, lhs, rhs, out] = splitOn " " s
      opcode                       = O (read lhs) (read rhs) (read out)
   in
      case M.lookup instruction instructionSet of
        Just f  ->
          let regs' = f opcode regs
              cwhat0' = regs' V.! 0
              cwhat4' = regs' V.! 4
           in if opcode == O 4 0 0 || b then
                trace (printf "%4s %s| %s -> %s"
                      (instruction)
                      (show opcode)
                      (showRegs regs)
                      (showRegs regs')
                      )
                $ regs'
              else
                regs'
        Nothing -> error $ "Uknown instuction: " <> instruction


runProgram :: String -> Vector String -> Vector Int
runProgram ipSetInstruction program =
  go (V.fromList $ replicate 6 0)
  where
    ipIdx = read $ tail $ dropWhile (/= ' ') ipSetInstruction

    len   = V.length program

    go regs =
      let instruciton = program V.! (regs V.! ipIdx)
          regs'       = interpret False instruciton regs
          ip          = regs' V.! ipIdx
       in if ip >= 0 && ip < len - 1 then
            go (incIP ipIdx regs')
          else
            regs'



getTargetInteger :: String -> Vector String -> Int
getTargetInteger ipSetInstruction program =
  go (V.fromList $ 1 : replicate (nRegisters - 1)  0)
  where
    nRegisters       = 6
    ipIdx            = read $ tail $ dropWhile (/= ' ') ipSetInstruction
    programLen       = V.length program
    targetIntRegIdx  = nRegisters - 1

    go regs = let ip = regs V.! ipIdx in
      if ip == programLen - 1 then
        regs V.! targetIntRegIdx
      else
        let regs' = interpret True (program V.! ip) regs in
        go (incIP ipIdx regs')


incIP :: Int -> Vector Int -> Vector Int
incIP ipIdx regs =
  regs V.// [(ipIdx, 1 + regs V.! ipIdx)]


addi :: Opcode -> Vector Int -> Vector Int
addi = binary (+) Rel Imm


addr :: Opcode -> Vector Int -> Vector Int
addr = binary (+) Rel Rel


muli :: Opcode -> Vector Int -> Vector Int
muli = binary (*) Rel Imm


mulr :: Opcode -> Vector Int -> Vector Int
mulr = binary (*) Rel Rel


bani :: Opcode -> Vector Int -> Vector Int
bani = binary (.&.) Rel Imm


banr :: Opcode -> Vector Int -> Vector Int
banr = binary (.&.) Rel Rel


bori :: Opcode -> Vector Int -> Vector Int
bori = binary (.|.) Rel Imm


borr :: Opcode -> Vector Int -> Vector Int
borr = binary (.|.) Rel Rel


gtF :: Int -> Int -> Int
gtF a b = if a > b then 1 else 0


gtir :: Opcode -> Vector Int -> Vector Int
gtir = binary gtF Imm Rel


gtri :: Opcode -> Vector Int -> Vector Int
gtri = binary gtF Rel Imm


gtrr :: Opcode -> Vector Int -> Vector Int
gtrr = binary gtF Rel Rel


eqF :: Int -> Int -> Int
eqF a b = if a == b then 1 else 0


eqir :: Opcode -> Vector Int -> Vector Int
eqir = binary eqF Imm Rel


eqri :: Opcode -> Vector Int -> Vector Int
eqri = binary eqF Rel Imm


eqrr :: Opcode -> Vector Int -> Vector Int
eqrr = binary eqF Rel Rel


seti :: Opcode -> Vector Int -> Vector Int
seti = unary Imm


setr :: Opcode -> Vector Int -> Vector Int
setr = unary Rel


part1 :: IO ()
part1 = interactS "input19.txt" $ \lines -> do
  let ([ipInstruction], rest) = splitAt 1 lines
      program               = V.fromList rest
      regs                  = runProgram ipInstruction program
      ans                   = regs V.! 0

  printf "Part1 : The value of register 0 is %d\n" ans


divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0] <> [n]


part2 :: IO ()
part2 = interactS "input19.txt" $ \lines -> do
  let ([ipInstruction], rest) = splitAt 1 lines
      program                 = V.fromList rest
      targetInt               = getTargetInteger ipInstruction program
      ds                      = divisors targetInt
      ans                     = sum ds
  -- DONE: 1. Extract target number
  --       2. Get the all divisors
  --       3. Sum them up
  printf "Part2: Target(%d) , Divisors(%s) -> Sum(%d)\n" targetInt (show ds) ans


test :: IO ()
test = do
  let inps =
        [ "#ip 0"
        , "seti 5 0 1"
        , "seti 6 0 2"
        , "addi 0 1 0"
        , "addr 1 2 3"
        , "setr 1 0 0"
        , "seti 8 0 4"
        , "seti 9 0 5" ]
      (ipInstruction, program) = (head inps, V.fromList . tail $ inps)
      regs                     = runProgram ipInstruction program

  print "-- final --"
  printf "%s\n" (show regs)

  pure ()
