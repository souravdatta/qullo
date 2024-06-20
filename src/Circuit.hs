{-# LANGUAGE RankNTypes #-}

module Circuit
  ( qubits,
    layer,
    Circuit,
    newCircuit,
    insertGate,
    numRegisters,
    layers,
    runCircuit,
  )
where

import Core (QMat, gate, i, q0, (<=>))

qubits :: Int -> QMat
qubits n = if n <= 1 then q0 else q0 <=> qubits (n - 1)

layer :: [QMat] -> QMat
layer = foldl1 (<=>)

data Circuit = Circuit
  { numRegisters :: Int,
    layers :: [[QMat]]
  }

newCircuit :: Int -> Int -> Circuit
newCircuit n ls = Circuit {numRegisters = n, layers = replicate ls (replicate n i)}

instance Show Circuit where
  show :: Circuit -> String
  show (Circuit nr ls) = "Q(" ++ show nr ++ ")\n" ++ show ls

updateOne :: [a] -> Int -> a -> [a]
updateOne xs indx e =
  if (indx < 0) || (indx >= length xs)
    then xs
    else take indx xs ++ [e] ++ drop (indx + 1) xs

update :: [[a]] -> (Int, Int) -> a -> [[a]]
update xs (a, b) e =
  updateOne xs a (updateOne (xs !! a) b e)

insertGate :: (Int, Int) -> QMat -> Circuit -> Circuit
insertGate (nl, nr) g (Circuit numReg lrs) = Circuit numReg (update lrs (nl, nr) g)

runCircuit :: Circuit -> QMat -> QMat
runCircuit (Circuit _ lrs) input = foldl (\m l -> gate (foldl1 (<=>) (reverse l)) m) input lrs
