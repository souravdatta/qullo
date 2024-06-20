module Main (main) where

import Circuit (insertGate, layer, newCircuit, qubits, runCircuit)
import Core (defaultCounts, gate, h, x)

main :: IO ()
main = do
  let c = insertGate (0, 2) x $ insertGate (0, 1) h $ insertGate (0, 0) h $ newCircuit 3 1
  let sv = runCircuit c $ qubits 3
  cnts <- defaultCounts sv
  print cnts
