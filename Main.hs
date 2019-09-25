module Main where

import Control.Monad.State

main :: IO ()
main = runStateT conversation (i, i) >> return ()
  where i = maxBound :: Int

conversation :: StateT (Int, Int) IO ()
conversation = do
  ioST "A「好きな数字を思い浮かべてください」"
  initNum >>= \num -> ioST $ "B「はい(" ++ show num ++ ")」"
  ioST "A「その数字に1を足して」"
  plusST 1 >>= \str -> ioST $ "B (" ++ str ++ ")"
  ioST "A「2をかけて」"
  multST 2 >>= \str -> ioST $ "B (" ++ str ++ ")"
  ioST "A「6を足して」"
  plusST 6 >>= \str -> ioST $ "B (" ++ str ++ ")"
  ioST "A「2で割って」"
  divST 2 >>= \str -> ioST $ "B (" ++ str ++ ")"
  ioST "A「その数字から最初の数字を引いてください」"
  initNum >>= subST >>= \str -> ioST $ "B (" ++ str ++ "だ......)"
  ioST "A「その数字は...4ですね！」"
  comp 4 >>= \str -> ioST $ "B「" ++ str ++ "」"
  return ()

ioST :: String -> StateT (Int, Int) IO ()
ioST = liftIO . putStrLn

initNum :: StateT (Int, Int) IO Int
initNum = state $ \(m, i) -> (i, (m, i))

plusST :: Int -> StateT (Int, Int) IO String
plusST n = state f where f (m, i) = (show m', (m', i)) where m' = m + n

subST :: Int -> StateT (Int, Int) IO String
subST n = state f where f (m, i) = (show m', (m', i)) where m' = m - n

multST :: Int -> StateT (Int, Int) IO String
multST n = state f where f (m, i) = (show m', (m', i)) where m' = m * n

divST :: Int -> StateT (Int, Int) IO String
divST n = state f where f (m, i) = (show m', (m', i)) where m' = m `div` n

comp :: Int -> StateT (Int, Int) IO String
comp n = state $ \(m, i) -> (show (m == n), (m, i))
