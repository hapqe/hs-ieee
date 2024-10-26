import Data.Bits (Bits (xor))
import Data.Sequence (unfoldl)
import Debug.Trace (traceShow, traceShowId)

coolTrace :: (Show b) => b -> b
coolTrace a = traceShow ('-' : show a ++ "-") a

newtype Ieee = Ieee {unIeee :: [Bool]}

parseIeee :: String -> Ieee
parseIeee str = Ieee $ sign : exponent ++ limitMantissa infMantissa
 where
  sign = head str == '-'
  nosign = if head str == '-' then tail str else str

  decimal =
    tail -- drop the first bit (always 0)
      . (calcDecimal <$> read <*> (10 ^) . length) -- ensure the multiplied numbers are integers
      . tail -- drop '.'
      $ dropWhile (/= '.') nosign -- decimal string
  precomma = calcPrecomma . read $ takeWhile (/= '.') nosign

  exponent =
    take 8
      . addTrailingZeros
      . calcPrecomma
      . toInteger
      $ 126
        + if null precomma -- there is no precomma part
          then
            -(length . takeWhile not $ decimal)
          else length precomma

  addTrailingZeros bs = replicate (8 - length bs) False <> bs

  infMantissa = drop 1 $ dropWhile not (precomma ++ decimal) -- drop 1 because of the implicit bit

calcPrecomma 0 = []
calcPrecomma k = calcPrecomma (k `div` 2) ++ [k `mod` 2 == 1]

calcDecimal 0 _ = repeat False
calcDecimal k limit
  | k >= limit = True : calcDecimal ((k - limit) * 2) limit
  | otherwise = False : calcDecimal (k * 2) limit

limitMantissa inf = unrounded + round g r s
 where
  (unrounded, g : r : ss) = splitAt 23 inf
  s = or ss

  round False _ _ = False
  round True True _ = True
  round True False True = True
  round True False False = last unrounded

  (+) [] _ = [] -- bitwise addition
  (+) (x : xs) b = x `xor` b : xs + (x && b)

instance Show Ieee where
  show :: Ieee -> String
  show = concatMap (\b -> if b then "1" else "0") . unIeee

main = do
  let str = "13029.8391028321"
  let f = parseIeee str
  putStrLn $ str <> " is " <> show f <> " in binary!"
  print $ show f == "01000110010010111001011101011011"
