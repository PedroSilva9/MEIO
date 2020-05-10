--module Main where
import Cp
import Data.List
import Data.List.Split (chunksOf)

type Matrix a = [[a]]
type Pair a = (a,a)

f1pp = [0.0488, 0.1244, 0.2208, 0.2176, 0.1868, 0.1036, 0.0596, 0.0244, 0.0104, 0.0028, 0.0008, 0.0000, 0.0000]
f1pe = [0.0352, 0.0560, 0.1004, 0.1136, 0.1464, 0.1264, 0.1072, 0.1008, 0.0712, 0.0640, 0.0348, 0.0340, 0.0100]

f2pp = [0.0220, 0.0924, 0.1680, 0.2116, 0.1948, 0.1396, 0.0936, 0.0480, 0.0208, 0.0056, 0.0028, 0.0008, 0.0000]
f2pe = [0.0324, 0.0676, 0.1024, 0.1344, 0.1356, 0.1320, 0.0960, 0.0956, 0.0696, 0.0568, 0.0384, 0.0292, 0.0100]

-- gera todas as combinações de estados possiveis
generateStates :: [Pair Int]
generateStates = [(x,y) | x <- [0..12], y <- [0..12]]

-- gera um par contendo cada um uma lista de pares com os resultados da multiplicação das probabilidades para cada uma das filiais
-- output : ((filial 1, filial2), (custo1, custo2)) no caso em que nao transfere nada
filiais :: ((Matrix Double, Matrix Double), (Matrix Double, Matrix Double))
filiais =  devides $ (unzip >< unzip) . unzip . map (\x -> foldr (\l acc -> con acc x l) ((0,0), (0,0)) states) $ states
    where states  = generateStates
          divide  = chunksOf 13
          func ll = map (snd . f) $ ll
          devides = (divide >< divide) >< (func . divide >< func . divide)
          f       = foldl (\(acc, l) x -> if acc > 8
                                             then (succ acc, l ++ [x - 10])
                                             else (succ acc, l ++ [x])) (0,[])

con acc@((acc1, acc2),(acc3, acc4)) (l,c) (p,e) = if plus (minus (l,p), e) == c then tuplo else acc
    where filial1 = acc1 + ((f1pp !! p) * (f1pe !! e))
          filial2 = acc2 + ((f2pp !! p) * (f2pe !! e))
          custo1  = acc3 + ((f1pp !! p) * (f1pe !! e)) * 30 * (fromIntegral (min l p))
          custo2  = acc4 + ((f2pp !! p) * (f2pe !! e)) * 30 * (fromIntegral (min l p))
          tuplo   = ((filial1, filial2), (custo1, custo2))
          condition = cond (> 8) (const 10) (const 0)

-- caso a soma seja maior que 12 fica 12 caso contrario fica a soma
plus = cond ((<= 12) . uncurry (+)) (uncurry (+)) (const 12)

-- caso a subtraçao seja menor que 0 fica 0 caso contrario fica a subtraçao
minus = cond ((>= 0) . uncurry (-)) (uncurry (-)) (const 0)

-- emparelha as matrizes de ambas as filiais
allTheMatrices :: [Pair (Matrix Double)]
allTheMatrices = uncurry zip . (func >< func) $ allShifts
    where func input = map (chunksOf 169 . uncurry joinMatrices) $ input

allShifts = (mp, mc')
    where (mp, mc) = (generateAllMatrices >< generateAllMatrices) $ filiais
          mc'      = (head $ mc) : subsCosts li (tail mc)
          li       = allIndexes $ tail mp

getNonZeroIndexes :: (Num a, Eq a) => [a] -> [Int]
getNonZeroIndexes ll = snd $ foldl (\(i,l) x -> if x/=0 then (i+1,i:l) else (i+1,l)) (0,[]) ll

-- snd $ filis -> (custo1, custo2) -> [(custo1, custo2)] representa o lucro de transferir de filial para filial
-- [(custo1, custo2)] [(filial1, filial2)] ====> [((filial1, custo1), (filial2, custo2))]
-- comparar a linha da filial em questao com a linha da matriz custo associada, caso a linha seja constituida apenas por 0's temos que
-- subtrair na parte dos custos

-- cria todas as matrizes com os shifts
-- input : (filial1, filial2)
-- output : [(filial1, filial2)] -> representa as transações de filial para filial
generateAllMatrices :: Pair (Matrix Double) -> [Pair (Matrix Double)]
generateAllMatrices fil = fil : a ++ b
    where (a,b) = (func (upshift >< downshift), func (downshift >< upshift))
          func f = tail . take 4 . iterate f $ fil

-- downshift de uma matriz
downshift = cons . ((flip replicate 0) >< id) . split (length . head) init

-- upshift de uma matriz
upshift = uncurry (++) . (id >< (singl . (flip replicate 0))) . split tail (length . head)

-- emparelhar as duas matrizes respetivas às filiais
joinMatrices f1 f2 = foldr (\((f1l,f2l),(f1c,f2c)) acc -> (((f1 !! f1l) !! f1c)*((f2 !! f2l) !! f2c)):acc) [] l
    where l = [((x,y), (k,z)) | x <- [0..12], y <- [0..12], k <- [0..12], z <- [0..12]]

-- subtrai os elementos da matriz lc por diff nos indices indicados pela
-- lista li
replace li diff lc = chunksOf 13 . snd $ foo
    where foo = foldl(\(acc,l) x -> if elem acc li
                                       then (acc + 1, l ++ [x - fromIntegral(diff)])
                                       else (acc + 1, l ++ [x])) (0, []) $ concat lc


-- gera os indices /= 0 para todas as matrizes de cada filial
allIndexes :: [(Matrix Double, Matrix Double)] -> [([Int], [Int])]
allIndexes = map (getNonZeroIndexes . concat >< getNonZeroIndexes . concat)

-- substrai nas matrizes custo pelo valor pretendido nos indices
-- pretendidos
-- input:: [([Int], [Int])] -> [(Matrix Double, Matrix Double)]
-- output :: [(Matrix Double)]
subsCosts li lc = snd . foldl(\(acc, l) x -> if (acc < 3)
                                                then fstTransfers li acc l x
                                                else sndTransfers li acc l x) (0,[]) $ lc
    where fstTransfers li acc l x = (acc + 1, l ++ [(replace (fst $ (!!) li acc) ((acc + 1) * 7) >< id) $ x])
          sndTransfers li acc l x = (acc + 1, l ++ [(id >< replace (snd $ (!!) li acc) ((acc + 1 - 3) * 7)) $ x])

