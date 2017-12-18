module ListasSolucao where
import Data.Function
import Data.Aeson
import Data.Text
import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Text (encodeToLazyText)
import Test.HUnit

testMeuLastVazia = TestCase (assertEqual "testMeuLastVazia" "Lista Vazia!" (meuLast []) )
testMeuLastUmElemento = TestCase (assertEqual "testMeuLastUmElemento" 5 (meuLast [5]) )
testMeuLastImpar = TestCase (assertEqual "testMeuLastImpar" 3 (meuLast [13, 25, 3]) )
testMeuLastPar = TestCase (assertEqual "testMeuLastPar" 12 (meuLast [13, 2, 25, 12]) )
testMeuLastNegativo = TestCase (assertEqual "testMeuLastNegativo" -8 (meuLast [-5, -3, -8]) )
testMeuLastZeros = TestCase (assertEqual "testMeuLastZeros" 0 (meuLast [0, 0, 0, 0]) )
testMeuLastRepetidos = TestCase (assertEqual "testMeuLastRepetidos" 2 (meuLast [2, 2, 2]) )

testsMeuLast = TestList [testMeuLastVazia, testMeuLastUmElemento, testMeuLastImpar, testMeuLastPar, testMeuLastNegativo, testMeuLastZeros, testMeuLastRepetidos]

testPenultimoVazia = TestCase (assertEqual "testPenultimoVazia" "Lista sem penultimo" (penultimo []) )
testPenultimoUmElemento = TestCase (assertEqual "testPenultimoUmElemento" "Lista sem penultimo" (penultimo [5]) )
testPenultimoImpar = TestCase (assertEqual "testPenultimoImpar" 25 (penultimo [13, 25, 3]) )
testPenultimoPar = TestCase (assertEqual "testPenultimoPar" 8 (penultimo [13, 2, 8, 12]) )
testPenultimoNegativo = TestCase (assertEqual "testPenultimoNegativo" -3 (penultimo [-5, -3, -8]) )
testPenultimoZeros = TestCase (assertEqual "testPenultimoZeros" 0 (penultimo [0, 0, 0, 0]) )
testPenultimoRepetidos = TestCase (assertEqual "testPenultimoRepetidos" 2 (penultimo [2, 2, 2]) )

testsPenultimo = TestList [testPenultimoVazia, testPenultimoUmElemento, testPenultimoImpar,
 testPenultimoPar, testPenultimoNegativo, testPenultimoZeros, testPenultimoRepetidos]

testElementAtVazia = TestCase (assertEqual "testElementAtVazia" (elementAt 1 []) ){- testar-}
testElementAtUmElemento = TestCase (assertEqual "testElementAtUmElemento" 5 (elementAt 1 [5]) )
{- testar a mais-}
{- testar a menos-}
testElementAtImpar = TestCase (assertEqual "testElementAtImpar" 3 (elementAt 3 [13, 25, 3]) )
testElementAtPar = TestCase (assertEqual "testElementAtPar" 12 (elementAt 4 [13, 2, 25, 12]) )
testElementAtNegativo = TestCase (assertEqual "testElementAtNegativo" -5 (elementAt 1 [-5, -3, -8]) )
testElementAtZeros = TestCase (assertEqual "testElementAtZeros" 0 (elementAt 3 [0, 0, 0, 0]) )
testElementAtRepetidos = TestCase (assertEqual "testElementAttRepetidos" 2 (elementAt 2 [2, 2, 2]) )

testsElementAt = TestList [testElementAtVazia, testElementAtUmElemento, testElementAtImpar,
 testElementAtPar, testElementAtNegativo, testElementAtZeros, testElementAtRepetidos]

testMeuLengthVazia = TestCase (assertEqual "testMeuLengthVazia" 0 (meuLength []) )
testMeuLengthUmElemento = TestCase (assertEqual "testMeuLengthUmElemento" (meuLength [5]) )
testMeuLengthImpar = TestCase (assertEqual "testMeuLengthImpar" 3 (meuLength [13, 25, 3]) )
testMeuLengthPar = TestCase (assertEqual "testMeuLengthPar" 4 (meuLength [13, 2, 8, 12]) )
testMeuLengthNegativo = TestCase (assertEqual "testMeuLengthNegativo" 3 (meuLength [-5, -3, -8]) )
testMeuLengthZeros = TestCase (assertEqual "testMeuLengthZeros" 4 (meuLength [0, 0, 0, 0]) )
testMeuLengthRepetidos = TestCase (assertEqual "testMeuLengthRepetidos" 3 (meuLength [2, 2, 2]) )

testsMeuLength = TestList [testMeuLengthVazia, testMeuLengthUmElemento, testMeuLengthImpar,
 testMeuLengthPar, testMeuLengthNegativo, testMeuLengthZeros, testMeuLengthRepetidos]

testMeuReversoVazia = TestCase (assertEqual "testMeuReversoVazia" [] (meuReverso []) )
testMeuReversoUmElemento = TestCase (assertEqual "testMeuReversoUmElemento" [5] (meuReverso [5]) )
testMeuReversoImpar = TestCase (assertEqual "testMeuReversoImpar" [3, 25, 13] (meuReverso [13, 25, 3]) )
testMeuReversoPar = TestCase (assertEqual "testMeuReversoPar" [12, 8, 2, 13] (meuReverso [13, 2, 8, 12]) )
testMeuReversoNegativo = TestCase (assertEqual "testMeuReversoNegativo" [-8, -3, -5] (meuReverso [-5, -3, -8]) )
testMeuReversoZeros = TestCase (assertEqual "testMeuReversoZeros" [0, 0, 0, 0] (meuReverso [0, 0, 0, 0]) )
testMeuReversoRepetidos = TestCase (assertEqual "testMeuReversoRepetidos" [2, 2, 2] (meuReverso [2, 2, 2]) )

testsMeuReverso = TestList [testMeuReversoVazia, testMeuReversoUmElemento, testMeuReversoImpar,
 testMeuReversoPar, testMeuReversoNegativo, testMeuReversoZeros, testMeuReversoRepetidos]

testIsPalindromeVazia = TestCase (assertEqual "testIsPalindromeVazia" True (isPalindrome []) )
testIsPalindromeUmElemento = TestCase (assertEqual "testIsPalindromeElemento" True (isPalindrome [5]) )
testIsPalindromeImpar = TestCase (assertEqual "testIsPalindromeImpar" False (isPalindrome [13, 25, 3]) )
testIsPalindromeImpar = TestCase (assertEqual "testIsPalindromeImpar" True (isPalindrome [13, 25, 13]) )
testIsPalindromePar = TestCase (assertEqual "testIsPalindromePar" False (isPalindrome [13, 2, 8, 12]) )
testIsPalindromePar = TestCase (assertEqual "testIsPalindromePar" True (isPalindrome [12, 3, 3, 12]) )
testIsPalindromeNegativo = TestCase (assertEqual "testIsPalindromeNegativo" Faalse (isPalindrome [-5, -3, -8]) )
testIsPalindromeNegativo = TestCase (assertEqual "testIsPalindromeNegativo" True (isPalindrome [-1, -3, -1]) )
testIsPalindromeZeros = TestCase (assertEqual "testIsPalindromeZeros" True (isPalindrome [0, 0, 0, 0]) )
testIsPalindromeRepetidos = TestCase (assertEqual "testIsPalindromeRepetidos" True (isPalindrome [2, 2, 2]) )

testsIsPalindrome = TestList [testIsPalindromeVazia, testIsPalindromeElemento, testIsPalindromeImpar,
 testIsPalindromePar, testIsPalindromeNegativo, testIsPalindromeZeros, testIsPalindromeRepetidos]
 
testCompressVazia = TestCase (assertEqual "testCompressVazia" [] (compress []) )
testCompressUmElemento = TestCase (assertEqual "testCompressUmElemento" [5] (compress [5]) )
testCompressImpar = TestCase (assertEqual "testCompressImpar" [13, 25, 3, 5] (compress [13, 25, 3, 5, 3]) )
testCompressPar = TestCase (assertEqual "testCompressPar" [12, 2, 8] (compress [12, 2, 8, 12]) )
testCompressNegativo = TestCase (assertEqual "testCompressNegativo" [-5, -8] (compress [-5, -8, -8]) )
testCompressZeros = TestCase (assertEqual "testCompressZeros" [0] (compress [0, 0, 0, 0]) )
testCompressRepetidos = TestCase (assertEqual "testCompressRepetidos" [2] (compress [2, 2, 2]) )

testsCompress = TestList [testCompressVazia, testCompressUmElemento, testCompressImpar,
 testCompressPar, testCompressNegativo, testCompressZeros, testCompressRepetidos]

testCompactVazia = TestCase (assertEqual "testCompactVazia" [] (compact []) )
testCompactUmElemento = TestCase (assertEqual "testCompactUmElemento" [5] (compact [5]) )
testCompactImpar = TestCase (assertEqual "testCompactImpar" [3, 3, 25] (compact [3, 25, 3]) )
testCompactPar = TestCase (assertEqual "testCompactPar" [13, 12, 12, 8] (compact [13, 12, 8, 12]) )
testCompactNegativo = TestCase (assertEqual "testCompactNegativo" [-5, -3, -8] (compact [-5, -3, -8]) )
testCompactZeros = TestCase (assertEqual "testCompactZeros" [0, 0, 0, 0] (compact [0, 0, 0, 0]) )
testCompactRepetidos = TestCase (assertEqual "testCompactRepetidos" [2, 2, 2] (compact [2, 2, 2]) )

testsCompact = TestList [testCompactVazia, testCompactUmElemento, testCompactImpar,
 testCompactPar, testCompactNegativo, testCompactZeros, testCompactRepetidos]

{-
removeList
encode
split
slice
insertAt
minList
mySum
maxList
buildPalindrome
mean
myAppend
-}
{-testFatorial01 = TestCase (assertEqual "testFatorial01" 1 (fatorial 0) )
testFatorial02 = TestCase (assertEqual "testFatorial02" 1 (fatorial 1) )
testFatorial03 = TestCase (assertEqual "testFatorial03" 2 (fatorial 2) )
testFatorial04 = TestCase (assertEqual "testFatorial04" 6 (fatorial 3) )
testFatorial05 = TestCase (assertEqual "testFatorial05" 24 (fatorial 4) )
testFatorial06 = TestCase (assertEqual "testFatorial06" 3628800 (fatorial 10) )

testsFatorial = TestList [testFatorial01, testFatorial02, testFatorial03,
  testFatorial04, testFatorial05, testFatorial06]

testIsPrime01 = TestCase (assertEqual "testIsPrime01" False (isPrime 0) )
testIsPrime02 = TestCase (assertEqual "testIsPrime02" False (isPrime (-1)) )
testIsPrime03 = TestCase (assertEqual "testIsPrime03" True (isPrime 1) )
testIsPrime04 = TestCase (assertEqual "testIsPrime04" True (isPrime 2) )
testIsPrime05 = TestCase (assertEqual "testIsPrime05" True (isPrime 3) )
testIsPrime06 = TestCase (assertEqual "testIsPrime06" False (isPrime 4) )
testIsPrime07 = TestCase (assertEqual "testIsPrime07" True (isPrime 5) )
testIsPrime08 = TestCase (assertEqual "testIsPrime08" False (isPrime 222) )
testIsPrime09 = TestCase (assertEqual "testIsPrime09" True (isPrime 223) )

testsIsPrime = TestList [testIsPrime01, testIsPrime02, testIsPrime03,
  testIsPrime04, testIsPrime05, testIsPrime06, testIsPrime07, testIsPrime08,
  testIsPrime09]

testFib01 = TestCase (assertEqual "testFib01" 0 (fib 0) )
testFib02 = TestCase (assertEqual "testFib02" 1 (fib 1) )
testFib03 = TestCase (assertEqual "testFib03" 1 (fib 2) )
testFib04 = TestCase (assertEqual "testFib04" 2 (fib 3) )
testFib05 = TestCase (assertEqual "testFib05" 3 (fib 4) )
testFib06 = TestCase (assertEqual "testFib06" 5 (fib 5) )
testFib07 = TestCase (assertEqual "testFib07" 8 (fib 6) )
testFib08 = TestCase (assertEqual "testFib08" 610 (fib 15) )

testsFib = TestList [testFib01, testFib02, testFib03, testFib04, testFib05,
  testFib06,testFib07,testFib08]

testMdc01 = TestCase (assertEqual "testeMdc01" 0 (mdc 0 0) )
testMdc02 = TestCase (assertEqual "testeMdc02" 1 (mdc 0 1) )
testMdc03 = TestCase (assertEqual "testeMdc03" 1 (mdc 1 1) )
testMdc04 = TestCase (assertEqual "testeMdc04" 1 (mdc 2 1) )
testMdc05 = TestCase (assertEqual "testeMdc05" 2 (mdc 2 2) )
testMdc06 = TestCase (assertEqual "testeMdc06" 5 (mdc 5 15) )
testMdc07 = TestCase (assertEqual "testeMdc07" 1 (mdc 7 13) )
testMdc08 = TestCase (assertEqual "testeMdc08" 25 (mdc 225 25) )
testMdc09 = TestCase (assertEqual "testeMdc09" 1 (mdc 1 (-1) ) )
testMdc10 = TestCase (assertEqual "testeMdc11" 1 (mdc (-1) 1) )
testMdc11 = TestCase (assertEqual "testeMdc12" 1 (mdc (-1) (-1) ) )
testMdc12 = TestCase (assertEqual "testeMdc13" 2 (mdc (-2) (-2) ) )
testMdc13 = TestCase (assertEqual "testeMdc14" 1 (mdc (-7) (-13) ) )
testMdc14 = TestCase (assertEqual "testeMdc15" 12 (mdc (-12) (-24) ) )

testsMdc = TestList [testMdc01, testMdc02, testMdc03, testMdc04, testMdc05,
  testMdc06, testMdc07, testMdc08, testMdc09, testMdc10, testMdc11, testMdc12,
  testMdc13, testMdc14]

testCoprimo01 = TestCase (assertEqual "testCoprimo01" False (coprimo 0 0) )
testCoprimo02 = TestCase (assertEqual "testCoprimo02" True (coprimo 0 1) )
testCoprimo03 = TestCase (assertEqual "testCoprimo03" True (coprimo 1 1) )
testCoprimo04 = TestCase (assertEqual "testCoprimo04" True (coprimo 2 1) )
testCoprimo05 = TestCase (assertEqual "testCoprimo05" False (coprimo 2 2) )
testCoprimo06 = TestCase (assertEqual "testCoprimo06" False (coprimo 5 15) )
testCoprimo07 = TestCase (assertEqual "testCoprimo07" True (coprimo 7 13) )
testCoprimo08 = TestCase (assertEqual "testCoprimo08" False (coprimo 225 25) )
testCoprimo09 = TestCase (assertEqual "testCoprimo09" True (coprimo 1 (-1) ) )
testCoprimo10 = TestCase (assertEqual "testCoprimo10" True (coprimo (-1) 1) )
testCoprimo11 = TestCase (assertEqual "testCoprimo11" True (coprimo (-1) (-1) ) )
testCoprimo12 = TestCase (assertEqual "testCoprimo12" False (coprimo (-2) (-2) ) )
testCoprimo13 = TestCase (assertEqual "testCoprimo13" True (coprimo (-7) (-13) ) )
testCoprimo14 = TestCase (assertEqual "testCoprimo14" False (coprimo (-12) (-24) ) )

testsCoprimo = TestList [testCoprimo01, testCoprimo02, testCoprimo03,testCoprimo04,
  testCoprimo05,testCoprimo06, testCoprimo07, testCoprimo08, testCoprimo09,
  testCoprimo10, testCoprimo11, testCoprimo12, testCoprimo13, testCoprimo14]


-}

testsPenultimo = TestList []
testsElementAt = TestList []
testsMeuLength = TestList []
testsMeuReverso = TestList []
testsIsPalindrome = TestList []
testsCompress = TestList []
testsCompact = TestList []
testsEncode = TestList []
testsSplit = TestList []
testsSlice = TestList []
testsInsertAt = TestList []
testsMinList = TestList []
testsMySum = TestList []
testsMaxList = TestList []
testsBuildPalindrome = TestList []
testsMean = TestList []
testsMyAppend = TestList []


tests = TestList [testsMeuLast]

reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = do
  return (count+1)

myPutText = PutText reportMsg 0  :: PutText Int

instance ToJSON Counts where
  toJSON (Counts cases tried errors failures) = object
    [ pack "matricula" .= ""
    , pack "totalTestes" .= show tried
    , pack "erros" .= show errors
    , pack "falhas" .= show failures
    , pack "passaram" .= show (tried - errors - failures)
    ]

main = do
  (testCounts, msgCount) <- runTestText myPutText tests
  let a = (encode testCounts)
  B.putStrLn $ a
  I.writeFile "test-output.json" (encodeToLazyText testCounts)
  Prelude.putStrLn "Arquivo json criado"
  return ()
