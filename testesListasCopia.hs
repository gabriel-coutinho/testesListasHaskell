import ListasSolucao
import Data.Function
import Data.Aeson as Aeson
import Data.Text
import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.Text (encodeToLazyText)
import Test.HUnit

testMeuLastVazia = TestCase (assertEqual "testMeuLastVazia" "Lista Vazia!" (meuLast ([] :: [Int])) )
testMeuLastUmElemento = TestCase (assertEqual "testMeuLastUmElemento" 5 (meuLast [5]) )
testMeuLastImpar = TestCase (assertEqual "testMeuLastImpar" 3 (meuLast [13, 25, 3]) )
testMeuLastPar = TestCase (assertEqual "testMeuLastPar" 12 (meuLast [13, 2, 25, 12]) )
testMeuLastNegativo = TestCase (assertEqual "testMeuLastNegativo" (-8) (meuLast [(-5), (-3), (-8)]) )
testMeuLastZeros = TestCase (assertEqual "testMeuLastZeros" 0 (meuLast [0, 0, 0, 0]) )
testMeuLastRepetidos = TestCase (assertEqual "testMeuLastRepetidos" 2 (meuLast [2, 2, 2]) )

testsMeuLast = TestList [testMeuLastVazia, testMeuLastUmElemento, testMeuLastImpar,
 testMeuLastPar, testMeuLastNegativo, testMeuLastZeros, testMeuLastRepetidos]

testPenultimoVazia = TestCase (assertEqual "testPenultimoVazia" "Lista sem penultimo" (penultimo []) )
---testPenultimoUmElemento = TestCase (assertEqual "testPenultimoUmElemento" "Lista sem penultimo" (penultimo [5]) )
testPenultimoImpar = TestCase (assertEqual "testPenultimoImpar" 25 (penultimo [13, 25, 3]) )
testPenultimoPar = TestCase (assertEqual "testPenultimoPar" 8 (penultimo [13, 2, 8, 12]) )
testPenultimoNegativo = TestCase (assertEqual "testPenultimoNegativo" (-3) (penultimo [(-5), (-3), (-8)]) )
testPenultimoZeros = TestCase (assertEqual "testPenultimoZeros" 0 (penultimo [0, 0, 0, 0]) )
testPenultimoRepetidos = TestCase (assertEqual "testPenultimoRepetidos" 2 (penultimo [2, 2, 2]) )

testsPenultimo = TestList [testPenultimoVazia, testPenultimoImpar,
 testPenultimoPar, testPenultimoNegativo, testPenultimoZeros, testPenultimoRepetidos]

testElementAtVazia = TestCase (assertEqual "testElementAtVazia" "Lista Vazia" (elementAt 1 ([] :: [Int])) )
testElementAtUmElemento = TestCase (assertEqual "testElementAtUmElemento" 5 (elementAt 1 [5]) )
testElementAtImpar = TestCase (assertEqual "testElementAtImpar" 3 (elementAt 3 [13, 25, 3]) )
testElementAtPar = TestCase (assertEqual "testElementAtPar" 12 (elementAt 4 [13, 2, 25, 12]) )
testElementAtNegativo = TestCase (assertEqual "testElementAtNegativo" (-5) ((elementAt 1 [(-5), (-3), (-8)])) )
testElementAtZeros = TestCase (assertEqual "testElementAtZeros" 0 (elementAt 3 [0, 0, 0, 0]) )
testElementAtRepetidos = TestCase (assertEqual "testElementAttRepetidos" 2 (elementAt 2 [2, 2, 2]) )

testsElementAt = TestList [testElementAtVazia, testElementAtUmElemento, testElementAtImpar,
 testElementAtPar, testElementAtNegativo, testElementAtZeros, testElementAtRepetidos]

testMeuLengthVazia = TestCase (assertEqual "testMeuLengthVazia" 0 (meuLength []) )
testMeuLengthUmElemento = TestCase (assertEqual "testMeuLengthUmElemento" 1 (meuLength [5]) )
testMeuLengthImpar = TestCase (assertEqual "testMeuLengthImpar" 3 (meuLength [13, 25, 3]) )
testMeuLengthPar = TestCase (assertEqual "testMeuLengthPar" 4 (meuLength [13, 2, 8, 12]) )
testMeuLengthNegativo = TestCase (assertEqual "testMeuLengthNegativo" 3 (meuLength [(-5), (-3), (-8)]) )
testMeuLengthZeros = TestCase (assertEqual "testMeuLengthZeros" 4 (meuLength [0, 0, 0, 0]) )
testMeuLengthRepetidos = TestCase (assertEqual "testMeuLengthRepetidos" 3 (meuLength [2, 2, 2]) )

testsMeuLength = TestList [testMeuLengthVazia, testMeuLengthUmElemento, testMeuLengthImpar,
 testMeuLengthPar, testMeuLengthNegativo, testMeuLengthZeros, testMeuLengthRepetidos]

testMeuReversoVazia = TestCase (assertEqual "testMeuReversoVazia" [] (meuReverso ([] :: [Int])))
testMeuReversoUmElemento = TestCase (assertEqual "testMeuReversoUmElemento" [5] (meuReverso [5]) )
testMeuReversoImpar = TestCase (assertEqual "testMeuReversoImpar" [3, 25, 13] (meuReverso [13, 25, 3]) )
testMeuReversoPar = TestCase (assertEqual "testMeuReversoPar" [12, 8, 2, 13] (meuReverso [13, 2, 8, 12]) )
testMeuReversoNegativo = TestCase (assertEqual "testMeuReversoNegativo" [(-8), (-3), (-5)] (meuReverso [(-5), (-3), (-8)]) )
testMeuReversoZeros = TestCase (assertEqual "testMeuReversoZeros" [0, 0, 0, 0] (meuReverso [0, 0, 0, 0]) )
testMeuReversoRepetidos = TestCase (assertEqual "testMeuReversoRepetidos" [2, 2, 2] (meuReverso [2, 2, 2]) )

testsMeuReverso = TestList [testMeuReversoVazia, testMeuReversoUmElemento, testMeuReversoImpar,
 testMeuReversoPar, testMeuReversoNegativo, testMeuReversoZeros, testMeuReversoRepetidos]

testIsPalindromeVazia = TestCase (assertEqual "testIsPalindromeVazia" True ((ListasSolucao.isPalindrome ([] :: [Int]))) )
testIsPalindromeUmElemento = TestCase (assertEqual "testIsPalindromeElemento" True (isPalindrome [5]) )
testIsPalindromeImpar = TestCase (assertEqual "testIsPalindromeImpar" False (isPalindrome [13, 25, 3]) )
testIsPalindromeImparEqual = TestCase (assertEqual "testIsPalindromeImparEqual" True (isPalindrome [13, 25, 13]) )
testIsPalindromePar = TestCase (assertEqual "testIsPalindromePar" False (isPalindrome [13, 2, 8, 12]) )
testIsPalindromeParEqual = TestCase (assertEqual "testIsPalindromeParEqual" True (isPalindrome [12, 3, 3, 12]) )
testIsPalindromeNegativo = TestCase (assertEqual "testIsPalindromeNegativo" False (isPalindrome [(-5), (-3), (-8)]) )
testIsPalindromeNegativoEqual = TestCase (assertEqual "testIsPalindromeNegativoEqual" True (isPalindrome [(-1), (-3), (-1)]) )
testIsPalindromeZeros = TestCase (assertEqual "testIsPalindromeZeros" True (isPalindrome [0, 0, 0, 0]) )
testIsPalindromeRepetidos = TestCase (assertEqual "testIsPalindromeRepetidos" True (isPalindrome [2, 2, 2]) )

testsIsPalindrome = TestList [testIsPalindromeVazia, testIsPalindromeUmElemento, testIsPalindromeImpar,
 testIsPalindromePar, testIsPalindromeNegativo, testIsPalindromeImparEqual, testIsPalindromeParEqual,
  testIsPalindromeNegativoEqual, testIsPalindromeZeros, testIsPalindromeRepetidos]
 
testCompressVazia = TestCase (assertEqual "testCompressVazia" [] (compress ([] :: [Int])) )
testCompressUmElemento = TestCase (assertEqual "testCompressUmElemento" [5] (compress [5]) )
testCompressImpar = TestCase (assertEqual "testCompressImpar" [13, 25, 3, 5] (compress [13, 25, 3, 5, 3]) )
testCompressPar = TestCase (assertEqual "testCompressPar" [12, 2, 8] (compress [12, 2, 8, 12]) )
testCompressNegativo = TestCase (assertEqual "testCompressNegativo" [(-5), (-8)] (compress [(-5), (-8), (-8)]) )
testCompressZeros = TestCase (assertEqual "testCompressZeros" [0] (compress [0, 0, 0, 0]) )
testCompressRepetidos = TestCase (assertEqual "testCompressRepetidos" [2] (compress [2, 2, 2]) )

testsCompress = TestList [testCompressVazia, testCompressUmElemento, testCompressImpar,
 testCompressPar, testCompressNegativo, testCompressZeros, testCompressRepetidos]

testCompactVazia = TestCase (assertEqual "testCompactVazia" [] (ListasSolucao.compact ([] :: [Int])) )
testCompactUmElemento = TestCase (assertEqual "testCompactUmElemento" [5] (compact [5]) )
testCompactImpar = TestCase (assertEqual "testCompactImpar" [3, 3, 25] (compact [3, 25, 3]) )
testCompactPar = TestCase (assertEqual "testCompactPar" [13, 12, 12, 8] (compact [13, 12, 8, 12]) )
testCompactNegativo = TestCase (assertEqual "testCompactNegativo" [(-5), (-3), (-8)] (compact [(-5), (-3), (-8)]) )
testCompactZeros = TestCase (assertEqual "testCompactZeros" [0, 0, 0, 0] (compact [0, 0, 0, 0]) )
testCompactRepetidos = TestCase (assertEqual "testCompactRepetidos" [2, 2, 2] (compact [2, 2, 2]) )

testsCompact = TestList [testCompactVazia, testCompactUmElemento, testCompactImpar,
 testCompactPar, testCompactNegativo, testCompactZeros, testCompactRepetidos]

testEncodeVazia = TestCase (assertEqual "testEncodeVazia" [] (ListasSolucao.encode ([] :: [Int])))
testEncodeUmElemento = TestCase (assertEqual "testEncodeUmElemento" [(5,1)] (ListasSolucao.encode [5]))
testEncodeImpar = TestCase (assertEqual "testEncodeImpar" [(3,2),(25,1)] (ListasSolucao.encode [3, 25, 3]))
testEncodePar = TestCase (assertEqual "testEncodePar" [(13,1), (12,2), (8,1)] (ListasSolucao.encode [13, 12, 8, 12]))
testEncodeNegativo = TestCase (assertEqual "testEncodeNegativo" [((-5),1) ((-3),1), ((-8),1)] (ListasSolucao.encode [(-5), (-3), (-8)]))
testEncodeZeros = TestCase (assertEqual "testEncodeZeros" [(0,4)] (ListasSolucao.encode [0, 0, 0, 0]))
testEncodeRepetidos = TestCase (assertEqual "testEncodeRepetidos" [(2,3)] (ListasSolucao.encode [2, 2, 2]))

testsEncode = TestList [testEncodeVazia, testEncodeUmElemento, testEncodeImpar,
 testEncodePar, testEncodeNegativo, testEncodeZeros, testEncodeRepetidos]

testSplitVazia = TestCase (assertEqual "testSplitVazia" [] ((ListasSolucao.split ([] :: [Int]) 0)) )
testSplitUmElemento = TestCase (assertEqual "testSplitUmElemento" [[5]] (ListasSolucao.split [5] 1))
testSplitImpar = TestCase (assertEqual "testSplitImpar" [[3, 25],[3]] (ListasSolucao.split [3, 25, 3] 1))
testSplitPar = TestCase (assertEqual "testSplitPar" [[13, 12],[8, 12]] (ListasSolucao.split [13, 12, 8, 12] 2))
testSplitNegativo = TestCase (assertEqual "testSplitNegativo" [[(-5), (-3), (-8)]] (ListasSolucao.split [(-5), (-3), (-8)] 3))
testSplitZeros = TestCase (assertEqual "testSplitZeros" [[0],[0, 0, 0]] (ListasSolucao.split [0, 0, 0, 0] 1))
testSplitRepetidos = TestCase (assertEqual "testSplitRepetidos" [[2, 2],[2]] (ListasSolucao.split [2, 2, 2] 2))

testsSplit = TestList [testSplitVazia, testSplitUmElemento, testSplitImpar,
 testSplitPar, testSplitNegativo, testSplitZeros, testSplitRepetidos]

testSliceUmElemento = TestCase (assertEqual "testSliceUmElemento" [5] (slice [5] 1 1) )
testSliceImpar = TestCase (assertEqual "testSliceImpar" [3, 25] (slice [3, 25, 3] 1 2))
testSlicePar = TestCase (assertEqual "testSlicePar" [12] (slice [13, 12, 8, 12] 2 2) )
testSliceNegativo = TestCase (assertEqual "testSliceNegativo" [(-5), (-3)] (slice [(-5), (-3), (-8)] 1 2))
testSliceZeros = TestCase (assertEqual "testSliceZeros" [0, 0, 0] (slice [0, 0, 0, 0] 1 3))
testSliceRepetidos = TestCase (assertEqual "testSliceRepetidos" [2, 2, 2] (slice [2, 2, 2, 2] 2 4))

testsSlice = TestList [testSliceUmElemento, testSliceImpar,
 testSlicePar, testSliceNegativo, testSliceZeros, testSliceRepetidos]

testInsertAtVazia = TestCase (assertEqual "testInsertAtVazia" [0] (insertAt 0 1 []))
testInsertAtUmElemento = TestCase (assertEqual "testInsertAtUmElemento" [4, 5] (insertAt 4 1 [5]))
testInsertAtImpar = TestCase (assertEqual "testInsertAtImpar" [3, 25, 5, 3] (insertAt 5 3 [3, 25, 3]))
testInsertAtPar = TestCase (assertEqual "testInsertAtPar" [13, 2, 12, 8, 12] (insertAt 2 4 [13, 12, 8, 12]))
testInsertAtNegativo = TestCase (assertEqual "testInsertAtNegativo" [(-5), (-9), (-3), (-8)] (insertAt (-9) 2 [(-5), (-3), (-8)]))
testInsertAtZeros = TestCase (assertEqual "testInsertAtZeros" [0, 0, 0, 0, 0] (insertAt 0 5 [0, 0, 0, 0]))
testInsertAtRepetidos = TestCase (assertEqual "testInsertAtRepetidos" [2, 2, 2, 2, 2] (insertAt 2 4 [2, 2, 2, 2]) )

testsInsertAt = TestList [testInsertAtVazia, testInsertAtUmElemento, testInsertAtImpar,
 testInsertAtPar, testInsertAtNegativo, testInsertAtZeros, testInsertAtRepetidos]

testMinListVazia = TestCase (assertEqual "testMinListVazia" undefined (minList ([] :: [Int])) )
testMinListUmElemento = TestCase (assertEqual "testMinListUmElemento" 5 (minList [5]) )
testMinListImpar = TestCase (assertEqual "testMinListImpar" 3 (minList [13, 25, 3]) )
testMinListPar = TestCase (assertEqual "testMinListPar" 2 (minList [13, 2, 8, 12]) )
testMinListNegativo = TestCase (assertEqual "testMinListNegativo" (-3) (minList [(-5), (-3), (-8)]) )
testMinListZeros = TestCase (assertEqual "testMinListZeros" 0 (minList [0, 0, 0, 0]) )
testMinListRepetidos = TestCase (assertEqual "testMinListRepetidos" 2 (minList [2, 2, 2]) )

testsMinList = TestList [testMinListVazia, testMinListUmElemento, testMinListImpar,
 testMinListPar, testMinListNegativo, testMinListZeros, testMinListRepetidos]

testMaxListVazia = TestCase (assertEqual "testMaxListVazia" undefined (maxList []) )
testMaxListUmElemento = TestCase (assertEqual "testMaxListUmElemento" 5 (maxList [5]) )
testMaxListImpar = TestCase (assertEqual "testMaxListImpar" 25 (maxList [13, 25, 3]) )
testMaxListPar = TestCase (assertEqual "testMaxListPar" 13 (maxList [13, 2, 8, 12]) )
testMaxListNegativo = TestCase (assertEqual "testMaxListNegativo" (-3) (maxList [(-5), (-3), (-8)]) )
testMaxListZeros = TestCase (assertEqual "testMaxListZeros" 0 (maxList [0, 0, 0, 0]) )
testMaxListRepetidos = TestCase (assertEqual "testMaxListRepetidos" 2 (maxList [2, 2, 2]) )

testsMaxList = TestList [testMaxListVazia, testMaxListUmElemento, testMaxListImpar,
 testMaxListPar, testMaxListNegativo, testMaxListZeros, testMaxListRepetidos]

testMySumVazia = TestCase (assertEqual "testMySumVazia" [] (mySum ([] :: [Int])) )
testMySumUmElemento = TestCase (assertEqual "testMySumUmElemento" 5 (mySum [5]) )
testMySumImpar = TestCase (assertEqual "testMySumImpar" 51 (mySum [13, 25, 3]) )
testMySumPar = TestCase (assertEqual "testMySumPar" 35 (mySum [13, 2, 8, 12]) )
testMySumNegativo = TestCase (assertEqual "testMySumNegativo" (-16) (mySum [(-5), (-3), (-8)]) )
testMySumZeros = TestCase (assertEqual "testMySumZeros" 0 (mySum [0, 0, 0, 0]) )
testMySumRepetidos = TestCase (assertEqual "testMySumRepetidos" 6 (mySum [2, 2, 2]) )

testsMySum = TestList [testMySumVazia, testMySumUmElemento, testMySumImpar,
 testMySumPar, testMySumNegativo, testMySumZeros, testMySumRepetidos]

testBuildPalindromeVazia = TestCase (assertEqual "testBuildPalindromeVazia" [] (buildPalindrome ([] :: [Int])))
testBuildPalindromeUmElemento = TestCase (assertEqual "testBuildPalindromeUmElemento" [5, 5] (buildPalindrome [5]) )
testBuildPalindromeImpar = TestCase (assertEqual "testBuildPalindromeImpar" [13, 25, 3, 3, 25, 13] (buildPalindrome [13, 25, 3]) )
testBuildPalindromePar = TestCase (assertEqual "testBuildPalindromePar" [13, 2, 8, 12, 12, 8, 2, 13] (buildPalindrome [13, 2, 8, 12]) )
testBuildPalindromeNegativo = TestCase (assertEqual "testBuildPalindromeNegativo" [(-5), (-3), (-8), (-8), (-3), (-5)] (buildPalindrome [(-5), (-3), (-8)]) )
testBuildPalindromeZeros = TestCase (assertEqual "testBuildPalindromeZeros" [0, 0, 0, 0, 0, 0, 0, 0] (buildPalindrome [0, 0, 0, 0]) )
testBuildPalindromeRepetidos = TestCase (assertEqual "testBuildPalindromeRepetidos" [2, 2, 2, 2, 2, 2] (buildPalindrome [2, 2, 2]) )

testsBuildPalindrome = TestList [testBuildPalindromeVazia, testBuildPalindromeUmElemento, testBuildPalindromeImpar,
 testBuildPalindromePar, testBuildPalindromeNegativo, testBuildPalindromeZeros, testBuildPalindromeRepetidos]

testMeanVazia = TestCase (assertEqual "testMeanVazia" undefined (mean []) )
testMeanUmElemento = TestCase (assertEqual "testMeanUmElemento" 5 (mean [5]) )
testMeanImpar = TestCase (assertEqual "testMeanImpar" 17 (mean [13, 25, 3]) )
testMeanPar = TestCase (assertEqual "testMeanPar" 8.75 (mean [13, 2, 8, 12]) )
testMeanNegativo = TestCase (assertEqual "testMeanNegativo" (-6.0) (mean [(-5), (-5), (-8)]) )
testMeanZeros = TestCase (assertEqual "testMeanZeros" 0 (mean [0, 0, 0, 0]) )
testMeanRepetidos = TestCase (assertEqual "testMeanRepetidos" 2 (mean [2, 2, 2]) )

testsMean = TestList [testMeanVazia, testMeanUmElemento, testMeanImpar,
 testMeanPar, testMeanNegativo, testMeanZeros, testMeanRepetidos]

testMyAppendVaziaComUM = TestCase (assertEqual "testMyAppendVazia" [2] (myAppend [] [2]) )
testMyAppendUmElemento = TestCase (assertEqual "testMyAppendUmElemento" [5, 1] (myAppend [5] [1]) )
testMyAppendImpar = TestCase (assertEqual "testMyAppendImpar" [13, 25, 3, 1, 9] (myAppend [13, 25, 3] [1, 9]) )
testMyAppendPar = TestCase (assertEqual "testMyAppendPar" [13, 2, 8, 12, 6, 9, 9] (myAppend [13, 2, 8, 12] [6, 9, 9]) )
testMyAppendNegativo = TestCase (assertEqual "testMyAppendNegativo" [(-5), (-3), (-8), (-8), (-2)] (myAppend [(-5), (-3), (-8)] [(-8), (-2)]) )
testMyAppendZeros = TestCase (assertEqual "testMyAppendZeros" [0, 0, 0, 0, 0, 0] (myAppend [0, 0, 0, 0] [0, 0]) )
testMyAppendRepetidos = TestCase (assertEqual "testMyAppendRepetidos" [2, 2, 2, 2 , 2, 2] (myAppend [2, 2, 2] [2 , 2, 2]) )

testsMyAppend = TestList [testMyAppendUmElemento, testMyAppendImpar,
 testMyAppendPar, testMyAppendNegativo, testMyAppendVaziaComUM, testMyAppendZeros, testMyAppendRepetidos]

tests = TestList [testsMeuLast, testsPenultimo, testsElementAt, testsMeuLength, testsMeuReverso, testsIsPalindrome,
  testsCompress, testsCompact, testsEncode, testsSplit, testsSlice, testsInsertAt, testsMinList, testsMySum,
   testsMaxList, testsBuildPalindrome, testsMean, testsMyAppend]

reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = do
  return (count+1)

myPutText = PutText reportMsg 0  :: PutText Int

instance ToJSON Counts where
  toJSON (Counts cases tried errors failures) = object
    [ pack "totalTestes" .= show tried
    , pack "erros" .= show errors
    , pack "falhas" .= show failures
    , pack "passaram" .= show (tried - errors - failures)
    ]

main = do
  (testCounts, msgCount) <- runTestText myPutText tests
  let a = (Aeson.encode testCounts)
  B.putStrLn $ a
  I.writeFile "test-output.json" (encodeToLazyText testCounts)
  Prelude.putStrLn "Arquivo json criado"
  return ()
