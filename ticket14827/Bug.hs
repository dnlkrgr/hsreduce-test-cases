{-# language OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text ( Text(..) )

longestCommonSubstring :: [Text] -> Text
longestCommonSubstring entries
  = go [""] ""
  where
      go :: [Text] -> Text -> Text
      go [] longest = longest
      go current@(s : _) _
        = go (filter substringOfAll $ concatMap step current) s
      substringOfAll :: Text -> Bool
      substringOfAll s = all (\ e -> T.isInfixOf s e) entries
      step :: Text -> [Text]
      step s = map (\ c -> T.cons c s) ['A', 'C', 'G', 'T']

main :: IO ()
main
  = do let lss = longestCommonSubstring contents
       print lss

contents :: [Text]
contents
  = ["TAGCCGACGTTTTCACTCGTTCCCATGCATTATGCAGCTATTTCCAGTAATTGTTCCCCGCTCGATAAGGTGTCCAAGGACAAATCTTGAAACATATCGACGCTTGACAGAACCCGGATTCCATCGGCGGTGTTCACCTAGGGCACCTATCGTGTTTCCACTAGAGTAATCACAACAATTGAAAAGTAACTCGTGTTTCCCTTTGTGCCGGGCGAACGCTACGCCCCCAGCTACTACACCAACGTTAATCGAAGACCTTCTTGACAGTTGGTAGCACATACCCTACGCTGGGTACGAGCTGGAGAGCTGAAACCCGACCCTATGAGAATCCATACTTCAGACTGTATCGTTGTCCATATAATGGACACGGGGTACTTTTGGCATGATATTGTAATTTGTCATGCGATTGCCTCAGTATATCCCTGCTTACACGTGCAGAAAATATGTCCCATTTTACACCGGTCCACCATAACAATTATGCAACTTGTCAGTCGGGGTAGGCTCGGTCTCTGCGTCTAGCGGTGTGAATTACGCGTCCCACCTGCACCAACCACTGACTTTAGGGATCTTTGGGTATCGTGCATCTTGATTCGGTTCTGCCGGAGTTTTCTTAGCAACCAAAACACGGCACATCCAGCCATTTCTTTAACAGTTGAGAGTGTTAGGCCTAACAAATTTACGAGCGAAAATATGAGCTAAGACGAAACGAGACGACTGAAGAGACCCTACGGACGAGCGCCTTGAATATTGTCACCCCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTCGCCGAGTTTCTACCTGATGCGAGGCCTAAGAGGCAGTTGTGTAAAGGAAAATATTCCAAGCTT",
     "TGCCCGAATTAGTCTAGCGAGTCAGGGTCCTCTCTCTAACCCCTCCCCTTCATAACGATACCAGGCCTCGTTGTCACTTCTGTTGCACAGGCGATGCCCGGCATCAAAGCTGGTGAGCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTATCGATCAGGTATGATAAAATGACCTGCCCCTGAAGGACATGAACGTGCACAGGGTAGAACTGCATGGGTTCCCCTGGGGTGGGTCGGGGACACAGTTGATGGCTGCTAAAGCGTTATCTGCGCTTGCGAGCTACAGCCCAACTTGGAACAAAGCGCACGATATTCGTGTGCTCGTATCGCAGTCACGTGTTATTTCGCTCTAGGGTCAACAGATTAAAACCCATTGCTAGATCATGGAAGACGACCTCTGCATCCCGTCGCTTAGCATTTCGAGCCACTTCTACGCACCGTTTACAAGACACAGAGAACTGGGACTCCAGGCCGGGCCTCAGTAAAGTGACGCAGGCTTAATCGGTCAGGCTGCCAGGCCCAAGGCTCGATAGCGATATCTTCTTTTACCTACTCAACTCTTCTGACCTTTGGAATCGTACCCTATGCTTAATATTCTTTCGACCCTTAGCTTAAATCAGCACTACTGGAAGCGACATTGAGTCCGCCTCCGTTCATGACCGCATGCATCAGGTATCCCAAGCACGTTCGCGAACTCCCTGTGCCTCTCGCGGTTTACTGCCACCCAAGTGCGATTATTACAACATACGTTACAGGGGTAGTTAAATAAATAAACTTGGCGCATTCGCGGTGTGTCCCGGCAATACCTGATCGTCCCCCATCCACAGGTGGTGCAGAGTGGGTAGGCGGGGG",
     "ATAAGTCCCCACAGCTGTTCAAACGCTCTTCGCGACGTAAGCTGTCAAAAGGAGTGGGTTGCCGATGTTCAGTAAGCGTAACCTATAAGATAGATCGTCGGTATGTGCAAAGGTCAAGGAGATAGAGTTAAGCAATTTATCGTGGCCTTGCGACGTCTTCTGCGCTCCAGATGCGCATCCCTAAACACTCGATATATGTCCAGACAGTATCTTGTGACACCATTGTGCGATCTCTCGCACAATCGGCGTTACTTCAGTTGGTGTTATGTGGGTTACTCAGTTCAGGACAGTACAGACAACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTGACGGGGGTTGCACGACAGTCCCGGGCGTTGTTTACGCTACTTTATCACGGTATCCTGCGTGCGCCCCACTGCAGTTCGGGCGAGCTGACCGGCCTTACCCCTTTGGTACCCTTGACTCCTATCTTGGGTAAGGTCGTGTGAGATTAAATTTACTGAGGCTTCCCGAGGCGCACAGTTCGCCCCGCGTTGAATTTCGGGCAATTAGATTATACATGGTGTGGAAAGGTCCGTGAAAGAATAACCTATGGGGCATACTCGAATTCCGAGTATACTGGCACAAGCCTGACACGGCGGTTAGTAATCCTCTTTTTGTCGCGTCCGCCATCTTTCCTAAAGCTGCGCATCAAGGCGTCCGAAGGATAATAAGTCGAGATCACAGATATTAAGAGCAGGGGAATTAAACCCTTATGTGTGTCTTCTCTCTAGAAACATGAGAACAGCGTTACACTGCGACTAATCTCATCCAAACCTGCAGAACTGGGCTGCGAGCTGATGTATCTCGGTGTAGTCGACGACCTCA",
     "CCCGGTAAAGAAACTCGTACAGTCCAATAGTCGGCTGGCCGGCTGGCAATTACTAGGGGCAAGTGCGGGGCCATATTTATTGTATCTCCGGGTTCCGACCTAGGAGGGCGTGACATGATCCCCTGCGGTGGCTACTATTCACACACCAATGTATGCGCCCTGAGCCCGTACGTTGTAACGCTCATCATTCCATGGCATCACTGGTTCCGGAACACCATTGGAACTGGTAAGACTCCAGCTTACGTCAAGCTGACCGGGGCGCGCTGAGTGTTCGTCTCAAACAGCTGACTGTTGCCATTCCGCCCGTAGCCTGCGTAAGCCGCATTACCATCTTCCTATTTCTACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTCTCCCTCTGAGCTCGGACGACCCGTATAATTAAGGAAAGAATTAACGACGGTCCTAGGCCCGCACTACCCGTCATGCGCAAATCCAACATGGAGCGTAGTACTGGTGGTTAATATGAGAGCAAGATGTAGATGTCCTAACCAGGCCTCGTAGTGAAGTAAACCTCTGAGTTTTAAGCACCTGGGGCCTACAAGCTTGTATTACTCTTGTGTCTAAGGGACCATTCTGTAACACCCAATTACCGGCCTCGGGGATCTACCAGGCGCCAAAACCTCAGCACTCCTACATTGAGGACCAATCGTGTTCCGAACGCTTGGCTTAACGGATGATATACCCATAAAACAGCTCGGTGTGGAGTCCGTACTATTGTGATCAGTTGGTGCACCACCCCTCACCACATCGAAATCTGATCCACGCACGTGGCAGTCCACCATGACGCAAAAGTCACCGTGTCCGTCTTGTAGGACGATGGCTCTC",
     "CGGTCGGATGCCACTTTGCTAATGAGTGCTTCGACTCCCACTCCACTCGACCGAACGTGGTTGGTAATCCGGTAGATCTCTACCTTTACCGTCTAAGTAGGATCGATACCATGTTCCTGTAGATACAGGGAATCTATTAAGGGCCGATGTTTAGGTAGGTCCCCTTAAGCCCAGGCTACAAGGGTGTGCAGGCTCACTTGTCCGATTTTCTCTGGTTATTCGGAAGCTCTGCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTTTGTTATCTAAAGCCGTAAACTGACATCATTGAATGCTGGGGCTAAAGTTTACGTAAGCAGGCTATTTCAGAAACCTCATCCTAAAACCGCACACCGCTCTAATTCAGGATAGTTGCGGGTGGGCACTGCATGTGCTATATTGACAATCGGTACTGGGTTTAGAGATTTATCATTGATTAAACTACCCAGATGAGATGTCGGCTCTTGCGAATGACCGGCGCCCACACGGTGAAAAAAATGTTAACAAGTCATTCGGCTCAGTATTCTATGGCTTAGTCCCCCCCCACTGCAGGATAATGTACGGTTATAAGAATCATTACCCCGGGCCGAAAGTTTATAATTGCTATGAACTATGAAATAGCCCCTTATATAACAGGATGAACGTGAACGGTTCATCATGTTTAAAACTTGGTGTTATGTTGACGGTAGGCGGGCCTGACACTGTTAGTAGCGGGGCGGGAGTAATCCTAGGCTTAACCTCATAGTCAGAAGAACTTTGGTATTGGCCGCGGCATGGTTCGCTATCGGATATGTTTTTCATATGGCCGTCCCACATAAATGGGCTACCAACTACGAGATCCATTTAGC",
     "AAGTTGACGCAGTATGTTCTTGATTATTACCGCGACTACAAGTCAGTGGCGGCCTACGTCAAGCTCTCTGGGGGGTTGAACTCTGTATTTACTATGTAATACAAACTCGACGTAAGCGGGCGAGACTAACCCGAGCGATTGTCGCGCGCTTTAAAGCAGATCTGCAGTTCGCAATGGTATGCTTTTATTGGGTCTCGTAACAATGCGGGGTTGTCTGCGTCAAGCTCCGTTCTATGGGAATGTCCTGTGGAAGGAGGTTTCGACCATTCACTTGGCATCGAGGGCTAAGGTTCACTAGTGCAGGCAATCCCTGGGGGTGCGCGGATTATCGTAGTGGAGGCTGGGGCGAATCTTTCTGCGGATGGTAGAGGTGGATGACTCGCAGGGTTGGGGGGATAAGAGGATGCCATCTGGCTGCCCCGTATATGCGTTCTTGGACCCGGTTAGGGTCTATCCTTCCCGGGCTTCCAGTACTTTGGCTAGTATGCATTAACAACCGATCGCACTTCATGGGTATTCTGATGACCTACTGCACATTTTTAAGTACAGAAATAGTGCATAAGGCCTGCACGGACTAACTCTCTATACACTTTATGAGGCACATCCGGAGTTCGCTCACGTCTCGCCTGACAGGTTCCGTATCCGAGCCCTCTTGTTTGCGAATATATGGTGGAGACATTTTTTTGCGTAGAAACGGTTGTTTACGGGAACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTTCAGGAACTTGAGTAGTTTCCGATGAGTCATAGGGTGCATAATTTAATATAAGGTACGCCCCAGGATTTTCGTACGTTGAGCGCCCTCGGGGCATCTCAATCCGCCACCC",
     "GTTTAACAACTAGCCCATCAGCCGCAGGAGCTCCATGGTGGTACATAACTTAGAGTCAATGTAGATCGGCTCGAAGCGTTTCCAAGCTACCCCAGGCACATCCCATGCCTCGACGCCCAGCATCTAGACATATTGGCGCACAATCGCAGGCCGTTAAACCCCTTGAAAAGCTATCGTGCGGGTAGCAACACCAACTACCTCGCCTACTGTTATCCCGCAATGAATACTGCAGTTATGGTAGTCAGGTTATATAATGGTAATCACCAGCACTGTGCTGGACCACTCTTGTGGCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTTGGAAATTTCTGATTCCAAACATCCTTCTCAGGCCGCTAATGGAACACGTCGTATTCCGCGCGTAATACCAGAAAATCCCCTTCAGTATGGTCCATGACATCTTGGTCAGCCTACTACCTCCATCGTTCTTGTGACGGGTGGTGCATTCTATGCACTGCGGGCCAGACTTCGCCTGAATGCCGAGGTAACATCTCCGCGGCGAAATAGAGGGTAGTGTTCGCTGAACGCTTTCCGCTCGCCGGAGTTTTGTCTAGCCGAATTAGCAGCTGTTGGACTGGCGCGGGTGATCCAGAGTTCGTTCTGTGTGCTAGGAACACGATGTATCTACTCTGACGTTGGCTAAGGGCAGTCATCAAACAAATTGCTTGGGTCAATTGCCAGGTGCCAAGACCATGGGGCTCGCCATTAAACTTAGAACTCTGAAACCTGAGCCAGTAAACTCCTGAGGATTGTTCAGGTCTTGCAGACTCCCTTTACTCTTAACGAGTCGTATCTCCAAAGGAGGTAGATATGGTGAAATGGGGATAC",
     "TAATCCACTAATAATATGCAACAGCGCTAAAACGGGTTCGGCGTTGACCGAAAGTCCATCCTAGTCCTCAAGGAGTAGAAACTCTGATAAACTCGACTGTTCGCACAATTTGTTCTGACCCTTGTGACCGACTATCGCAACGCATCTTCATTCAACCCCAATATGTTGCAGAATCCTTCGGACGGACGGGGTAGTGGACTACGAGACTACACTGTCTTTTCCTTCCTTTATACCTCCTCGACTAGCGCTCATACACATTAGCCGACTGTTTATTGCACCCTGGACGAGTCCAACTGTAAACGGCCAACTTGGATCGATCTTGGATTCTAGGCACCCGAAGACGAGGATCCGCAGCGGTCAAATTGACTACGCAGCGCCCTAAGAATCTAGTTAATGCATAAACCGAAGTCAGAAAGGAGATATCGTGCACTTACGAATAACAACGCTACGGAACGTAATACATGGTCAGGCGTATTTATTGCGCCTTTCTTTACTACTGAGGAGGCACGCCTACCTATGGACGATACACTCCTGCGAGGTCGGCCGCGCTATGGGGTAGTTTGGTCAAAGGCCAATAAGGGCGCATGAAAACAAGCGATGGTTGGGCAAATCGACTACATGCGCCGATCCAGATTAGTGCTTTCCGTTATACTCGTACGGAATCTCAGCATCCTACCAGACCCTGACACGAAGACTCCTCGATGCTTGGCGTTCACAATGCCCGCAAGGTCTAGGCTCTTGTCGCGCCAGATGCGGAGATGATGCGCGAACCACGAGGCGAATGGTCCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTAGTGTGTGGCGGGATCAGGGGTATAGTCTAGCA",
     "TACTACTCCTATAGGGTCTGCATGATAGAAATATAAAATAGGTCAAAGGTCGATGTGTCTGTCCGCCTGAGCCTGTGGCTATTGTATACGCGAATAGAGTACCGATATACGACCGGCGTACGTCGCCACTGTCATGGCCTTAGATTACTGTTCACAGTCGTACCAAATCTATAACTCGGCACTGGTTCAACACACCCCTTCAGGGCGTGTTTAATTCTATCAAGTTTTGGTTTCAAGAACGAACACTACAGCTGCTGGGATGGCTTGGGTATAGCTCTATAGATACGATCACAGTGGGCCTTCGTTGCCCCTAGTCCGCTTCGTGTAGTGACTTTCGATGTCCCTAGGAAAATGGCCGATTCGCCGTCCAGTGGTGTAGCCTGAGCCCACGGGAGGTGCAAGGTGCCATTTGCTATCAATCGCAAATCTTGCTAAAAAAACGGCTGTCATGGCTAAGTTATTTTTTTGGAGTCATTCCCCCAGCCCACCTTGCAGCCGGATCAATATCCAGCTAGTTAAAGAAAATATCAAGAGGGCCTTGAATAGTAGTGGTGTCAGGGAATAAGATATTTGAACGAACGCCACCTTAGTGTCGAATTCCTTAAGTGGGACTCACCGTTGATGCCCAAGGATCGATGCTATCCTCCCTGGTGATCTATCGACGAGTTTCCAACCATGGGCATCACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTGGTTTTTTGACCCCCGAGAATTTGTCTGTAATCACTCCAATCCCGAAACCTAACCTATGCGTCTCGTTATATGCTAGGGTACGCCTTGTTGAAGAGGCGTAGACTTTTGAGCTAATACAAGCGCTCGCGACTGTG",
     "AATTGCTGGATCATGAACTAAGCTTCAGCCTACACTCGCCGCTACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTCGGCAGTGTTTTAAGAAGTTATTATTCCTCAATGGGGCTCAATTGCCTGCTTAAAGTCTCCCCCATGGCACGGTATCTGACGAACACGTCTTTACGGAGCGCACATTCCATTTCGGCAGCTTTATAGTCTCATGTATCTTCGACAGGGCTACCCACATCAGAAGCGCCGAGATCCACTACAGGCCCGCGGGGTATTCGTAGGTGCGCTATAAGCTCGTTGCTTTGGATGTAACAGAATTCTGGGGCTCAATTCACACATTGGGTCATGGGTCGATTAATCTCGTGCTTAATCACTCCACAATATCCTTCGTGCGTCGCACGGGGGGCCGCGATTCTTAAACAGTTCTCTTGAACGGTCAGGCCCAGAAGAGAAAGTTGGACTTTAAAAATGCAGAATTTGGACCAGGGGATCTCACAGCAGAATAGTGTGGTATACGTCATTTCCCAGTTCCGGTGCTGAGGGGCATACGGCGGATGCCAATAGTCTGTCCAGTACCTCAAAGAGTCTGAATGGCTGGTCCCGGTTCGCCCCACTGTAGGTTAAGCAGTACGCCGTACTTGACACAGTCAATTTGCAATCCGTCGCAACGGTGTGCCAAGGATGGATTCTGAGGCATGTCCACCGTTCCCAATTCCTTGCGCTTAATAATGGGCGCGTTACATTGTGTACATCCACTCTTCATGAGCCAACCTAATAACTCGTTAGCAGGTACTGTCATGGTTTCGTGAAAGTCCAAGAGACCCGTCAGAAGCTTCCAGTGAGGCTGCGCCGCCAC",
     "GTTGTGACCTCTTCGGTCGGAGGCTATTGAGGGTAAGGGTTGAGTATATTAACCACCAAATGCCAGTGACAGGGAGGTGTCTGCCTCTACCCGCTTATTTACGCGGACTATATGCATTAGGTAGTTCCTAACGGTCGTTGAGTATTGTGATCCGCGTGGTGATTAACCACGCTATGAGCTTTCATATAGCCTGTTACGACGACTTGTGTAGTAGTCGAGGTCACTTACCAGTAGCATCTTGGTCGGCGTAAGCGAGCCTAAACGGTATGGACCGGAAGTGGCACGTTCCTGTGACCCTTCATCCTTTGATGGTTAACAGGGACCTAAGAAGCACACACAAGGCAGGACTGCCGTGGGAGATGCTCTGAACACAGGACCTTGGGGATAAGCTAGCAACGGCGGAATGTGCCCAGCAATCCGGATCAAGTACGGTAGATCCAGCCTTGCGCTTGCGGCTGTTCATCGGCACCGATATCTTTTGCCCCGTACTTACCTATGCACCGCGCTACGCCACCTAATAGTTCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTGTTGCTGCACAACTTCTTCAGCGACTAATCAGCTACTTTCTCAGTTAGAGAAGTCTGGGTTTCTTTGCGTATAGCCAGGCGGGTGGTCCGGTATACCGAGTCTACACAGGTGTAGAATCGGATATCAACCCATTCCCCACCGGGTTGGTAATTCGGTAGAAAATAATCCCATAGGTAACATGACCGTGCGTTGTTGCACGGGCGTGCTTGTGTATGCGGCCAAAATCATAGGTTTAGAATTGGTATCAGGGTCTAGCATAGACCTCTACTTAGATAGGATGCGCATAACACGCTTCG",
     "GTATTGATCAGGGTGGACATCTGTCCATACACACCTCTGACAATCCAGCCTCTTAAAAAGAAATCAGCGCCTCGCACCGTTGAGTCCTCCTACGGGATACTTGAGCAGGTGTCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTCTAAGAAAACGTCGCAAGTACCTTTCATGTAAGCAAAAACCGTGTCTTGGACCGGGAAAAGCTGCGGGACGCGCTGATACGGTTTTGGTAGAGACGACTGAGCCAGTACAGCTTATGCGTCACCGAGACCTGTTCCAATGGTTGTCAAGGACACTATATGGTAGGTATCTTGTCTTTTATTAGCGGGAATGATTGTGTACCTTACAGATCCAATCCTCGCGGTTACTCACTAGTCAGCTATGTAGGCCTACTTGCCGCAACATATCTCAGCTCGTGAGAGTTCGCATTCCTATTTCCCAGCGCGGTATACAGGTTTGTCTATTAGGAGAAGATGGTAGGCAGGCGCAATGTAGACACCACGTAATCGCTGCACAGACCATTCCGAGCCGGCCCGAGCCCCCTTAGGCGCTACAATAGAGCTTTTAGAGCTCTGTCTGTCATCTTATCTTTTCTGTGTATCGCTACGAAGGCAGGCAATCTCAACACGTCCATCTATTTGCCAGTGCCTCAGGAAAGGCATGTCATTGGTACCCGGCGCCGTTCGACATACCAGGTATGCGAAGCTAATAGTATATGAAGCATTGCACCGGAACTAGCACAGACAACACTTCGGCCTCGAGCTCCATGATCATGTATCCGGTGAATCATCAGCGTAGCGTCATAACACATCGAGGCTCTCCTGGTTTCGCTTTGCAGACATTTCAGCGT",
     "GGCGGACCTATGCCCCATGTAAGTTTTATAATAGTATCTTCTTATCGGCGCTTTATCTGTCCTTCTCCAGCACAACTGATACGCAAGGAGCAGATACTGCGCGACCTGTTGGCCACATGGATTAACCGGTCCCTTAGAAGTCCCCACGGTGCGGCATGCCCTCGGATCGTTAGCGTATACTCCACTCGAGAGTAAAAGAGTGCGACAAGCGATGAAAGGTCATACATCTGAAAGGGACCCACCGTTGTTGCTTCCAAACATCATCGTAGAAATCGTAATTGTCCTTCCATAATAGGGGCATTGCTCCAGGGCGCGGTTCGCCGGGCTTGGCGCTCTCTATAAAAACTTTGTGACTTTTGTGGACATATGAGGGAACACCCTTGACCGACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTGATAATTTGTTTGCAGACCCCGAACTGTATATGACTCAACCTGGTCGTTCCCAGCAATAAAAGTTCTCAAGGCCTATGATGGGTCTGGTGCCCACTCCTACCATGGTTCTTCGTGAATCTAACTATTGTCACGCTTGGTATGTTTATCCTCCGGGAGACCGTGTTGAACCCCTACTTTCTAAATTTCGAGAAGGTCGGTAACCATCTCTTATCTGTCTGGCACTATTCAGAGCCGCCCGTGCGAATTCTCCCCCATTAGTACGAATCGAGAACCAGCGCGCTCTTACGTGGCCGTATTGCTACATAAACTGAGGATTTGCGAGGCGGCCATTCTTTTCTTCTATGCCATTAGTGTGGAGGTACTGCAAAAATACGCGAAGCTCGACGAGTTTAAGTGGAAGCACCTTAGTTGTGGTTGCCCAAGGCTATGCC",
     "AGATAGGTACGTTAGCGTCTGGAGTCGGCCATCTGGCATATTTTGGCCAATATGTACATAAACCGAAGAAATTACTCATTACAAGGCTGCCCTAAGTTTCCATGAACCCAATCTCTAGAATTGATCACAGAGCCACTCGCCTCCAGGGCAGCGTTGCCGGTATACTCAAAGCGCAGGTAGCTCACTTCCTTGATAAGAAGCGCAAAGCTTCACGAAATTTAGAACATCCGTAAATAGGCCGAGGCGGCCGGGAAACTAAAATTAGTTACATGCGGACTCTCCCAAAACACCCGACCTGGTACTTCGATGATGCGTATTCTTGCTGAACATGGTTCGGATCCTGCCACTAAAACGGTATGGCAGAATAGGGGTACTCAGTTAGGCGAGGGATCCATGTCGGACGCCCGCCAATACCCCCAACATTAACCAGACTCAGTTCTGCGAACAGTATACGAGCTCCCAGGTGGACATCCCGGTTCGAGGACTTCGTCTGCACAGTGAATATGACATGGCGTCCGACTTAAAATTAAACTGGCTATTTGTCTAGAGATGCACTATTGGCGGGATTAGGCAAACTGCTTCGGATAACTTGAGATGGACTCTAAATAATACTCCCCGAGTGTATGTTTGATATGCCCTAACATGCTCAATACACTGGCGCGAAGCCGTAGGCATTGTGTGTCACATGTGATAGGGACCGAGAACGTAGGAGGTTTCGTCGGTGTCATCAATAGACACATATGTGATAGGCCCAGGCATCCTACGCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTCGTAGACTCGGTGACGTCACAGGTCTAGCAACATTTATCCGGCTTCGACAGAACC",
     "ACGAACAGCTGCTAGATTGCGCGAGGGGATGCCGGATTGCGCTTAGCCAGGGGGCTGGCCTCTTACTTGTTGTATGCCTTGGCCGGTGTCAGAGGGCGAGGAGTCGCATTCAACTAAGTCCTATAGCGTGGCTTGCTTGATGGTTGAACGGTTTCTTCGAGTAAAGCCCTGACCCTTAAAGGCCAAGGACTGCGCCGTCAACTTGGCTCGCCGCAAGTCGCTGAGCGCATATCACGCGGCTCAATCTCATACCCAGGATTTAAGTGGACGCGGAATACACCCTCGCGCCCGTTTTTCCGCGTAGACTGCGCAAGTACAGGGGGGGGGGTATACATACGGATTGTTCGGTACAGACCCCGTGGGCGGTGGCTTCGATTTACGGTGTCTGACAGCTCTTGCAAACAGGGCAGTCTAGCGCGACTGCCACGATAGGGCTCATTCCTTCAGCAGCGTATCCATGTCGCGGTCTGAGTCAACCTTAGGGAGGGTTGACCAATCAAGGATAAGAAGACTATTGTCGTTTCTATCAAAGCTTACCAATACGATTTAATTGAGCACAGATTGAACCCCATCTATGGGAAAAATCTCGCACCCGTTGCTATCCCACAATCAACAGAGATCGCGCAGGAGATCCGGACGTTCGTCGAATTGCTCATTGATTGAGCAGCACAAACCTATATTTTCTAAGGGCCCACCCTAGCGCCGTCCCTCGTTTCCTTCTGTCTAAGGCGAAGACTTAATCCTAGGAGGTGTCGCACGAACGAAAGGCGCGCGCGGGCGCATACAAACACCGCCCTCGCGCTAGAAGCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTTTATTCAGAAGC",
     "CGTCGGAAGATTGTACCAAATTTTCTTTAATAGGCGATGAAACTTAGAGAATACTTCCTTTTGGTCAAGTCGTATTCTTAATGGTAGGTCTTTGCGACGGGATCACAGTACTACGCGTCACAAGGGGTCGCTTATTCACTGCAACCGACATCCTCTTAAGCATGCACGCCATATAGTGAGTCAGATAGTTAGTCGGACCTTTTACGCACTATGATAGTAACAGCCGTCTCCGGGTTTCCTGAACCGTCTTCCAATCGTGCGTTCTTATACCGGACTTTGCCGCTATCTTCCTCTAACATGTGCCAACGCAATCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTGTTCTGATAGGCCTCTGTGCAGGAAACAAAACCGGTATCAGGGACTGACAGTAGCGTTAACAGCATAACTCATCTTGAAGCGGCCACCAGAGGGCTTCCTTTCCGATAGTTAGGACTTTTAGCAGGGCTGCTACTCTGGGTGTGACTCTTTGAGCCTCAGTCAAGTGGCAAAATGCCGCACGTCTCTTCATATGTCAACGGATAGCCAACCCATCTCGTTTGCCGCTAAAGACTCATCCTTGAACTTCAACGTCTGCCTTGCAGTATCGCGTCGACCGCATCTATACACTCCCTGACGCCACATGCCGTATACGAATGGTACTATGAGGTAACAGTCGAGGCCCCCGTTTACCAGGTCGACCATCCATAGGAGAACCAGTAACGAATAAGAGCCGCTACTTCCAAAGAACCGTTGCACCGCAATGGATCGTATACGTCTAACTGTACGACGCGCGACGTGCATCCGAACTCCAATGAGAACCGTATTCTACAGCTTTAACCATGGGGA",
     "TTATTCTTAATAAGTCTTGAAGGAGTTCTATATTTCCGATCAATGGGGGAGGGAGAACCGTGTTCAATGATTGGGTGGACCTATCTGTACCCTTCCCGCAGTCCAGCTGCGACCCGAATTGATTTAGTTTAAGAGGGCAATCGGCGGGGATATGGCTTTCCCCGTTGGGCTGAGGAAGATAGGAACGGAACCCATGCCCATATACCCGAATAGCAGGTGCGAGAAACGCTCAGTATTTCATTTAAGATAATACGGGGTACAGCGCGGAACGGGAGTGGCAGCGTTAACCATGGCGTAGGTTAGCAGAGGATTATTAGGCGGGCGCTGCCCCGCCGGCACCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTCGCTCTCAACGTTGTGAAGGAATTCACTCTATATTGACGTGCCCATTCCCTGCGAGCGGATCGAAAAACGTAAATTGGGCTGGGCAATTCCCCTGAGTGCATAAGTGGCAGGTGTCGTTACAAGGCGCTGTGCTACCGAGGGAACTAGTAGGGTTCCCCGGGGACGCTCTTCCACGCTGGTATGTTGACGGTGTTTCTGCTTCTCTGTGTCCGTTTCCCCCTTGTTCGTGACCGAACCAGAACGGGTTCATTGGAGTCGGCCAGTATCGGGCTGTACAAATATAGACCCGTTTCTCTTATGGGTAACCTGACTGTGAGAACGATGTGTACACTACGAGAAAGAGTAGAATGTTTCGGTGCCGCCGTCGCTGGCGTCGTTGCCTTGATAGGATTCCTTTTATGTGATATTTACGAGTCTACTTCAGTGAAGTGTCCAGCAATGTAGGCGATGCTCTCGGTCGTTCGGCTAAGCCACTAGACG",
     "TACCCTTGTAATATTAGCCTTTCCATAATATTACTTCCGCATTGCTGGTGGTAGCTTTTGTATCGCTTGCAAAGCATACTTGACTCCGTCGCGCTATCCGACAGAGCCCCACCAAGCCTCTATGCCCTTTCGCACTAAGCCGAAATGCGTCCCACCTATTGATCCTGCCTCTTTTGTCCCGGCAAGGAGACCGTGCGTTTAGAATTTTCTATAACGGTTCGGTTTTACTAGCATGCGGGTGGCAATAATTACACTGCGAATCTTGTTGCGGTTAAGGCATAAATGGCACAGAATGCAACAAGGCCCTCCTTCCCTCGCCCAACTATGTAGATCCAGTGATTAACAAGAACCGCATAAAATGCGACGAGTTATATCGAGAGGCGCGAAGCAGCGCTAGTTAGCTCGCCCCACGGGTGCCGCACGTACAATCTTGTGGGCCCCTTAATTAGTATGAACTGCTATACCACGGGCAGGCCACGCGCCCCTAATGTTGGTCTCTTGACCAACCTAGGAACTTACCGACTAGAGTTTCTCTAAACATGAATCCATAAGAGAATTTGCAGTAAGCATCATGCGGCGGCCAAACGGTATTTCCCGGCTTGGTTCACTCGGAGAAAGACTACACCGGCACGACCCCGGTACGTTACACTGACTATCGCGACTCCCCCTTAATACAGTAGTACCGCGCTGGGGTTTAGATATCTGTTTATTGTCCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTTAACCATGGAGGAACTTCCAACACTCGAGGGGGCTCGGTATCGATTAACATATTTAAGCCCCGGCGCTAGGTGATACAACAGCACATGTGCGAAGACATAATTCCAG",
     "GTCAACCTGAATTATGCTAACCAGTCCCTTCTACCGGGGCCCATCATGTGTTCCTCCTGGGAATAGCTAGCCTAGCCTATTTAACGTACGTTATGATGAAAGCCGTAACAAGCACAGGTCCAGTTTACATGTCTCGTCTGTCAGTAGTTTTAACGGGCGTACGTACGCTGTGATGTAAAAGGGAAAACCACGAATTCCGGTAGCGGGATGTGGACTGGAAGTGAGCTAACCTTACTATTTTTGGTGGTCAAAGTACCGGTAATGCTCAGACTCCGTAAGATACGCTGTCAACCGTTGGCACGGTGGGTAACTAAGTCTCGACAACCGAAGGCACATCCTAGACAAGCGTAATCTCTTGCGCCTTCTAACAATATTAGCCAGAGCCAGGTCCCTTGCCCCGTATTACCGCGGGAGCACTCATAATCCATACAGACAGGCGACATACCATTGGCATAAGGAAGCATAGGATTAAAAGTGTAGGAGCACCACTCGGATGCAAGACAGTACTCATAAGTCCGAAAAGAGCTGGGCCAGAGACTTAATGGGCGAGACCGCTATATTGGCCAGATTAGGAACTCAACTGACGCTAGTCCGGAAGTTCGCGCGCGGTTGCAAGCGGCTTGATCCTGTGCCATGACGAGCCACTCTCCATGGTGAGGGGGCCCTTCACGAATTTGCTTGGAAGGGGTATATACCAGCCGAAGGGTAGCAAATGAATGCTGACACTTTCCTCGGGCATGGCATTAAGTAGCGGAGTGGGTGCACTCCTGTCCGGCGATACCTGAGTTAGGTGCGGGAGCGAGTTAATGATCGTAGTATCGCCAACTTAGTGAAACAGTTGTAACGGCCGCTCTATGCGAATTATCGTGTTTTCCAGCTCCAATAAGCTACAGGAAGGATGCCTGGCCTTGTCACTTTATTCGGGAACGAATAGACGTGGAGCGTCTTCCGGCCACTCCTAAGATATTCAACTTTAGGACGTATTCACGCCCCTCCGCTG"]
