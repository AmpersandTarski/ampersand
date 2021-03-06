﻿{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Ampersand.Test.Parser.ArbitraryTree () where

import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import Ampersand.Input.ADL1.Lexer ( keywords, isSafeIdChar ) 
import           RIO.Char
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import           Test.QuickCheck hiding (listOf1)
import           Test.QuickCheck.Instances ()

-- Useful functions to build on the quick check functions

-- | Generates a simple ascii character
printable :: Gen Char
printable = suchThat arbitrary isValid
    where isValid x = isPrint x && isAscii x

-- | Generates a simple string of ascii characters
safeStr :: Gen Text
safeStr = (T.pack <$> listOf printable) `suchThat` noEsc

-- Generates a simple non-empty string of ascii characters
safeStr1 :: Gen Text
safeStr1 = safeStr `suchThat` (not.T.null)

noEsc :: Text -> Bool
noEsc = not . T.any ( == '\\')

listOf1 :: Gen a -> Gen (NE.NonEmpty a)
listOf1 p = (NE.:|) <$> p <*> listOf p

-- Generates a filePath
safeFilePath :: Gen FilePath
safeFilePath = T.unpack <$> safeStr

-- Genrates a valid ADL identifier
identifier :: Gen Text
identifier = (T.cons <$> firstChar <*> (T.pack <$> listOf restChar))
               `suchThat` noKeyword
  where
    firstChar :: Gen Char
    firstChar = arbitrary `suchThat` isAscii `suchThat` isSafeIdChar True
    restChar :: Gen Char
    restChar = arbitrary `suchThat` isAscii `suchThat` isSafeIdChar False 


    noKeyword :: Text -> Bool
    noKeyword x = x `notElem` map T.pack keywords


-- Genrates a valid ADL upper-case identifier
upperId :: Gen Text
upperId = identifier `suchThat` startUpper
    where startUpper txt = case T.uncons txt of
            Nothing -> False
            Just (h,_) -> isUpper h

-- Genrates a valid ADL lower-case identifier
lowerId :: Gen Text
lowerId = identifier `suchThat` startLower
    where startLower txt = case T.uncons txt of
            Nothing -> False
            Just (h,_) ->  isLower h

-- Generates an object
objTermPrim :: Bool -> Int -> Gen (P_BoxItem TermPrim)
objTermPrim isTxtAllowed 0 = objTermPrim isTxtAllowed 1 -- minimum of 1 sub interface
objTermPrim isTxtAllowed i =
  makeObj isTxtAllowed genPrim ifc genView i
    where ifc :: Int -> Gen (P_SubIfc TermPrim)
          ifc n = subIfc (objTermPrim True) (n`div`2)
          --TODO: The view is never tested like this
          genView = pure Nothing
          genPrim :: Gen TermPrim
          genPrim = PNamedR <$> arbitrary


makeObj :: Bool -> Gen a -> (Int -> Gen (P_SubIfc a)) -> Gen (Maybe Text) -> Int -> Gen (P_BoxItem a)
makeObj isTxtAllowed genPrim ifcGen genView n =
  oneof $ (P_BxExpr <$> identifier  <*> arbitrary <*> term <*> arbitrary <*> genView <*> ifc)
         :[P_BxTxt  <$> identifier  <*> arbitrary <*> safeStr | isTxtAllowed]
     where term = Prim <$> genPrim
           ifc  = if n == 0 then pure Nothing
                  else Just <$> ifcGen (n`div`2)
        
genIfc :: Arbitrary a => Int -> Gen (P_SubIfc a)
genIfc = subIfc $ makeObj True arbitrary genIfc (pure Nothing)

subIfc :: (Int -> Gen (P_BoxItem a)) -> Int -> Gen (P_SubIfc a)
subIfc objGen n 
    | n == 0 = P_InterfaceRef <$> arbitrary <*> arbitrary <*> identifier
    | otherwise = P_Box  <$> arbitrary <*> arbitrary <*> vectorOf n (objGen$ n`div`2)

instance Arbitrary BoxHeader where
    arbitrary = BoxHeader <$> arbitrary <*> pure "BOX" <*> listOf arbitrary
    
instance Arbitrary TemplateKeyValue where
    arbitrary = TemplateKeyValue 
                 <$> arbitrary 
                 <*> identifier `suchThat` startsWithLetter
                 <*> liftArbitrary safeStr1
       where startsWithLetter :: Text -> Bool
             startsWithLetter t = case T.uncons t of
                      Nothing -> False
                      Just (h,_) -> isLetter h

--- Now the arbitrary instances
instance Arbitrary P_Cruds where
    arbitrary = P_Cruds <$> arbitrary
                        <*> (T.pack <$> suchThat (sublistOf "cCrRuUdD") isCrud)
      where isCrud str = L.nub (map toUpper str) == map toUpper str

instance Arbitrary Origin where
    arbitrary = pure OriginUnknown

instance Arbitrary P_Context where
    arbitrary = PCtx
       <$> identifier   -- name
       <*> arbitrary -- pos
       <*> arbitrary -- lang
       <*> arbitrary -- markup
       <*> arbitrary -- patterns
       <*> arbitrary -- rules
       <*> arbitrary -- relations
       <*> arbitrary -- concepts
       <*> arbitrary -- identities
       <*> arbitrary -- role rules
       <*> arbitrary -- representation
       <*> arbitrary -- views
       <*> arbitrary -- gen definitions
       <*> arbitrary -- interfaces
       <*> arbitrary -- purposes
       <*> arbitrary -- populations
       <*> arbitrary -- generic meta information

instance Arbitrary MetaData where
    arbitrary = MetaData <$> arbitrary <*> safeStr <*> safeStr

instance Arbitrary P_RoleRule where
    arbitrary = Maintain <$> arbitrary <*> arbitrary <*> listOf1 identifier

instance Arbitrary Representation where
    arbitrary = Repr <$> arbitrary 
                     <*> arbitrary `suchThat` noOne
                     <*> arbitrary

instance Arbitrary TType where
    arbitrary = elements . filter (TypeOfOne /=) $ [minBound..]

instance Arbitrary Role where
    arbitrary =
      oneof [ Role    <$> identifier
            , Service <$> identifier
            ]

instance Arbitrary P_Pattern where
    arbitrary = P_Pat <$> arbitrary <*> identifier  <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_Relation where
    arbitrary = P_Relation 
         <$> lowerId
         <*> arbitrary
         <*> arbitrary
         <*> listOf safeStr1 `suchThat` (\xs -> 3 <= length xs)
         <*> arbitrary
         <*> arbitrary

instance Arbitrary a => Arbitrary (Term a) where
    arbitrary = do lv <- choose (0,6)
                   sized (genTerm lv)

genTerm :: Arbitrary a => Int -> Int -> Gen (Term a)
genTerm lv n = if n == 0
               then Prim <$> arbitrary
               else oneof options
    where gen :: Arbitrary a => Int -> Gen (Term a)
          gen l = genTerm l (n`div`2)

          options :: Arbitrary a => [Gen (Term a)]
          options = concat $ drop lv levels

          levels :: Arbitrary a => [[Gen (Term a)]]
          levels = [
            -- level 0: pRule
            [PEqu <$> arbitrary <*> gen 1 <*> gen 1,
             PInc <$> arbitrary <*> gen 1 <*> gen 1],
            -- level 1: pTerm
            [PIsc <$> arbitrary <*> gen 2 <*> gen 2,
             PUni <$> arbitrary <*> gen 2 <*> gen 2],
            -- level 2: pTrm2
            [PDif <$> arbitrary <*> gen 3 <*> gen 3],
            -- level 3: pTrm3
            [PLrs <$> arbitrary <*> gen 4 <*> gen 4,
             PRrs <$> arbitrary <*> gen 4 <*> gen 4,
             PDia <$> arbitrary <*> gen 4 <*> gen 4],
            -- level 4: pTrm4
            [PCps <$> arbitrary <*> gen 5 <*> gen 5,
             PRad <$> arbitrary <*> gen 5 <*> gen 5,
             PPrd <$> arbitrary <*> gen 5 <*> gen 5],
            -- level 5: pTrm5
            [PKl0 <$> arbitrary <*> gen 6,
             PKl1 <$> arbitrary <*> gen 6,
             PFlp <$> arbitrary <*> gen 6,
             PCpl <$> arbitrary <*> gen 6],
            -- level 6: pTrm6
            [PBrk <$> arbitrary <*> gen 1,
             Prim <$> arbitrary]]

instance Arbitrary TermPrim where
    arbitrary = oneof 
        [ PI      <$> arbitrary
        , Pid     <$> arbitrary <*> arbitrary
        , Patm    <$> arbitrary <*> arbitrary <*> arbitrary
        , PVee    <$> arbitrary
        , Pfull   <$> arbitrary <*> arbitrary <*> arbitrary
        , PNamedR <$> arbitrary
        ]

instance Arbitrary a => Arbitrary (PairView (Term a)) where
    arbitrary = PairView <$> arbitrary
               
instance Arbitrary a => Arbitrary (PairViewSegment (Term a)) where
    arbitrary = oneof 
        [ PairViewText <$> arbitrary <*> safeStr
        , PairViewExp  <$> arbitrary <*> arbitrary <*> sized(genTerm 1) -- only accepts pTerm, no pRule.
        ]

instance Arbitrary a => Arbitrary (PairViewTerm a) where
    arbitrary = PairViewTerm <$> arbitrary

instance Arbitrary a => Arbitrary (PairViewSegmentTerm a) where
    arbitrary = PairViewSegmentTerm <$> arbitrary

instance Arbitrary SrcOrTgt where
    arbitrary = elements [minBound..]

instance Arbitrary a => Arbitrary (P_Rule a) where
    arbitrary = P_Rule 
        <$> arbitrary
        <*> identifier
        <*> sized (genTerm 0) -- rule is a term level 0
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary PConceptDef where
  arbitrary =
    PConceptDef <$> arbitrary <*> identifier <*> arbitrary
      <*> arbitrary
      <*> identifier
instance Arbitrary PCDDef where
    arbitrary = oneof 
      [ PCDDefLegacy <$> safeStr <*> safeStr
      , PCDDefNew <$> arbitrary
      ]
instance Arbitrary PAtomPair where
    arbitrary = PPair <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P_Population where
    arbitrary = oneof 
        [ P_RelPopu 
             <$> arbitrary `suchThat` noOne
             <*> arbitrary `suchThat` noOne
             <*> arbitrary
             <*> arbitrary
             <*> arbitrary
        , P_CptPopu 
             <$> arbitrary
             <*> arbitrary `suchThat` notIsOne
             <*> arbitrary
        ]

instance Arbitrary P_NamedRel where
    arbitrary = PNamedRel <$> arbitrary <*> lowerId <*> arbitrary

instance Arbitrary PAtomValue where
  -- Arbitrary must produce valid input from an ADL-file, so no Xlsx stuff allowed here,
  -- otherwise it is likely that Quickcheck will fail because of it.
    arbitrary = oneof
       [ScriptString <$> arbitrary <*> safeStr `suchThat`  stringConstraints,
        ScriptInt <$> arbitrary <*> arbitrary `suchThat` (0 <= ) ,
        ScriptFloat <$> arbitrary <*> arbitrary `suchThat` (0 <= ) ,
        ScriptDate <$> arbitrary <*> arbitrary,
--        ScriptDateTime <$> arbitrary <*> arbitrary, --TODO #1090 Show of ScriptDateTime doesn't pass the roundtrip.
        ComnBool <$> arbitrary <*> arbitrary
       ]
     where stringConstraints :: Text -> Bool
           stringConstraints = all isValid . T.unpack
           isValid :: Char -> Bool
           isValid c = c `notElem` ['\'', '"', '\\']
instance Arbitrary P_Interface where
    arbitrary = P_Ifc <$> arbitrary
                      <*> identifier
                      <*> arbitrary
                      <*> sized (objTermPrim False)
                      <*> arbitrary
                      <*> safeStr

instance Arbitrary a => Arbitrary (P_SubIfc a) where
    arbitrary = sized genIfc

instance Arbitrary P_IdentDef where
    arbitrary = P_Id <$> arbitrary 
                     <*> identifier
                     <*> arbitrary `suchThat` notIsOne
                     <*> arbitrary

instance Arbitrary P_IdentSegment where
    arbitrary = P_IdentExp <$> sized (objTermPrim False)

instance Arbitrary a => Arbitrary (P_ViewD a) where
    arbitrary = P_Vd <$> arbitrary <*> identifier <*> arbitrary
                    <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ViewHtmlTemplate where
    arbitrary = ViewHtmlTemplateFile <$> safeFilePath

instance Arbitrary a => Arbitrary (P_ViewSegment a) where
    arbitrary = P_ViewSegment <$> (Just <$> identifier) <*> arbitrary <*> arbitrary 
instance Arbitrary a => Arbitrary (P_ViewSegmtPayLoad a) where
    arbitrary =
        oneof [ P_ViewExp  <$> sized(genTerm 1) -- only accepts pTerm, no pRule.
              , P_ViewText <$> safeStr
              ]

instance Arbitrary PPurpose where
    arbitrary = PRef2 <$> arbitrary <*> arbitrary <*> arbitrary <*> listOf safeStr1

instance Arbitrary PRef2Obj where
    arbitrary =
        oneof [
            PRef2ConceptDef <$> identifier,
            PRef2Relation <$> arbitrary,
            PRef2Rule <$> identifier,
            PRef2IdentityDef <$> identifier,
            PRef2ViewDef <$> identifier,
            PRef2Pattern <$> identifier,
            PRef2Interface <$> identifier,
            PRef2Context <$> identifier
        ]

instance Arbitrary PMeaning where
    arbitrary = PMeaning <$> arbitrary

instance Arbitrary PMessage where
    arbitrary = PMessage <$> arbitrary

instance Arbitrary P_Concept where
    arbitrary = frequency 
      [ (100, PCpt <$> upperId)
      , (  1, pure P_ONE)
      ]

instance Arbitrary P_Sign where
    arbitrary = P_Sign <$> arbitrary <*> arbitrary

instance Arbitrary PClassify where
    arbitrary = PClassify 
         <$> arbitrary 
         <*> arbitrary `suchThat` notIsOne
         <*> arbitrary `suchThat` noOne
     
instance Arbitrary Lang where
    arbitrary = elements [minBound..]

instance Arbitrary P_Markup where
    arbitrary = P_Markup <$> arbitrary <*> arbitrary <*> safeStr `suchThat` noEndMarkup
     where 
       noEndMarkup :: Text -> Bool
       noEndMarkup = not . T.isInfixOf "+}"

instance Arbitrary PandocFormat where
    arbitrary = elements [minBound..]

instance Arbitrary Prop where
    arbitrary = elements [minBound..]


noOne :: Foldable t => t P_Concept -> Bool
noOne = all notIsOne
notIsOne :: P_Concept -> Bool
notIsOne = (P_ONE /= )        
