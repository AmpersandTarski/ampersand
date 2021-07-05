{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Options.DocOptsParser 
   (docOptsParser)
where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.Options.FSpecGenOptsParser
import           Ampersand.Options.Utils
import           Data.Tuple.Extra
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import qualified RIO.List as L
import qualified RIO.Text as T


-- | Command-line parser for the proto command.
docOptsParser :: 
     Parser DocOpts
docOptsParser =
      ( \blackWhite chapters genGraphics fspecFormat fSpecGenOpts
        outputLanguage genLegalRefs -> DocOpts
                { xblackWhite = blackWhite
                , xchapters = chapters
                , xgenGraphics = genGraphics
                , xfspecFormat = fspecFormat
                , x3fSpecGenOpts = fSpecGenOpts
                , x3OutputLanguage = outputLanguage
                , xgenLegalRefs = genLegalRefs
                }
      ) <$> blackWhiteP
        <*> chaptersP
        <*> genGraphicsP
        <*> fSpecFormatP
        <*> fSpecGenOptsParser False
        <*> outputLanguageP
        <*> genLegalRefsP

chaptersP :: Parser [Chapter]
chaptersP =  
   (\intro sharedlang diagnosis conceptualanalysis dataanalysis glossary->
     let x = [intro,sharedlang,diagnosis,conceptualanalysis,dataanalysis,glossary]
     in 
     if length x /= length [c::Chapter | c <- [minBound..]] 
     then --To fix this: make sute all chapters are handled in this function.
          fatal "Not all chapters are implemented thru options." 
     else 
       case both (fmap fst) . L.partition isTrue . filter (isJust . snd) $ x of
         ([],[]) -> [minBound..]
         (xs,[]) -> xs -- Only explicit requested chapters
         ([],ys) -> case [minBound..] L.\\ ys of
                      [] -> exitWith $ PosAndNegChaptersSpecified
                        [ "Are you kidding? do you realy want an empty document?"]
                      cs -> cs -- All chapters exept ys
         (xs,ys) -> let otherChapters = ([minBound..] L.\\ xs) L.\\ ys
                    in if null otherChapters
                       then xs
                       else exitWith $ PosAndNegChaptersSpecified 
                        [ "It is unclear what chapters you want in your document."
                        , "  You want: "<> (T.intercalate ", " . map tshow $ xs)
                        , "  You don't want: "<> (T.intercalate ", " . map tshow $ ys)
                        , "  What about the other chapters: " <> (T.intercalate ", " . map tshow $ otherChapters)<>" ?"
                        , "  Please don't mix `--no-<chapter>` with `--<chapter>`."
                        ]
   ) <$> chapterParser Intro
     <*> chapterParser SharedLang
     <*> chapterParser Diagnosis
     <*> chapterParser ConceptualAnalysis
     <*> chapterParser DataAnalysis
     <*> chapterParser Glossary
  where
    isTrue :: (Chapter,Maybe Bool) -> Bool
    isTrue (_,Just True) = True
    isTrue _             = False
    chapterParser :: Chapter -> Parser (Chapter,Maybe Bool)
    chapterParser chp = (chp,) 
          <$> enableDisableFlags Nothing (Just True) (Just False)
                (show chp) (" printing of chapter "<>show chp<>".") mods
       where 
         mods = help $ "Do or do not include chapter "<>show chp<>" in the generated document."
          

fSpecFormatP :: Parser FSpecFormat
fSpecFormatP = toFormat . T.pack <$> strOption
         (  long "format"
         <> metavar "FORMAT"
         <> completeWith (map (T.unpack . stripF) allFormats)
         <> help "The format in which the output is written."
         )
   where toFormat :: Text -> FSpecFormat
         toFormat s = case filter matches allFormats of
            -- FIXME: The fatals here should be plain parse errors. Not sure yet how that should be done.
            --        See https://hackage.haskell.org/package/optparse-applicative
                   [] -> fatal $ T.unlines
                        ["No matching formats found. Possible formats are:"
                        , "  "<>T.intercalate ", " (map stripF allFormats)
                        ]
                   [f] -> f
                   xs -> fatal $ T.unlines 
                        [ "Ambiguous format specified. Possible matches are:"
                        , "  "<>T.intercalate ", " (map stripF xs)
                        ]
            where
              matches :: FSpecFormat -> Bool
              matches fmt = T.toLower s `T.isPrefixOf` stripF fmt
         stripF :: FSpecFormat -> Text
         stripF fmt = case T.uncons . T.toLower . tshow $ fmt of
                Just ('f',tl) -> tl
                xs -> fatal $ "All formats used to start with an 'F': "<>tshow xs
         allFormats :: [FSpecFormat]
         allFormats = [minBound..]

genGraphicsP :: Parser Bool
genGraphicsP = boolFlags True "graphics"
         "generation of graphics before generating the document."
         mempty

blackWhiteP :: Parser Bool
blackWhiteP = switch
        ( long "blackWhite"
        <> help ("avoid coloring conventions to facilitate readable pictures in "
                <> "black and white.")
        )

genLegalRefsP :: Parser Bool
genLegalRefsP = boolFlags False "legal-refs"
         "generation of a table of legal references in Natural Language chapter of the output document."
         mempty


