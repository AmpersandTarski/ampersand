module Ampersand.Options.Utils where

import Ampersand.Basics
import Options.Applicative
import qualified RIO.Char as C

-- | If argument is True, hides the option from usage and help
hideMods :: Bool -> Mod f a
hideMods hide = if hide then internal <> hidden else idm

-- Common parsers:

outputLanguageP :: Parser (Maybe Lang)
outputLanguageP =
  f
    <$> strOption
      ( long "language"
          <> metavar "OUTPUTLANGUAGE"
          <> value "language of the context of the model"
          <> help
            ( "Pick 'NL' for Dutch or 'EN' for English, as the "
                <> "language to be used in your output. Without this "
                <> "option, output is written in the language of your "
                <> "context."
            )
      )
  where
    f :: String -> Maybe Lang
    f l = case map C.toUpper l of
      "NL" -> Just Dutch
      "UK" -> Just English
      "US" -> Just English
      "EN" -> Just English
      _ -> Nothing

outputFileP :: String -> Parser FilePath
outputFileP deflt =
  strOption
    ( long "to"
        <> metavar "OUTPUTFILE"
        <> value deflt
        <> showDefault
        <> help "Name of the file where the output is written to."
    )
