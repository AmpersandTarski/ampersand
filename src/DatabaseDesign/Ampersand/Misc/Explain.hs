{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.Explain
    ( string2Blocks
    , blocks2String
    , PandocFormat(..)
    )
where

import Text.Pandoc
import Data.List (isPrefixOf)
import DatabaseDesign.Ampersand.Core.ParseTree      (PandocFormat(..))


-- | use a suitable format to read generated strings. if you have just normal text, ReST is fine.
-- | defaultPandocReader flags should be used on user-defined strings.
string2Blocks :: PandocFormat -> String -> [Block]
string2Blocks defaultformat str
 = case blocks of             -- WHY (SJ, dec 7th, 2011): What is the point of changing Para into Plain?
    [Para is] -> [Plain is]   -- BECAUSE (SJ, dec 7th, 2011): The table of relations in the LaTeX output of ChapterDataAnalysis gives errors when LaTeX is run, because Para generates a newline that LaTeX cannot cope with.
    _         -> blocks       --                              However, this Para is generated by Pandoc, so I'm wondering whether the mistake is in Pandoc? Anyway, this solution is a dirty fix, which I don't like.
   where 
     Pandoc _ blocks = thePandocParser defaultParserState (removeCRs str')
     removeCRs :: String -> String
     removeCRs [] = []
     removeCRs ('\r' :'\n' : xs) = '\n' : removeCRs xs
     removeCRs (c:xs) = c:removeCRs xs
     (thePandocParser,str') = whatParser2UseOnWhatString
     whatParser2UseOnWhatString :: (ParserState -> String -> Pandoc,String)
     whatParser2UseOnWhatString -- = (readRST, str)
        | markDownPrefix `isPrefixOf` str = (readMarkdown, drop (length markDownPrefix) str)
        | reSTPrefix     `isPrefixOf` str = (readRST     , drop (length reSTPrefix)     str)
        | hTMLPrefix     `isPrefixOf` str = (readHtml    , drop (length hTMLPrefix)     str)
        | laTeXPrefix    `isPrefixOf` str = (readLaTeX   , drop (length laTeXPrefix)    str)
        | otherwise   = case defaultformat of
                          Markdown  -> (readMarkdown , str)
                          ReST      -> (readRST , str)
                          HTML      -> (readHtml , str)
                          LaTeX     -> (readLaTeX , str)
       where markDownPrefix = makePrefix Markdown
             reSTPrefix     = makePrefix ReST
             hTMLPrefix     = makePrefix HTML
             laTeXPrefix    = makePrefix LaTeX

makePrefix :: PandocFormat -> String             
makePrefix format = ":"++show format++":"

-- | write [Block] as String in a certain format using defaultWriterOptions
blocks2String :: PandocFormat -> Bool -> [Block] -> String
blocks2String format writeprefix ec 
 = [c | c<-makePrefix format,writeprefix]
   ++ unwords ( lines $ writer defaultWriterOptions (Pandoc (Meta [][][]) ec))
   where writer = case format of
            Markdown  -> writeMarkdown
            ReST      -> writeRST
            HTML      -> writeHtmlString
            LaTeX     -> writeLaTeX
