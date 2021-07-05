{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.PandocAux
      ( writepandoc
      , chptTitle
      , count
      , showMath
      , texOnlyMarginNote
      , commaPandocAnd
      , commaPandocOr
      , outputLang
      )
where
import           Ampersand.ADL1 hiding (Identity)
import           Ampersand.Basics
import           Ampersand.Classes (isFunction)
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.StaticFiles_Generated
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as BL
import           System.Directory
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.PDF (makePDF)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Text.DocTemplates.Internal as PT
import qualified RIO.Map as Map
import Text.DocTemplates
-- | Default key-value pairs for use with the Pandoc template
defaultWriterVariables :: (HasDocumentOpts env) => env -> FSpec -> PT.Context Text -- [(Text , Text)]
defaultWriterVariables env fSpec
  = mkContext $
    [ ("title", (case (outputLang', view chaptersL env) of
                        (Dutch  , [Diagnosis]) -> "Diagnose van "
                        (English, [Diagnosis]) -> "Diagnosis of "
                        (Dutch  , [SharedLang])-> "Taalmodel van "
                        (English, [SharedLang])-> "Shared language of "
                        (Dutch  , _          ) -> "Functioneel Ontwerp van "
                        (English, _          ) -> "Functional Design of "
                )<>name fSpec)
    , ("fontsize", "12pt")   --can be overridden by geometry package (see below)
    , ("lang"    , case outputLang' of
                       Dutch   -> "nl-NL"
                       English -> "en-US")
    , ("papersize", "a4")
    , ("babel-lang", case outputLang' of
                       Dutch   -> "dutch"
                       English -> "english")
    , ("documentclass","report")
    ] <>
    [ ("toc" , "<<TheTableOfContentsShouldGoHere>>") | [Diagnosis] == view chaptersL env ]<>
    [ ("header-includes", T.unlines
         [ "% ============Ampersand specific Begin================="
         , "% First a couple of LaTeX packages are included:"
         , ""
         , "% The glossaries package supports acronyms and multiple glossaries"
         , "\\usepackage[toc]{glossaries}    % Add the glossaries to the table of contents"
         , "% \\makeglossaries" -- Disabled because of warnings in LaTeX. TODO: Have to generate glossaries using Pandoc, not only for LaTeX. 
         , ""
         , "% geometry provides a flexible and easy interface to page dimentions"
         , "\\usepackage[ top=1.5cm, bottom=1.5cm, outer=5cm, inner=2cm"
         , "            , heightrounded, footskip=.5cm"
         , "            , marginparwidth=2.5cm, marginparsep=0.5cm]{geometry}"
         , ""
         , "% breqn – Automatic line breaking of displayed equations"
         , "\\usepackage{breqn}"
         , ""
         , "% colonequals – Colon equals symbols"
         , "\\usepackage{colonequals}"
         , ""
         , "% caption – Customising captions in floating environments"
         , "\\usepackage{caption}"
         , "\\captionsetup{format=plain"
         , "              ,textfont=bf,labelfont=small"
         , "              ,labelsep=none"
         , "              ,labelformat=empty"
         , "              ,width=.85\\textwidth"
         , "              }"
         , ""
         , "% textcomp – LATEX support for the Text Companion fonts -- Disabled because obsolete."
         , "% \\usepackage{textcomp}"
         , ""
         , "% hypcap – Adjusting the anchors of captions"
         , "\\usepackage[all]{hypcap}"
         , ""
         , "% <Adaptation>: The LaTeX commands \\[ and \\], are redefined in the amsmath package, making sure that equations are"
         , "% not numbered. This is undesireable behaviour. this is fixed with the following hack, inspired on a note"
         , "% found at http://tex.stackexchange.com/questions/40492/what-are-the-differences-between-align-equation-and-displaymath"
         , "\\DeclareRobustCommand{\\[}{\\begin{equation}}"
         , "\\DeclareRobustCommand{\\]}{\\end{equation}}"
         , "% <End-adaptation>"
         , ""
         , ""
         , "% ============Ampersand specific End==================="
         ])
    | view fspecFormatL env `elem` [Fpdf,Flatex]
    ]
  where
    outputLang' :: Lang
    outputLang' = outputLang env fSpec
    mkContext :: [(Text,Text)] -> PT.Context Text
    mkContext xs = PT.Context $ Map.fromList . map fun $ xs
      where fun :: (Text,Text) -> (Text,PT.Val Text)
            fun (k,v)= (k, SimpleVal (Text (T.length v) v))
            
--DESCR -> functions to write the pandoc
writepandoc :: (HasDirOutput env, HasFSpecGenOpts env, HasDocumentOpts env, HasLogFunc env) => 
      FSpec -> Pandoc -> RIO env ()
writepandoc fSpec thePandoc = do
  env <- ask
  logInfo $ "Generating "<>display (fSpecFormatString env) <>" to : "<>display (T.pack $ outputFile env)
  liftIO $ createDirectoryIfMissing True (takeDirectory (outputFile env))
  liftIO $ writepandoc' env fSpec thePandoc
 where
    fSpecFormatString :: (HasDocumentOpts env) => env -> Text 
    fSpecFormatString = T.toLower . T.drop 1 . tshow . view fspecFormatL

outputFile :: (HasDocumentOpts env, HasFSpecGenOpts env, HasDirOutput env) => env -> FilePath
outputFile env = view dirOutputL env </> baseName env -<.> ext (view fspecFormatL env) 
       
ext :: FSpecFormat -> FilePath
ext format =
      case format of
        Fasciidoc     -> ".txt"
        Fcontext      -> ".context"
        Fdocbook      -> ".docbook"
        Fdocx         -> ".docx"
        Fhtml         -> ".html"
        Fpdf          -> ".pdf"
        Fman          -> ".man"
        Fmarkdown     -> ".md"
        Fmediawiki    -> ".mediawiki"
        Fopendocument -> ".odt"
        Forg          -> ".org"
        FPandoc       -> ".pandoc"
        Fplain        -> ".plain"
        Frst          -> ".rst"
        Frtf          -> ".rtf"
        Flatex        -> ".ltx"
        Ftexinfo      -> ".texinfo"
        Ftextile      -> ".textile"
                   
writepandoc' :: (HasDocumentOpts env, HasFSpecGenOpts env, HasDirOutput env) => env -> FSpec -> Pandoc -> IO ()
writepandoc' env fSpec thePandoc = liftIO . runIOorExplode $ do
  case writer of 
     ByteStringWriter f -> do 
       res <- f writerOptions thePandoc -- >>= handleError
       let content :: LByteString
           content = res
       liftIO $ createDirectoryIfMissing True (takeDirectory (outputFile env))
       BL.writeFile (outputFile env) content
     TextWriter f -> case view fspecFormatL env of
        Fpdf -> do
           res <- makePDF "pdflatex" [] f writerOptions thePandoc -- >>= handleError)
           case res of
             Right pdf -> writeFnBinary (outputFile env) pdf
             Left err' -> liftIO . throwIO . PandocPDFError .
                               decodeUtf8 . BL.toStrict $ err'
        _     -> liftIO $ do
                output <- runIO (f writerOptions thePandoc) >>= handleError
                writeFileUtf8 (outputFile env) output
 where   
    writer :: PandocMonad m => Writer m
    writer = case lookup writerName writers of
                Nothing -> fatal $ "There is no such Pandoc writer: "<>writerName
                Just w -> w
    writerName =
      case view fspecFormatL env of
       Fpdf    -> "latex"
       Flatex  -> "latex"
       FPandoc -> "native"
       Fhtml   -> "html5"
       fmt     -> T.toLower . T.drop 1 . tshow $ fmt
    writeFnBinary :: MonadIO m => FilePath -> BL.ByteString -> m()
    writeFnBinary f bs = do
        liftIO $ createDirectoryIfMissing True (takeDirectory f)
        BL.writeFile (UTF8.encodePath f) bs
    
    writerOptions :: WriterOptions
    writerOptions = def
                      { writerTableOfContents=True
                      , writerNumberSections=True
                      , writerTemplate= Just template
                      , writerVariables=defaultWriterVariables env fSpec
                      , writerHTMLMathMethod =MathML
                    --  , writerMediaBag=bag
                    --  , writerReferenceDocx=Just docxStyleUserPath
                    --  , writerVerbose=optVerbosity
                      }
      where 
        template :: Template Text
        template = case runIdentity $ compileTemplate "" templateText of
                Right a -> a
                Left b  -> fatal $ "Could not compile the template. "<> T.pack b
          where
            templateText :: Text
            templateText = case decodeUtf8 <$> getStaticFileContent PandocTemplates (T.unpack $ "default."<>writerName) of
                             Nothing -> fatal $ "Cannot get the pandoc template for "<> writerName
                             Just partial  -> partial

-----Linguistic goodies--------------------------------------

count :: Lang -> Int -> Text -> Text
count    lang    n      x
 = case (lang, n) of
      (Dutch  , 0) -> "geen "<>plural Dutch x
      (Dutch  , 1) -> "één "<>x
      (Dutch  , 2) -> "twee "<>plural Dutch x
      (Dutch  , 3) -> "drie "<>plural Dutch x
      (Dutch  , 4) -> "vier "<>plural Dutch x
      (Dutch  , 5) -> "vijf "<>plural Dutch x
      (Dutch  , 6) -> "zes "<>plural Dutch x
      (Dutch  , _) -> tshow n<>" "<>plural Dutch x
      (English, 0) -> "no "<>plural English x
      (English, 1) -> "one "<>x
      (English, 2) -> "two "<>plural English x
      (English, 3) -> "three "<>plural English x
      (English, 4) -> "four "<>plural English x
      (English, 5) -> "five "<>plural English x
      (English, 6) -> "six "<>plural English x
      (English, _) -> tshow n<>" "<>plural English x



chptTitle :: Lang -> Chapter -> Inlines
chptTitle lang cpt =
  case cpt of
    Intro                 -> text.l $ (NL "Inleiding", EN "Introduction")
    SharedLang            -> text.l $ (NL "Gemeenschappelijke taal", EN "Shared Language")
    Diagnosis             -> text.l $ (NL "Diagnose", EN "Diagnosis")
    ConceptualAnalysis    -> text.l $ (NL "Conceptuele Analyse", EN "Conceptual Analysis")
    DataAnalysis          -> text.l $ (NL "Gegevensstructuur", EN "Data structure")
    Glossary              -> text.l $ (NL "Glossarium", EN "Glossary")
 where 
     -- shorthand for easy localizing    
    l :: LocalizedStr -> Text
    l = localize lang
    



--DESCR -> pandoc print functions for Ampersand data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> Inlines

instance ShowMath Rule where
 showMath = showMath . formalExpression

instance ShowMath Expression where
 showMath = math . showExpr . insParentheses
   where  showExpr :: Expression -> Text
          showExpr (EEqu (l,r)) = showExpr l<>inMathEquals<>showExpr r
          showExpr (EInc (l,r)) = showExpr l<>inMathInclusion<>showExpr r
          showExpr (EIsc (l,r)) = showExpr l<>inMathIntersect<>showExpr r
          showExpr (EUni (l,r)) = showExpr l<>inMathUnion<>showExpr r
          showExpr (EDif (l,r)) = showExpr l<>inMathDifference <>showExpr r
          showExpr (ELrs (l,r)) = showExpr l<>inMathLeftResidu<>showExpr r
          showExpr (ERrs (l,r)) = showExpr l<>inMathRightResidu<>showExpr r
          showExpr (EDia (l,r)) = showExpr l<>inMathDiamond<>showExpr r
          showExpr (ECps (EEps i sgn,r)) | i==source sgn||i==target sgn = showExpr  r
                                         | otherwise                    = showExpr (ECps (EDcI i,r))
          showExpr (ECps (l,EEps i sgn)) | i==source sgn||i==target sgn = showExpr  l
                                         | otherwise                    = showExpr (ECps (l,EDcI i))
          showExpr (ECps (l,r)) = showExpr l<>inMathCompose<>showExpr r
          showExpr (ERad (l,r)) = showExpr l<>inMathRelativeAddition<>showExpr r
          showExpr (EPrd (l,r)) = showExpr l<>inMathCartesianProduct<>showExpr r
          showExpr (EKl0 e)     = showExpr (addParensToSuper e)<>inMathStar
          showExpr (EKl1 e)     = showExpr (addParensToSuper e)<>inMathPlus
          showExpr (EFlp e)     = showExpr (addParensToSuper e)<>inMathFlip
          showExpr (ECpl e)     = inMathOverline (showExpr e)
          showExpr (EBrk e)     = "("<>showExpr e<>")"
          showExpr (EDcD d)     = inMathText (name d)
          showExpr (EDcI c)     = "I_{ \\lbrack "<>inMathText (name c)<>" \\rbrack }"
          showExpr  EEps{}      = "" -- fatal "EEps may occur only in combination with composition (semicolon)."  -- SJ 2014-03-11: Are we sure about this? Let's see if it ever occurs...
          showExpr (EDcV sgn)   = "V_{ \\lbrack "<>inMathText (name (source sgn))<>"*"<>inMathText (name (target sgn))<>" \\rbrack }"
          showExpr (EMp1 val _) = atomVal2Math val --"\texttt{"<>show val<>"}"
          
atomVal2Math :: PAtomValue -> Text
atomVal2Math pav =
   case pav of
    PSingleton   _ s _ -> " \\texttt{"<>tshow s<>"}"
    ScriptString   _ s -> " \\texttt{"<>tshow s<>"}"
    XlsxString     _ s -> " \\texttt{"<>tshow s<>"}"
    ScriptInt      _ i -> tshow i
    ScriptFloat    _ d -> tshow d
    XlsxDouble     o d -> fatal ("We got a value "<>tshow d<>" from "<>tshow o<>", which has to be shown in an expression, however the technicaltype is not known.")
    ComnBool       _ b -> tshow b
    ScriptDate     _ x -> tshow x
    ScriptDateTime _ x -> tshow x

-- add extra parentheses to consecutive superscripts, since latex cannot handle these
-- (this is not implemented in insParentheses because it is a latex-specific issue)
addParensToSuper :: Expression -> Expression
addParensToSuper e@EKl0{} = EBrk e
addParensToSuper e@EKl1{} = EBrk e
addParensToSuper e@EFlp{} = EBrk e
addParensToSuper e        = e

instance ShowMath Relation where
 showMath decl = math . noBreaking $ 
        inMathText (name decl)<>" \\lbrack "
     <>(inMathText . name . source $ decl)<> (if isFunction (EDcD decl) then " \\mapsto " else "*")
     <>(inMathText . name . target $ decl)<>" \\rbrack "

noBreaking :: (IsString a, Semigroup a) => a -> a
noBreaking x = "{"<>x<>"}"

---------------------------
--- Math related stuff ---
---------------------------
-- safe function to have plain text in a piece of Math
inMathText :: Text -> Text
inMathText = id

inMathCartesianProduct :: Text
inMathCartesianProduct = "\\times "

inMathCompose :: Text
inMathCompose = ";"

inMathRelativeAddition :: Text
inMathRelativeAddition = "\\dagger "

inMathIntersect :: Text
inMathIntersect = "\\cap "

inMathUnion :: Text
inMathUnion = "\\cup "

inMathInclusion :: Text
inMathInclusion = "\\vdash "

inMathEquals :: Text
inMathEquals = "="

inMathStar :: Text
inMathStar = "^{*}"

inMathPlus :: Text
inMathPlus = "^{+}"

inMathDifference :: Text
inMathDifference = " - "

inMathLeftResidu :: Text
inMathLeftResidu = " / "

inMathRightResidu :: Text
inMathRightResidu = " \\backslash "

inMathDiamond :: Text
inMathDiamond = " \\Diamond "

inMathFlip :: Text
inMathFlip = "^{\\smallsmile}"

inMathOverline :: Text -> Text
inMathOverline x = " \\overline{"<>x<>"} "

texOnlyMarginNote :: Text -> Text
texOnlyMarginNote marginNote = 
   "\\marginpar{\\begin{minipage}[t]{3cm}{\\noindent\\small\\em "<>marginNote<>"}\\end{minipage}}"


commaPandocAnd :: Lang -> [Inlines] -> Inlines
commaPandocAnd Dutch = commaNLPandoc "en"
commaPandocAnd English = commaEngPandoc "and"
commaPandocOr :: Lang -> [Inlines] -> Inlines
commaPandocOr Dutch = commaNLPandoc "of"
commaPandocOr English = commaEngPandoc "or"

commaEngPandoc :: Inlines -> [Inlines] -> Inlines
commaEngPandoc s [a,b,c] = a <> ", " <> b <> ", " <> s <> space <> c
commaEngPandoc s [a,b]   = a <> space <> s <> space <> b
commaEngPandoc _   [a]   = a
commaEngPandoc s (a:as)  = a <> ", " <> commaEngPandoc s as
commaEngPandoc _   []    = mempty

commaNLPandoc :: Inlines -> [Inlines] -> Inlines
commaNLPandoc s [a,b]  = a <> space <> s <> space <> b
commaNLPandoc  _  [a]  = a
commaNLPandoc s (a:as) = a <> ", " <> commaNLPandoc s as
commaNLPandoc  _  []   = mempty
   
outputLang :: (HasOutputLanguage env) => env -> FSpec -> Lang
outputLang env fSpec = fromMaybe (defOutputLang fSpec) $ view languageL env

