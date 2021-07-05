{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Output.FSpec2Pandoc (fSpec2Pandoc)
where
import           Ampersand.Output.ToPandoc
import qualified RIO.List as L
import qualified RIO.Text as T
import           RIO.Time
import           Text.Pandoc.CrossRef
--DESCR ->
--The functional design document starts with an introduction
--The second chapter defines the functionality of the system for stakeholders.
--Because we assume these stakeholders to speak the language of the primary process without any technical knowledge,
--the second chapter contains natural language only.
--The third chapter is intended for the analyst. It contains all the rules mentioned in
--natural language in the second chapter. It presents the trace from natural language
--to the formal rule.
--The fourth chapter presents a datamodel together with all the multiplicity rules.
-- by datasets and rules.
--Datasets are specified through PLUGS in Ampersand. The dataset is build around one concept,
--also called the theme. Functionalities defined on the theme by one or more plugs are
--described together with the rules that apply to the dataset. Rules not described by
--the dataset are described in the last section of chapter 2.
--The following chapters each present one INTERFACE
--The specification end with a glossary.

--TODO: Invent a syntax for meta information that is included in the source file...

--The following general requirements apply to the functional design document:
--Descriptive title, number, identifier, etc. of the specification
--Date of last effective revision and revision designation
--A logo (trademark recommended) to declare the document copyright, ownership and origin
--Table of Contents
--Person, office, or agency responsible for questions on the specification, updates, and deviations.
--The significance, scope or importance of the specification and its intended use.
--Terminology, definitions and abbreviations to clarify the meanings of the specification
--Test methods for measuring all specified characteristics
--Material requirements: physical, mechanical, electrical, chemical, etc. Targets and tolerances.
--Performance testing requirements. Targets and tolerances.
--Drawings, photographs, or technical illustrations
--Workmanship
--Certifications required.
--Safety considerations and requirements
--Environmental considerations and requirements
--Quality control requirements, Sampling (statistics), inspections, acceptance criteria
--Person, office, or agency responsible for enforcement of the specification.
--Completion and delivery.
--Provisions for rejection, reinspection, rehearing, corrective measures
--References and citations for which any instructions in the content maybe required to fulfill the traceability and clarity of the document
--Signatures of approval, if necessary
--Change record to summarize the chronological development, revision and completion if the document is to be circulated internally
--Annexes and Appendices that are expand details, add clarification, or offer options.

fSpec2Pandoc :: (HasDirOutput env, HasDocumentOpts env) 
   => env -> UTCTime -> FSpec -> (Pandoc, [Picture])
fSpec2Pandoc env now fSpec = (thePandoc,thePictures)
  where
    -- shorthand for easy localizing    
    l :: LocalizedStr -> Text
    l = localize outputLang'
    outputLang' = outputLang env fSpec
    wrap :: Pandoc -> Pandoc
    wrap (Pandoc meta blocks) = 
      Pandoc meta $ runCrossRef m' Nothing crossRefBlocks blocks 
      where 
        m' =   figureTitle ( (str.l) (NL "Figuur" ,EN "Figure"))
            <> tableTitle  ( (str.l) (NL "Tabel"  ,EN "Table" ))
            <> figPrefix [(str.l) (NL "fig.",EN "fig.")
                         ,(str.l) (NL "figs.",EN "figs.")]
            <> eqnPrefix [(str.l) (NL "relatie",EN "relation")
                         ,(str.l) (NL "relaties", EN "relations")]
            <> tblPrefix [(str.l) (NL "tbl.",EN "tbl.")
                         ,(str.l) (NL "tbls.",EN "tbls.")]
            <> lstPrefix [(str.l) (NL "afspraak",EN "agreement")
                         ,(str.l) (NL "afspraken", EN "agreements")]
            <> secPrefix [(str.l) (NL "hoofdstuk",EN "chapter")
                         ,(str.l) (NL "hoofdstukken", EN "chapters")]
            <> cref True     --required for pandoc-crossref to do its work properly
            <> chapters True -- Numbering with subnumbers per chapter

    thePandoc = wrap .
        setTitle
           (case metaValues "title" fSpec of
                [] -> (if view chaptersL env == [Diagnosis]
                       then (text.l)
                               ( NL "Diagnose van "
                               , EN "Diagnosis of ")
                       else if view chaptersL env == [ConceptualAnalysis]
                       then (text.l)
                               ( NL "Conceptuele Analyse van "
                               , EN "Conceptual Analysis of ")
                       else if view chaptersL env == [Glossary]
                       then (text.l)
                               ( NL "Woordenlijst van "
                               , EN "Glossary of ")
                       else  (text.l)
                               ( NL "Functioneel Ontwerp van "
                               , EN "Functional Design of ")
                      ) <> (singleQuoted.text.name) fSpec
                titles -> (text . T.concat . L.nub) titles --reduce doubles, for when multiple script files are included, this could cause titles to be mentioned several times.
           )
      . setAuthors ( 
           case metaValues "authors" fSpec of
             [] -> [(text.l) 
                    ( NL "Specificeer auteurs in Ampersand met: META \"authors\" \"<auteursnamen>\""
                    , EN "Specify authors in Ampersand with: META \"authors\" \"<author names>\"")
                   ] 
             xs -> text <$> L.nub xs  --reduce doubles, for when multiple script files are included, this could cause authors to be mentioned several times.

        )
      . setDate (text (T.pack $ formatTime (lclForLang outputLang') "%-d %B %Y" now))
      . doc . mconcat $ blocksByChapter
    
    thePictures = concat picturesByChapter
    blocksByChapter :: [Blocks]
    picturesByChapter :: [[Picture]]
    (blocksByChapter, picturesByChapter) = L.unzip . map fspec2Blocks . chaptersInDoc $ env

    fspec2Blocks :: Chapter -> (Blocks, [Picture])
    fspec2Blocks Intro                 = (chpIntroduction       env now fSpec, [])
    fspec2Blocks SharedLang            = (chpNatLangReqs        env  0 fSpec, [])
    fspec2Blocks Diagnosis             = chpDiagnosis           env    fSpec
    fspec2Blocks ConceptualAnalysis    = chpConceptualAnalysis  env  0 fSpec
    fspec2Blocks DataAnalysis          = chpDataAnalysis        env    fSpec
    fspec2Blocks Glossary              = chpGlossary            env    fSpec

