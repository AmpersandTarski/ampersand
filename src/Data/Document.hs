{-# OPTIONS_GHC -Wall #-}
module Data.Document where
   import Strings (chain)
   import Text.Html
   import Options
   import Languages
   -----------------some new data types for simple Document structures--------

   data DTxt = Text String
             | RefSection String
--   data DCell = Cell [DTxt]
--   data DRow  = Row [DCell]
--   data DTable = Table [DRow]

   data DLItem = Item { liDTxt :: DTxt}
   data DSContent = List { items:: [DLItem]}
                  | Par { dtxts :: [DTxt] }
                  | Section {dsLevel :: Int           -- level of header 
                            ,dshead  :: DTxt          -- header of the section
                            ,dscnts  :: [DSContent]   -- stuff in the section
                            }
                  
   data Document = Doc { dflgs :: Options
                       , dtitle :: DTxt
                       , dcont :: [DSContent]
                       }

   class Renderable a where
     render2LaTeX :: a -> String
     render2Html :: a -> Html
--   render2Rtf  :: a -> RTF  -- Komt ooit nog wel eens...

   instance Renderable Document where
      render2LaTeX x = lIntro (dflgs x) 
                    ++ concat
                         [ "\n\\begin{document}"
                         , case language (dflgs x) of
                               English -> "\n" 
                               Dutch   -> "\n\\selectlanguage{dutch}\n"
                         , "\\title{" ++ render2LaTeX (dtitle x) ++ "}"
                         , "\\maketitle"
                         , "\\tableofcontents"
         --                , chain "\n" [lshow language c| c<-(rd' name . preCl . Cl context . ctxwrld) context]
                         , "\n\\end{document}"
                         ]
   
          
   instance Renderable DSContent where
      render2LaTeX x =
           case x of          
             List{} -> "\n\\begin{itemize}" 
                        ++ foldr (++) [] (map render2LaTeX (items x))
                        ++ "\n\\end{itemize}"
             Par {} -> chain "\n\n"  (map (render2LaTeX) (dtxts x))
             Section{} -> "\n\\" ++ subs (dsLevel x) ++ "section{"++ headtxt ++ "}"
                          ++ foldr (++) [] (map render2LaTeX (dscnts x))
                             where subs n 
                                     | n == 1 = []
                                     | n > 1  = "sub" ++ subs (n-1)
                                     | otherwise = undefined
                                   headtxt = render2LaTeX (dshead x)
      render2Html x =
           case x of
             List{} -> dlist (foldr (+++) noHtml (map render2Html (items x)))
             Par {} -> paragraph (foldr (+++) noHtml (map render2Html (dtxts x)))       
             Section{} -> header (render2Html (dshead x))
                      +++ foldr (+++) noHtml (map render2Html (dscnts x))

   instance Renderable DLItem where
       render2LaTeX item = "\n\\item " ++ render2LaTeX (liDTxt item)
       render2Html item = dterm (render2Html (liDTxt item))
        
   instance Renderable DTxt where
     render2LaTeX dtxt = case dtxt of 
                           Text str -> str
                           RefSection ref -> "\n\\ref{chp:"++ref++"}"
     render2Html dtxt  = case dtxt of 
                           Text str -> stringToHtml str
                       


--------------------- Spullen hieronder moeten nog eens op de schop. (HJO, 17 feb 2009)

   lIntro :: Options -> String
   lIntro flags 
     = chain "\n"
         [ "\\documentclass[10pt,a4paper]{report}"
         , case language flags of
              Dutch -> "\\usepackage[dutch]{babel}" 
              English -> ""
         , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
         , "\\parindent 0em                      % Width of paragraph indentation."
         , "\\usepackage{theorem}"
         , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{"
                   ++ (case language flags of
                          Dutch -> "Definitie" 
                          English -> "Definition"
                       )++"}[section]"
         , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{"++
                  case language flags of
                      Dutch -> "Ontwerpregel" 
                      English -> "Design Rule"
               ++"}"
         , "\\usepackage{graphicx}"
         , "\\usepackage{amssymb}"
         , "\\usepackage{amsmath}"
 --        , "\\usepackage{zed-csp}"
         , "\\usepackage{longtable}"
         , "\\def\\id#1{\\mbox{\\em #1\\/}}"
         , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"
         , "\\newcommand{\\iden}{\\mathbb{I}}"
         , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
         , "\\newcommand{\\full}{\\mathbb{V}}"
         , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
         , "\\newcommand{\\relAdd}{\\dagger}"
         , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
         , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
         , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
         , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
         , "\\newcommand{\\rel}{\\times}"
         , "\\newcommand{\\compose}{;}"
         , "\\newcommand{\\subs}{\\vdash}"
         , "\\newcommand{\\fun}{\\rightarrow}"
         , "\\newcommand{\\isa}{\\sqsubseteq}"
         , "\\newcommand{\\N}{\\mbox{\\msb N}}"
         , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
         , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\mbox{$\\rightarrow$}\\id{#3}}"
         , "\\newcommand{\\signat}[3]{\\mbox{${#1}_{[{#2},{#3}]}$}}"
         , "\\newcommand{\\declare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\times$}\\id{#3}}"
         , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\fun$}\\id{#3}}"]

 