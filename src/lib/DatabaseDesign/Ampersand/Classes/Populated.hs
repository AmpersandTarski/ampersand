{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Classes.Populated                 (Populated(..),atomsOf)
where
   import DatabaseDesign.Ampersand.ADL1.Pair                       (kleenejoin,mkPair,closPair)
   import DatabaseDesign.Ampersand.ADL1.Expression                 (notCpl)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Basics                     (Collection (..),fatalMsg, Identified(..))   
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Populated.hs"

   
   class Populated a where
    -- | this function returns the pairs as content of a specific a, given a list of populations. 
    --   The list of populations should contain all user-defined populations. 
    fullContents :: [UserDefPop] -> a -> Pairs
   
   -- | This function returns the atoms of a concept (like fullContents does for relation-like things.)
   atomsOf :: [UserDefPop] -> A_Concept -> [String] 
   atomsOf _ ONE  = ["1"] -- fatal 126 "Asking for the value of the universal singleton"
   atomsOf pt c@PlainConcept{}
     = fatal 99924 "please retypecheck!" {-
       nub$[srcPaire p | PRelPopu dcl ps   <- pt, p <- ps, (source dcl) DatabaseDesign.Ampersand.Core.Poset.<= c]
         ++[trgPaire p | PRelPopu dcl ps   <- pt, p <- ps, (target dcl) DatabaseDesign.Ampersand.Core.Poset.<= c]
         ++[a          | PCptPopu cpt atms <- pt, a <- atms, cpt        DatabaseDesign.Ampersand.Core.Poset.<= c]
       -}
   instance Populated Declaration where
    fullContents pt dcl
      = case dcl of
         Isn c -> [ mkPair a a | a <-atomsOf pt c]
         Vs sgn -> [ mkPair sa ta | sa<-atomsOf pt (source sgn), ta<-atomsOf pt (target sgn)]
         Sgn{} -> if decISA dcl
                  then [ mkPair a a | a <-atomsOf pt (source dcl)] 
                  else 
                   case filter (isTheDecl dcl) pt of
                     []    -> [] 
                     [pop] -> popps pop
                     _     -> fatal 31 $ "Multiple entries in lookup table found for declaration."
     where isTheDecl d pop =
             case pop of
               PRelPopu{}  -> d == popdcl pop
               PCptPopu{}  -> False


   instance Populated Expression where
    fullContents pt = contents
     where
      contents expr
       = case expr of
            EEqu (l,r) _ -> contents ((l .|-. r) ./\. (r .|-. l))
            EImp (l,r) sgn -> contents (notCpl sgn l .\/. r)
            EUni (l,r) _ -> contents l `uni` contents r
            EIsc (l,r) _ -> contents l `isc` contents r
            EDif (l,r) _ -> contents l >- contents r
            -- The left residual l/r is defined by: for all x,y:  y(l/r)x  <=>  for all z in X, x l z implies y r z.
            ELrs (l,r) c _ -> [(y,x) | x <- case source l of
                                            sl@PlainConcept{} -> atomsOf pt sl
                                            sl     -> fatal 68 ("source l should be PlainConcept instead of "++show sl++".")
                                   , y <- case source r of
                                            sr@PlainConcept{} -> atomsOf pt sr
                                            sr     -> fatal 71 ("source r should be PlainConcept instead of "++show sr++".")
                              --   Derivation:
                              --   , and      [(x,z) `elem` contents l <- (y,z) `elem` contents r          |z<- atomsOf pt (target l `join` target r)]
                              --   , and      [(x,z) `elem` contents l || (y,z) `notElem` contents r       |z<- atomsOf pt (target l `join` target r)]
                              --   , (not.or) [not ((x,z) `elem` contents l || (y,z) `notElem` contents r) |z<- atomsOf pt (target l `join` target r)]
                              --   , (not.or) [     (x,z) `notElem` contents l && (y,z) `elem` contents r  |z<- atomsOf pt (target l `join` target r)]
                              --   , (not.null) [ () |z<- atomsOf pt (target l `join` target r), (x,z) `notElem` contents l, (y,z) `elem` contents r]
                                   , (not.null) [ () |z<- atomsOf pt c, (x,z) `notElem` contents l, (y,z) `elem` contents r]
                                   ]   -- equals contents (ERrs (flp r, flp l))
            -- The right residual l\r defined by: for all x,y:   x(l\r)y  <=>  for all z in X, z l x implies z r y.
            ERrs (l,r) c _ -> [(x,y) | x <- case target l of
                                            tl@PlainConcept{} -> atomsOf pt tl
                                            tl     -> fatal 83 ("target l should be PlainConcept instead of "++show tl++".")
                                   , y <- case target r of
                                            tr@PlainConcept{} -> atomsOf pt tr
                                            tr     -> fatal 86 ("target r should be PlainConcept instead of "++show tr++".")
                              --   Derivation:
                              --     and      [(z,x) `elem` contents l    -> (z,y) `elem` contents r       |z<- atomsOf pt (source l `join` source r)]
                              --     and      [(z,x) `notElem` contents l || (z,y) `elem` contents r       |z<- atomsOf pt (source l `join` source r)]
                              --     (not.or) [not ((z,x) `notElem` contents l || (z,y) `elem` contents r) |z<- atomsOf pt (source l `join` source r)]
                              --     (not.or) [     (z,x) `elem` contents l && (z,y) `notElem` contents r  |z<- atomsOf pt (source l `join` source r)]
                              --     (not.null) [ () |z<- atomsOf pt (source l `join` source r), (z,x) `elem` contents l, (z,y) `notElem` contents r]
                                   , (not.null) [ () |z<- atomsOf pt c, (z,x) `elem` contents l, (z,y) `notElem` contents r]
                                   ]   -- equals contents (ELrs (flp r, flp l))
            ERad (l,r) c _ -> [(x,y) | x <- case target l of
                                            tl@PlainConcept{} -> atomsOf pt tl
                                            tl     -> fatal 97 ("target l should be PlainConcept instead of "++show tl++".")
                                   , y <- case source r of
                                            sr@PlainConcept{} -> atomsOf pt sr
                                            sr     -> fatal 100 ("source r should be PlainConcept instead of "++show sr++".")
                                   , and [(x,z) `elem` contents l || (z,y) `elem` contents r |z<- atomsOf pt c]
                                   ]
            EPrd (l,r) _ -> [ (a,b) | a <- atomsOf pt (source l), b <- atomsOf pt (target r) ]
            ECps (l,r) _ _ -> contents l `kleenejoin` contents r
            EKl0 e     _ -> if source e == target e --see #166
                            then closPair (contents e `uni` contents (iExpr (source e)))
                            else fatal 69 ("source and target of "++show e++show (sign e)++ " are not equal.")
            EKl1 e     _ -> closPair (contents e)
            EFlp e     _ -> [(b,a) | (a,b)<-contents e]
            ECpl e     _ -> [apair | apair <-[ mkPair x y | x<-atomsOf pt (source e), y<-atomsOf pt (target e)]
                                   , apair `notElem` contents e  ]
            EBrk e       -> contents e
            ETyp e   sgn -> if sign e==sgn then contents e else [(a,b) | (a,b) <-contents e
                                                                       , a `elem` atomsOf pt (source sgn)
                                                                       , b `elem` atomsOf pt (target sgn)]
            EDcD dcl   _ -> fullContents pt dcl
            EDcI     sgn -> [mkPair a a | a <- atomsOf pt (source sgn)]
            EDcV     sgn -> [mkPair s t | s <- atomsOf pt (source sgn)
                                            , t <- atomsOf pt (target sgn) ]
            EMp1 atom       sgn -> if name (source sgn)=="SESSION" then [] else [mkPair atom atom] -- prevent populating SESSION

{- Derivation of contents (ERrs (l,r)):
Let cL = contents l
    cR = contents r
  contents (ERrs (l,r))
= [(x,y) | x<-contents (target l), y<-contents (target r)
         ,      and [    (z,x) `notElem` cL || (z,y) `elem` cR  | z<-contents (source l)] ]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , not ( or [not((z,x) `notElem` cL || (z,y) `elem` cR) | z<-contents (source l)])]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , not ( or [    (z,x)  `elem` cL && (z,y) `notElem` cR | z<-contents (source l)])]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , null [ () | z<-contents (source l), (z,x)  `elem` cL && (z,y) `notElem` cR]]
= [(x,y) | x<-contents (target l), y<-contents (target r)
         , null [ () | (z,x') <- cL, x==x', (z,y) `notElem` cR ]]
-}
