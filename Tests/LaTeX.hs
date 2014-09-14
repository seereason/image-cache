{-# LANGUAGE OverloadedStrings #-}
module LaTeX (tests) where

import Data.Monoid ((<>))
import Data.Set (fromList)
import Data.String (fromString)
import Data.Text (Text)
import Text.LaTeX.Base.Class (comm0, commS)
import Text.LaTeX.Base.Commands (raw, vfill, textbf, vspace, large)
import Text.LaTeX.Base.Render (render)
import Text.LaTeX.Base.Syntax (LaTeX(TeXCommS), Measure(..))
import Text.LaTeX.Base.Writer (execLaTeXM, execLaTeXT)
import Text.LaTeX.Packages.Graphicx (IGOption(..), includegraphics)
import Test.HUnit

test1 =
    TestCase $
      assertEqual
        "includegraphics 1"
        "\\includegraphics[width=3.00000in,trim=1.00000pt 1.00000pt 1.00000pt 1.00000pt,clip=true]{/srv/appraisalscribe-development/images/c3bd1388b41fa5d956e4308ce518a8bd.png}"
        (render (execLaTeXM (includegraphics [IGWidth (In 3.0), IGTrim (Pt 1.0) (Pt 1.0) (Pt 1.0) (Pt 1.0), IGClip True]
                                             "/srv/appraisalscribe-development/images/c3bd1388b41fa5d956e4308ce518a8bd.png")))

test2 =
    TestCase $
      assertEqual
        "includegraphics 2"
        "\\includegraphics[width=\\textwidth]{/srv/appraisalscribe-development/images/c3bd1388b41fa5d956e4308ce518a8bd.png}\\vfill{}\\textbf{\\vspace{0.30000in}{\\large{}Intended Use: Sales Advisory}{\\large{}Type of Value:}\\vspace{0.30000in}{\\large{}Client:}\\vspace{0.30000in}{\\large{}Appraised By:}\\vspace{0.30000in}{\\large{}Date of Inspection:}{\\large{}Effective Date of Valuation:}{\\large{}Date of Report: \\reportdate{}}\\vfill{}}"
        (render
         (execLaTeXM
          (includegraphics [IGWidth (CustomMeasure (TeXCommS "textwidth"))]
                           "/srv/appraisalscribe-development/images/c3bd1388b41fa5d956e4308ce518a8bd.png" <>
           vfill <>
           textbf (vspace (In 0.30000) <>
                   large "Intended Use: Sales Advisory" <>
                   large "Type of Value:" <>
                   vspace (In 0.30000) <> large "Client:" <>
                   vspace (In 0.30000) <> large "Appraised By:" <>
                   vspace (In 0.30000) <> large "Date of Inspection:" <>
                   large "Effective Date of Valuation:" <>
                   large ("Date of Report: " <> comm0 "reportdate") <>
                   vfill))))

tests = TestList [test1, test2]
