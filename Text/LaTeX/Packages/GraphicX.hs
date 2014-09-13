{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, StandaloneDeriving #-}
-- | Use HaTeX to generate the stuff I have generating with raw.  Temporary.  (Hah.)
module Text.LaTeX.Packages.GraphicX
    ( IncludeGraphicsAttribute(..)
    , includegraphics
    , measure
    , nocomment
    , nbsp
    ) where

import Data.Set (Set, toList)
import Data.Text as Text (pack, intercalate)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands hiding (figure)
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Types
--import Text.LaTeX.Base.Texy
import Text.LaTeX.Base.Writer (LaTeXT)

-- | Non-breakable space character.
nbsp :: LaTeXC l => l
nbsp = raw "~"

-- | Renders as "%\n" - an idiom frequently used to start a new line
-- without the effect of a "real" newline.
nocomment :: LaTeXC l => l
nocomment = comment mempty

-- | I got this from a list that said "here are the important ones."
data IncludeGraphicsAttribute
    = Width LaTeX -- ^ Specify the preferred width of the imported image to xx.	NB. Only specifying either width or height will scale the image whilst maintaining the aspect ratio.
    | Height LaTeX -- ^ Specify the preferred height of the imported image to xx.
    | KeepAspectRatio -- ^ This can be set to either true or false. When true, it will scale the image according to both height and width, but will not distort the image, so that neither width nor height are exceeded.
    | Scale Double -- ^ Scales the image by the desired scale factor. e.g, 0.5 to reduce by half, or 2 to double.
    | Angle Double -- ^ This option can rotate the image by xx degrees (counter-clockwise)
    | Trim Measure Measure Measure Measure -- ^ This option will crop the imported image by l from the left, b from the bottom, r from the right, and t from the top. Where l, b, r and t are lengths.
    | Clip -- ^ For the trim option to work, you must set clip=true.
    | Page Int -- ^ If the image file is a pdf file with multiple pages, this parameter allows you to use a different page than the first.
    | Resolution Int -- ^ Specify image resolution in dpi
    deriving (Show, Eq, Ord)

deriving instance Ord Measure
deriving instance Ord LaTeX
deriving instance Ord MathType
deriving instance Ord TeXArg

instance Render IncludeGraphicsAttribute where
    render (Width l) = "width=" <> render l
    render (Height l) = "height=" <> render l
    render KeepAspectRatio = "keepaspectratio"
    render (Scale x) = "scale=" <> render x
    render (Angle x) = "angle=" <> render x
    render (Trim l b r t) = "trim=" <> render l <> " " <> render b <> " " <> render r <> " " <> render t
    render Clip = "clip"
    render (Page n) = "page=" <> render n
    render (Resolution n) = "resolution=" <> render n

instance Render (Set IncludeGraphicsAttribute) where
    render s = Text.intercalate ", " (map render (toList s))


includegraphics :: LaTeXC l => Set IncludeGraphicsAttribute -> FilePath -> l
includegraphics s path =
    fromLaTeX $ TeXComm "includegraphics" [OptArg (TeXRaw (render s)), FixArg (TeXRaw (pack path))]

-- Old
_includegraphics :: Monad m => Maybe (LaTeXT m ()) -> LaTeXT m () -> LaTeXT m ()
_includegraphics size name =
    maybe (liftL (\ l -> TeXComm "includegraphics" [FixArg l]) name)
          (\ s -> liftL2 (\ l1 l2 -> TeXComm "includegraphics" [OptArg l1, FixArg l2]) s name)
          size


-- | To use a measure where latex is required, e.g. in the width attribute of includegraphics
measure :: LaTeXC l => Measure -> l
measure = fromLaTeX . TeXRaw . render
