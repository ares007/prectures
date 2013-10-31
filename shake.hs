#!/usr/bin/env runhaskell

import Development.Shake
import Development.Shake.FilePath

import Text.Pandoc
import Text.Pandoc.PDF

import Data.List ( isPrefixOf )
import qualified Data.ByteString.Lazy as BL

main = shakeArgs shakeOptions $ do
	want ["prectures"]
	phony "prectures" $ do
		dirs <- fmap (filter $ not . isPrefixOf ".") $ getDirectoryDirs "."
		need $ fmap (<.> "pdf") dirs
	"*.pdf" *> subdirToBeamer
	phony "clean" $ do
		putNormal $ "removing pdf files"
		liftIO $ removeFiles "." ["//*.pdf"]

subdirToBeamer pdf = do
	let dir = dropExtension pdf
	let mdin = dir </> "slides.md"
	need [mdin]
	putNormal $ "pandoc " ++ pdf
	contents <- readFileLines mdin
	template <- readFileLines $ "template.tex"
	etheme <- doesFileExist $ dir </> "theme"
	ectheme <- doesFileExist $ dir </> "colortheme"
	let theme = if etheme then "logic" else "intridea"
	let ctheme = if ectheme then "slush" else "solarized"
	liftIO $ do
		slides <- mdToBeamer (theme, ctheme) dir (unlines template) (unlines contents)
		case slides of
			Right s -> BL.writeFile pdf s
			Left s -> print s

mdToBeamer (theme, ctheme) path template = makePDF "pdflatex" writeLaTeX (options (theme, ctheme) path template) . readMarkdown def

options (theme, ctheme) path template = def
	{
		writerBeamer = True,
		writerStandalone = True,
		writerListings = True,
		writerSlideLevel = Just 1,
		writerTemplate = template,
		writerVariables = [("theme", theme), ("colortheme", ctheme), ("fontsize", "14pt"), ("graphicspath", path)]
	}
