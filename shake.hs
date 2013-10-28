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
		dirs <- getDirectoryDirs "."
		need $ fmap (<.> "pdf") . filter (not . isPrefixOf ".") $ dirs
	"*.pdf" *> subdirToBeamer
	phony "clean" $ do
		putNormal $ "removing pdf files"
		liftIO $ removeFiles "." ["//*.pdf"]

subdirToBeamer dir = do
	let mdin = dropExtension dir </> "slides.md"
	need [mdin]
	putNormal $ "pandoc " ++ dir
	contents <- readFileLines mdin
	template <- readFileLines $ "template.tex"
	liftIO $ do
		slides <- mdToBeamer (unlines template) (unlines contents)
		case slides of
			Right s -> BL.writeFile dir s
			Left s -> print s

mdToBeamer template = makePDF "pdflatex" writeLaTeX (options template) . readMarkdown def

options template = def
	{
		writerBeamer = True,
		writerStandalone = True,
		writerListings = True,
		writerSlideLevel = Just 1,
		writerTemplate = template,
		writerVariables = [("theme", "intridea"), ("colortheme", "solarized"), ("fontsize", "14pt")]
	}
