#!/usr/bin/env runhaskell

import Development.Shake
import Development.Shake.FilePath

import qualified Data.ByteString.Lazy as BL

import Text.Pandoc
import Text.Pandoc.PDF

import Data.List ( isPrefixOf )

main = shakeArgs shakeOptions $ do
	phony "prectures" $ do
		dirs <- getDirectoryDirs "."
		need $ fmap (<.> "pdf") . filter (not . isPrefixOf ".") $ dirs
	"*.pdf" *> \out -> do
		let mdin = dropExtension out </> "slides.md"
		need [mdin]
		putNormal $ "pandoc " ++ out
		contents <- readFileLines mdin
		template <- readFileLines $ "template.tex"
		liftIO $ do
			slides <- makeSlides (unlines template) (unlines contents)
			case slides of
				Right s -> BL.writeFile out s
				Left s -> print s
	phony "clean" $ do
		putNormal $ "removing pdf files"
		liftIO $ removeFiles "." ["//*.pdf"]

options template = def
		{
			writerBeamer = True,
			writerStandalone = True,
			writerListings = True,
			writerSlideLevel = Just 1,
			writerTemplate = template,
			writerVariables = [("theme", "intridea"), ("colortheme", "solarized"), ("fontsize", "14pt")]
		}


makeSlides template = makePDF "pdflatex" writeLaTeX (options template) . readMarkdown def
