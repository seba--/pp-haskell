{-# LANGUAGE TemplateHaskell, CPP #-}
module Main where

import Language.Haskell.Exts
import System.Environment (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import System.Exit (exitFailure)
import System.Console.CmdArgs.Explicit
import Paths_pp_haskell (version)
import qualified Data.Version as V (showVersion)

import On

data Config = Config 
  {  input        ::  Maybe FilePath
  ,  output       ::  Maybe FilePath
  ,  parseMode    ::  ParseMode
  ,  printMode    ::  PPHsMode
  ,  printStyle   ::  Style
  ,  showHelp     ::  Bool
  ,  showVersion  ::  Bool
  }

config :: Config
config = Config 
  {  input        =  Nothing
  ,  output       =  Nothing
  ,  parseMode    =  defaultParseMode {
       fixities   =  Nothing
  }
  ,  printMode    =  defaultMode
  ,  printStyle   =  style
  ,  showHelp     =  False
  ,  showVersion  =  False
  }

flagJust names update typ help = flagReq names (\arg -> Right . update (const (Just arg))) typ help 
flagSet names update typ help = flagReq names (\arg -> Right . update (const arg)) typ help
flagCons names update typ help = flagReq names (\arg -> Right . update (arg :)) typ help
flagExts names update typ help = flagReq names (\arg -> Right . update (classifyExtension arg :)) typ help
flagConst names update value help = flagNone names (update (const value)) help
flagTrue names update help = flagConst names update True help
flagRead names update project typ help = flagReq names parse typ $ help ++ " Default: " ++ show def where  
  parse text config = case reads text of
    [(value, "")] -> Right $ update (const value) config
    _ -> Left $ "Expected " ++ typ ++ " but found '" ++ text ++ "'."
  
  def = project config

flagIndent names update project help = flagRead names ($(on 'printMode) . update) (project . printMode) "INDENT" help

arguments = (modeEmpty config)
  {  modeNames = ["pp-haskell"]
  ,  modeHelp = "Parses and pretty-prints haskell files."
  ,  modeGroupFlags = (toGroup [])
     {  groupUnnamed = 
        [  flagTrue ["help", "?", "h"] $(on 'showHelp) "Show this help."
        ,  flagTrue ["version", "v"] $(on 'showVersion) "Show version information."
        ,  flagJust ["input", "i"] $(on 'input) "FILE" "Read from FILE. Default: Read from stdin."
        ,  flagJust ["output", "o"] $(on 'output) "FILE" "Write to FILE. Default: Write to stdout."
        ,  flagSet  ["filename"] ($(on 'parseMode) . $(on 'parseFilename)) "NAME" "Original NAME of the file being parsed."
        ]
     ,  groupHidden = []
     ,  groupNamed = 
        [  ("\nParser configuration",
           [  flagExts ["extension", "X"] ($(on 'parseMode) . $(on 'extensions)) "EXTENSION" "Enable EXTENSION during parsing."
           ,  flagBool ["ignore-language-pragmas"] ($(on 'parseMode) . $(on 'ignoreLanguagePragmas) . const) $ "If set, the parser won't care about further extensions in LANGUAGE pragmas in source files. Default: " ++ show (ignoreLanguagePragmas (parseMode config)) ++ "."
           ,  flagBool ["ignore-line-pragmas"] ($(on 'parseMode) . $(on 'ignoreLinePragmas) . const) $ "If set, the parser won't read line position information from LINE pragmas in source files. Default: " ++ show (ignoreLinePragmas (parseMode config)) ++ "."
           -- TODO: support fixities?
           ])
        ,  ("\nPrinter configuration",
           [  flagBool ["spacing"] ($(on 'printMode) . $(on 'spacing) . const) $ "Blank lines between statements? Default: " ++ show (spacing (printMode config)) ++ "."
           ,  flagBool ["line-pragmas"] ($(on 'printMode) . $(on 'linePragmas) . const) $ "Add GHC-style LINE pragmas to output? Default: " ++ show (linePragmas (printMode config)) ++ "."
           ,  flagRead ["line-length"] ($(on 'printStyle) . $(on 'lineLength)) (lineLength . printStyle) "INTEGER" $ "Length of a line, in characters."
           ,  flagRead ["ribbons-per-line"] ($(on 'printStyle) . $(on 'ribbonsPerLine)) (ribbonsPerLine . printStyle) "FLOAT" $ "Ratio of ribbon length to line length."
           ])
        ,  ("\nLayout",
           [  flagConst ["implicit-layout"] ($(on 'printMode) . $(on 'layout)) PPOffsideRule $ "Pretty-print using classical layout by the offside-rule" ++ if layout (printMode config) == PPOffsideRule then " (Default)." else "."  
           ,  flagConst ["explicit-layout"] ($(on 'printMode) . $(on 'layout)) PPSemiColon $ "Pretty-print using classical layout made explicit by braces and semicolons" ++ if layout (printMode config) == PPSemiColon then " (Default)." else "."
           ,  flagConst ["one-line-per-decl"] ($(on 'printMode) . $(on 'layout)) PPInLine $ "Pretty-print each declaration in a single line, with newlines between them" ++ if layout (printMode config) == PPInLine then " (Default)." else "."
           ,  flagConst ["no-layout"] ($(on 'printMode) . $(on 'layout)) PPNoLayout $ "Pretty-print everything in a single line" ++ if layout (printMode config) == PPNoLayout then " (Default)." else "."
           ])
        ,  ("\nIndentation",
           [  flagIndent ["class-indent"] $(on 'classIndent) classIndent $ "Indentation of a class or instance."
           ,  flagIndent ["do-indent"] $(on 'doIndent) doIndent $ "Indentation of a do-expression."
           ,  flagIndent ["case-indent"] $(on 'caseIndent) caseIndent $ "Indentation of the body of a case expression."
           ,  flagIndent ["let-indent"] $(on 'letIndent) letIndent $ "Indentation of the declarations in a let expression."
           ,  flagIndent ["where-indent"] $(on 'whereIndent) whereIndent $ "Indentation of the declarations in a where clause."
           ,  flagIndent ["onside-indent"] $(on 'onsideIndent) onsideIndent $ "Indentation added for continuation lines that would otherwise be offside."
           ])
        ]
     }
  }

getFilename config 
  =  if null (parseFilename (parseMode config))
     then  case input config of
             Just filename -> filename
             Nothing -> "<stdin>"
     else  parseFilename (parseMode config)

readFileOrGetContents maybeFilename
  =  case maybeFilename of
       Nothing -> getContents
       Just filename -> readFile filename


writeFileOrPutStr maybeFilename
  =  case maybeFilename of
       Nothing -> putStr
       Just filename -> writeFile filename

main' config = do
  text <- readFileOrGetContents (input config)
  config <- return $ ($(on 'parseMode) . $(on 'parseFilename)) (const (getFilename config)) config
  let result = parseFileContentsWithMode (parseMode config) text
  case result of
    ParseFailed loc message -> do
      hPutStrLn stderr $ 
        concat  [  srcFilename loc
                ,  ":"
                ,  show (srcLine loc)
                ,  ":"
                ,  show (srcColumn loc)
                ,  ": "
                ,  message]
      exitFailure
    ParseOk ast -> do
      writeFileOrPutStr (output config) $ prettyPrintStyleMode (printStyle config) (printMode config) ast ++ "\n"
  
putHelp = do
  print $ helpText [] HelpFormatAll arguments
  putVersion

putVersion = do
  putStrLn $ "This is pp-haskell-" ++ V.showVersion version ++ " based on haskell-src-exts-" ++ VERSION_haskell_src_exts ++ "."

main = processArgs arguments >>= select

select config
  |  showHelp config  =  putHelp
  |  showVersion config = putVersion
  |  otherwise = main' config  
  
