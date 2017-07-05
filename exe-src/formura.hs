{-# LANGUAGE ImplicitParams #-}
module Main where


import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml.Pretty as Y
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr
import qualified Text.Trifecta as P

import qualified Formura.Annotation as A
import           Formura.Annotation.Boundary
import           Formura.Annotation.Representation
import           Formura.CommandLineOption
import           Formura.NumericalConfig
import           Formura.OrthotopeMachine.Graph
import           Formura.OrthotopeMachine.Translate (genOMProgram)
import           Formura.OrthotopeMachine.Manifestation (genMMProgram)
import qualified Formura.Parser as P
import           Formura.Desugar
import           Formura.Compiler
import           Formura.Syntax
import           Formura.MPICxx.Language (TargetLanguage(..), targetLanguage)
import qualified Formura.MPICxx.Translate as C
import qualified Formura.MPIFortran.Translate as F

-- Formuraの処理の始まり
main :: IO ()
main = do
  -- コマンドラインオプションの処理(Formura.CommandLineOption)
  opts <- getCommandLineOption
  -- ImplicitParams として使用する．(実際の引数はWithCommandLineOptionで定義)
  let ?commandLineOption = opts

  -- ファイル名リストで与えられたファイルに対してそれぞれprocessを実行
  mapM_ process (opts ^. inputFilenames)

-- 入力ファイル名からFormura処理および，ファイル出力まで行う．
process :: WithCommandLineOption => FilePath -> IO ()
process fn = do
  -- Text.Trifecta.Parserの機能を利用してFormuraソースコードをparseする．
  -- P.parseFromFileExはText.Trifecta.Parserで定義されている．
  -- Parser定義(P.runP $ P.program <* P.eof)とファイル名fnからファイルをparseする．
  -- runPはParser Program型．
  -- runPはP(Formura.Parser)型のレコードの要素
  -- programはP Program型（パーサー本体），Formura.Parserで定義
  -- P型コンストラクタはParsing型クラスのインスタンスとして導出されている．
  -- eofはText.Parser.Combinatorsで定義
  -- mprogはResult Program型．
  -- Program型はFormura.Syntaxで定義
  -- Result a型はコンストラクタとして，Failure a, Success aを持つ．
  -- <* 演算子はApplicative型クラスが持つ演算．左右の処理を実行して，左側の結果だけpureに入れて返す．
  mprog <- P.parseFromFileEx (P.runP $ P.program <* P.eof) fn
  case mprog of
      -- パースに失敗した場合，失敗した理由を標準エラー出力へ
      P.Failure doc -> Ppr.displayIO stdout $ Ppr.renderPretty 0.8 80 $ doc <> Ppr.linebreak
      -- 成功：次のステップcodegenへ進む．
      P.Success prog -> codegen prog

codegen :: WithCommandLineOption => Program -> IO ()
codegen sugarcoated_prog = do
  prog <- desugar sugarcoated_prog

  omProg <- genOMProgram prog

  when (?commandLineOption ^. verbose) $ do
    putStrLn "## Debug print: global environment of the simulation"
    print (omProg ^. omGlobalEnvironment)
    putStrLn ""

    putStrLn "## Debug print: simulation state"
    print (omProg ^. omStateSignature)
    putStrLn ""

    putStrLn "## Debug print: init graph"
    mapM_ pprNode $ M.toList (omProg ^. omInitGraph)
    putStrLn ""

    putStrLn "## Debug print: step graph"
    mapM_ pprNode $ M.toList (omProg ^. omStepGraph)
    putStrLn ""

  mmProg <- genMMProgram omProg
  when (?commandLineOption ^. verbose) $ do
    putStrLn "## Debug print: manifested init graph"
    mapM_ pprMMNode $ M.toList (mmProg ^. omInitGraph)
    putStrLn ""

    putStrLn "## Debug print: manifested step graph"
    mapM_ pprMMNode $ M.toList (mmProg ^. omStepGraph)
    putStrLn ""

  putStrLn $ "Target language is:" ++ show targetLanguage
  case targetLanguage of
    MPICxx -> C.genCxxFiles prog mmProg
    MPIFortran -> F.genFortranFiles prog mmProg

pprNode :: (OMNodeID, OMNode) -> IO ()
pprNode (i,n) = do
  let r = case A.toMaybe (n ^. A.annotation) of
        Just Manifest -> "M"
        _             -> " "
      varName = case A.toMaybe (n ^. A.annotation) of
        Just (SourceName n1) -> n1
        _                   -> ""
  putStrLn $ unwords [r , take 8 $ varName ++ repeat ' ', show (i,n)]

pprMMNode :: (OMNodeID, MMNode) -> IO ()
pprMMNode (i,n) = do
  let
      varName = case A.toMaybe (n ^. A.annotation) of
        Just (SourceName n1) -> n1
        _                   -> ""
      Just (Boundary bdy) = A.toMaybe $ n^.A.annotation
  putStrLn $ unwords [take 8 $ varName ++ repeat ' ', show (i,n),show bdy]
