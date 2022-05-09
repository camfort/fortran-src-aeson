module Main where

import Language.Fortran.Extra.JSON()
import Control.Monad.IO.Class
import Options.Applicative qualified as OA
import Language.Fortran.Parser qualified as F.Parser
import Language.Fortran.Version
import Data.ByteString qualified as B
import Data.Yaml qualified as Yaml

data Cfg = Cfg
  { cfgVer :: FortranVersion
  , cfgCmd :: Cmd
  }

data Cmd = CmdEncode FilePath | CmdDecode FilePath

pCfg :: OA.Parser Cfg
pCfg = Cfg <$> pFVer <*> pCmd

pFVer :: OA.Parser FortranVersion
pFVer = OA.option (OA.maybeReader selectFortranVersion) $ OA.long "version" <> OA.short 'v' <> OA.help "Fortran version" <> OA.metavar "FORTRAN_VER"

pCmd :: OA.Parser Cmd
pCmd = OA.hsubparser $
       cmd "encode" "Encode Fortran source to JSON" (CmdEncode <$> OA.strArgument mempty)
    <> cmd "decode" "Decode JSON to Fortran source" (CmdDecode <$> OA.strArgument mempty)

cmd :: String -> String -> OA.Parser a -> OA.Mod OA.CommandFields a
cmd name desc p = OA.command name (OA.info p (OA.progDesc desc))

execParserWithDefaults :: MonadIO m => String -> OA.Parser a -> m a
execParserWithDefaults desc p = liftIO $ OA.customExecParser
    (OA.prefs $ OA.showHelpOnError)
    (OA.info (OA.helper <*> p) (OA.progDesc desc))

main :: IO ()
main = do
    cfg <- execParserWithDefaults "TODO" pCfg
    case cfgCmd cfg of
      CmdEncode fp -> do
        bs <- B.readFile fp
        case (F.Parser.byVer (cfgVer cfg)) fp bs of
          Left  e   -> print e
          Right src -> do
            B.putStr $ Yaml.encode src
      CmdDecode _fp -> error "unimplemented"
