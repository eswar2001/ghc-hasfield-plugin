{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase,TypeApplications #-}
{-# LANGUAGE NondecreasingIndentation, ScopedTypeVariables #-}
module Data.Record.Plugin.HasFieldPattern (plugin) where

import Data.Generics.Uniplate.Data
import Data.Monoid
import Control.Monad (unless)
import Control.Monad.Trans.Writer.CPS
import Data.Record.Plugin.Shim

-- for check required extensions
import Control.Monad.Except
import Language.Haskell.TH (Extension(..))
import Data.List (intersperse)
#if __GLASGOW_HASKELL__ >= 900
import GHC.Hs
import GHC.Types.SourceText
import GHC.Driver.Errors
import GHC.Types.Error
import qualified GHC.Hs      as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Utils.Logger as GHC
#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Driver.Errors (printMessages)
import GHC.Driver.Errors.Types (GhcMessage(..), DriverMessage( DriverUnknownMessage ))
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Types.Error ( mkPlainDiagnostic, DiagnosticReason(..), singleMessage
  , Messages )
import GHC.Driver.Flags (WarningFlag(..))
import GHC.Types.SrcLoc (SrcSpan, RealSrcSpan)
import GHC.Utils.Outputable (neverQualify)
import GHC.Utils.Error as GUE
import GHC.Data.Bag (unitBag)
import qualified GHC.Utils.Logger as Logger
import GHC.Utils.Error (MsgEnvelope(..))
import qualified GHC.Utils.Outputable as Out
import GHC.Utils.Error (MsgEnvelope(..), MessageClass(MCDiagnostic))
#endif

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      parsedResultAction = aux
    , pluginRecompile    = purePlugin
    }
  where
#if __GLASGOW_HASKELL__ <906
    aux ::
         [CommandLineOption]
      -> ModSummary
      -> HsParsedModule -> Hsc HsParsedModule
    aux _opts _summary parsed@HsParsedModule{
      hpm_module =L l modl@HsModule{
                     hsmodDecls   = decls
                   , hsmodImports = imports
                   } } = do
      checkEnabledExtensions l                
      let (decls', needImports) = runWriter $ transformBiM transformPat decls
      let modls = if getAny needImports then [importDecl ghcRecordsCompat True] else []
      return $ parsed {
        hpm_module = L l $ modl {
            hsmodDecls   = decls'
          , hsmodImports = imports ++ modls
          }
        }
#else
    aux ::
         [CommandLineOption]
      -> ModSummary
      -> ParsedResult -> Hsc ParsedResult
    aux _opts _summary parsed = do
      let hpm = parsedResultModule parsed
      let L l modl@HsModule{
                     hsmodDecls   = decls
                   , hsmodImports = imports
                   } = hpm_module hpm
      checkEnabledExtensions l                
      let (decls', needImports) = runWriter $ transformBiM transformPat decls
      let modls = if getAny needImports then [importDecl ghcRecordsCompat True] else []
      let modl' = modl {
            hsmodDecls   = decls'
          , hsmodImports = imports ++ modls
          }
      let hpm' = hpm { hpm_module = L l modl' }
      return $ parsed { parsedResultModule = hpm' }
#endif


-- {-------------------------------------------------------------------------------
--   Main translation
-- -------------------------------------------------------------------------------}

transformPat :: LPat GhcPs -> Writer Any (LPat GhcPs)
transformPat p
#if __GLASGOW_HASKELL__ >= 906
  | Just (L l nm, RecCon recFields@(HsRecFields flds dotdot)) <- viewConPat p
  , Unqual nm' <- nm
  , Nothing    <- dotdot
  , not (null flds)
  , Just flds' <- mapM (\fld -> getFieldSel (HsRecFields [fld] Nothing)) flds
  , parseRec (occNameString nm')
  =  mkRecPat l flds'
#else
  | Just (L l nm, RecCon (HsRecFields flds dotdot)) <- viewConPat p
  , Unqual nm' <- nm
  , Nothing    <- dotdot
  , Just flds' <- mapM getFieldSel flds
  , parseRec (occNameString nm')
  =  mkRecPat l flds'
#endif

  | otherwise
  = return p

parseRec :: String -> Bool
parseRec "REC" = True
parseRec _ = False

mkRecPat ::
     SrcSpan
  -> [(FastString, LPat GhcPs)]
  -> Writer Any (LPat GhcPs)
mkRecPat l = \case
  [] -> do
#if __GLASGOW_HASKELL__ >= 906
      return (patLoc l (BangPat noAnn (patLoc l (WildPat noExtField))))
#else
      return (patLoc l (BangPat defExt (patLoc l (WildPat defExt))))
#endif
  [(f, p)] -> do
    doImport
#if __GLASGOW_HASKELL__ >= 906
    return (patLoc l (ViewPat noAnn (mkGetField f) p))
#else
    return (patLoc l (ViewPat defExt (mkGetField f) p))
#endif
  fields -> do
    doImport  
    let x  = mkRdrUnqual $ mkVarOcc "x"
    let getFieldsTuple = simpleLam x (mkTuple [mkGetField f `mkHsApp` mkVar l x | (f, _) <- fields])
#if __GLASGOW_HASKELL__ >= 906
    let patsTuple = TuplePat noAnn [p | (_, p) <- fields] Boxed
    return (patLoc l (ViewPat noAnn getFieldsTuple (patLoc l patsTuple)))
#else
    let patsTuple = TuplePat defExt [p | (_, p) <- fields] Boxed
    return (patLoc l (ViewPat defExt getFieldsTuple (patLoc l patsTuple)))
#endif
  where
    doImport :: Writer Any ()
    doImport = tell (Any True)
    mkGetField :: FastString -> LHsExpr GhcPs
    mkGetField fieldName =
      mkVar l getField' `mkAppType` mkSelector fieldName
    getField' = mkRdrQual ghcRecordsCompat $ mkVarOcc "getField"
    mkSelector :: FastString -> LHsType GhcPs
    mkSelector = litT . HsStrTy NoSourceText
    mkTuple :: [LHsExpr GhcPs] -> LHsExpr GhcPs
    mkTuple xs = 
#if __GLASGOW_HASKELL__ >= 906
      L (GHC.noAnnSrcSpan l) (ExplicitTuple noAnn [Present noAnn x | x <- xs] Boxed)
#elif __GLASGOW_HASKELL__ >= 900
      L (GHC.noAnnSrcSpan l) (ExplicitTuple defExt [(Present defExt x) | x <- xs] Boxed)
#else
      L l (ExplicitTuple defExt [L l (Present defExt x) | x <- xs] Boxed)
#endif

ghcRecordsCompat = mkModuleName "GHC.Records.Compat"

#if __GLASGOW_HASKELL__ <= 900
getFieldSel :: LHsRecField GhcPs (LPat GhcPs) -> Maybe (FastString, LPat GhcPs)
getFieldSel (L _ (HsRecField (L _ fieldOcc) arg pun))
  | FieldOcc _ (L _ nm) <- fieldOcc
  , Unqual nm' <- nm
  = Just (occNameFS nm', if pun then nlVarPat nm' else arg)

getFieldSel _ = Nothing

#elif __GLASGOW_HASKELL__ < 906
getFieldSel :: LHsRecField GhcPs (LPat GhcPs) -> Maybe (FastString, LPat GhcPs)
getFieldSel (L _ (HsRecField _ (L _ fieldOcc) arg pun))
  | FieldOcc _ (L _ nm) <- fieldOcc
  , Unqual nm' <- nm
  = Just (occNameFS nm', if pun then nlVarPat nm' else arg)

getFieldSel _ = Nothing
#else
getFieldSel :: HsRecFields GhcPs (LPat GhcPs) -> Maybe (FastString, LPat GhcPs)
getFieldSel (HsRecFields (fld : _) _) 
  | L _ rf <- fld
  , let fieldOcc = hfbLHS rf
  , let arg      = hfbRHS rf
  , let pun      = hfbPun rf
  , L _ (FieldOcc _ (L _ nm)) <- fieldOcc
  , Unqual nm' <- nm
  = Just (occNameFS nm', if pun then nlVarPat nm else arg)

getFieldSel _ = Nothing
#endif

{-------------------------------------------------------------------------------
  Check for enabled extensions

  In ghc 8.10 and up there are DynFlags plugins, which we could use to enable
  these extensions for the user. Since this is not available in 8.8 however we
  will not make use of this for now. (There is also reason to believe that these
  may be removed again in later ghc releases.)
-------------------------------------------------------------------------------}

checkEnabledExtensions :: SrcSpan -> Hsc ()
checkEnabledExtensions l = do
    dynFlags <- getDynFlags
    let missing :: [RequiredExtension]
        missing = filter (not . isEnabled dynFlags) requiredExtensions
    unless (null missing) $
      -- We issue a warning here instead of an error, for better integration
      -- with HLS. Frankly, I'm not entirely sure what's going on there.
      issueWarning l $ vcat . concat $ [
          [text "Please enable these extensions for use with Nau.PLugin.ViewPattern:"]
        , map ppr missing
        ]
  where
    requiredExtensions :: [RequiredExtension]
    requiredExtensions = [
          RequiredExtension [DataKinds]
        , RequiredExtension [FlexibleContexts]
        , RequiredExtension [TypeApplications]
        , RequiredExtension [ViewPatterns]
        ]

-- | Required extension
--
-- The list is used to represent alternative extensions that could all work
-- (e.g., @GADTs@ and @ExistentialQuantification@).
data RequiredExtension = RequiredExtension [Extension]

instance Outputable RequiredExtension where
  ppr (RequiredExtension exts) = hsep . intersperse (text "or") $ map ppr exts

isEnabled :: DynFlags -> RequiredExtension -> Bool
isEnabled dynflags (RequiredExtension exts) = any (`xopt` dynflags) exts

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.litT'
#if __GLASGOW_HASKELL__ < 900
litT :: HsTyLit -> LHsType GhcPs
litT = noLoc . HsTyLit defExt
#elif __GLASGOW_HASKELL__ < 906
litT :: HsTyLit -> LHsType GhcPs
litT = GHC.wrapXRec @(GhcPs) . HsTyLit defExt
#else
litT :: HsTyLit GhcPs -> LHsType GhcPs
litT lit = noLocA (HsTyLit defExt lit)
#endif
-- | Construct simple lambda
--
-- Constructs lambda of the form
--
-- > \x -> e
simpleLam :: RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
simpleLam x body = mkHsLam [nlVarPat x] body

mkVar :: SrcSpan -> RdrName -> LHsExpr GhcPs
mkVar l name =
#if __GLASGOW_HASKELL__ >= 900
  L (GHC.noAnnSrcSpan l) $ HsVar defExt (L (GHC.noAnnSrcSpan l) name)
#else
  L l $ HsVar defExt (L l name)
#endif

mkAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkAppType expr typ = 
#if __GLASGOW_HASKELL__ >= 906
  GHC.wrapXRec @(GhcPs) $ HsAppType defExt expr (L NoTokenLoc HsTok) (HsWC defExt typ)
#elif __GLASGOW_HASKELL__ >= 900
  GHC.wrapXRec @(GhcPs) $ HsAppType defExt expr (HsWC defExt typ)
#else
  noLoc $ HsAppType defExt expr (HsWC defExt typ)
#endif

issueWarning :: SrcSpan -> SDoc -> Hsc ()
#if __GLASGOW_HASKELL__ < 906
issueWarning l errMsg = do
  dynFlags <- getDynFlags
#if __GLASGOW_HASKELL__ >= 900
  logger <- GHC.getLogger
  liftIO $
    printOrThrowWarnings logger dynFlags
      (listToBag [mkWarnMsg l neverQualify errMsg])
#else
  liftIO $
    printOrThrowWarnings dynFlags
      (listToBag [mkWarnMsg dynFlags l neverQualify errMsg])
#endif
#else
issueWarning l errMsg = do
  logger   <- GHC.getLogger
  dynFlags <- getDynFlags
  diag_opts <- initDiagOpts <$> getDynFlags
  print_config <- initPrintConfig <$> getDynFlags
  let diagnostic = mkPlainDiagnostic WarningWithoutFlag noHints errMsg
  let ghcMsg = GhcUnknownMessage 
             $ mkUnknownDiagnostic @GhcMessage 
             $ GhcDriverMessage 
             $ DriverUnknownMessage 
             $ mkUnknownDiagnostic @DriverMessage diagnostic
  let msgEnv = mkMsgEnvelope diag_opts l neverQualify ghcMsg
  liftIO $ printOrThrowDiagnostics logger print_config diag_opts (singleMessage msgEnv)
#endif