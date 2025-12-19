{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DataKinds #-}

-- | Thin compatibility layer around GHC
--
-- This should be the only module with GHC-specific CPP directives, and the
-- rest of the plugin should not import from any GHC modules directly.
module Data.Record.Plugin.Shim (
    -- * Miscellaneous
    importDecl
  , conPat
  , mkFunBind
  , HsModule
  , LHsModule
  , LRdrName
  , pattern GHC.HsModule
  , putLogMsg
  , patLoc
  , viewConPat

    -- * Extensions
  , HasDefaultExt(..)

    -- * Generalized @forall@
#if __GLASGOW_HASKELL__ >= 900
  , HsTyVarBndr
  , LHsTyVarBndr
#endif
  , hsFunTy
  , userTyVar
  , kindedTyVar
  , hsTyVarLName
  , setDefaultSpecificity

    -- * Re-exports

    -- The whole-sale module exports are not ideal for preserving compatibility
    -- across ghc versions, but we'll deal with this on a case by case basis.
#if __GLASGOW_HASKELL__ < 900
  , module Bag
  , module BasicTypes
  , module ErrUtils
  , module GHC
  , module GhcPlugins
  , module HscMain
  , module NameCache
  , module TcEvidence
  , module GHC.Types.Name.Cache
#else
  , module GHC.Data.Bag
  , module GHC.Driver.Main
  -- , module GHC.Hs
  , module GHC.Plugins
  , module GHC.Tc.Types.Evidence
  , module GHC.Types.Basic
  , module GHC.Utils.Error
#endif
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NE

#if __GLASGOW_HASKELL__ < 900

import GHC.Hs.ImpExp (XImportDeclPass (..))
import Bag (listToBag, emptyBag)
import BasicTypes (SourceText(NoSourceText))
import ConLike (ConLike)
import ErrUtils (mkErrMsg, mkWarnMsg)
import GHC hiding (AnnKeywordId(..), HsModule, exprType, typeKind, mkFunBind)
import GhcPlugins hiding ((<>), getHscEnv, putLogMsg)
import HscMain (getHscEnv)
import NameCache (NameCache(nsUniqs))
import PatSyn (PatSyn)
import TcEvidence (HsWrapper(WpHole))

import qualified GHC
import qualified GhcPlugins as GHC

#else

import GHC.Core.Class (Class)
import GHC.Core.ConLike (ConLike)
import GHC.Core.PatSyn (PatSyn)
import GHC.Data.Bag (listToBag, emptyBag)
import GHC.Driver.Main (getHscEnv)
import GHC.Hs hiding (LHsTyVarBndr, HsTyVarBndr, HsModule, mkFunBind)
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Plugins hiding ((<>), getHscEnv, putLogMsg)
import GHC.Tc.Types.Evidence (HsWrapper(WpHole))
import GHC.Types.SourceText (SourceText(NoSourceText))
import qualified GHC.Parser.Annotation as GHC
import Unsafe.Coerce (unsafeCoerce)
#if __GLASGOW_HASKELL__ < 906
import GHC.Types.Name.Cache (NameCache(nsUniqs))
#endif

import GHC.Utils.Error (Severity(..))
import GHC.Types.Basic
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Hs      as GHC
import qualified GHC.Plugins as GHC
#endif


{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: ModuleName -> Bool -> LImportDecl GhcPs
#if __GLASGOW_HASKELL__ > 900
importDecl name qualified = GHC.wrapXRec @(GhcPs) $
#else
importDecl name qualified = GHC.noLoc $
#endif
    ImportDecl {
#if __GLASGOW_HASKELL__ <906
    ideclExt = GHC.noAnn
#else
    ideclExt =
      XImportDeclPass
        { ideclAnn        = GHC.noAnn
        , ideclSourceText = NoSourceText
        , ideclImplicit  = False
        }
#endif
#if __GLASGOW_HASKELL__ < 906
      , ideclSourceSrc = NoSourceText
#endif
#if __GLASGOW_HASKELL__ >= 900
    , ideclName = GHC.wrapXRec @(GhcPs) name
#else
    , ideclName = GHC.noLoc name
#endif
#if __GLASGOW_HASKELL__ < 906
    , ideclPkgQual   = Nothing
#else
    , ideclPkgQual   = GHC.NoRawPkgQual
#endif
    , ideclSafe     = False
#if __GLASGOW_HASKELL__ < 906
    , ideclImplicit = False
#endif

    , ideclAs       = Nothing
#if __GLASGOW_HASKELL__ < 906
    , ideclHiding = Nothing
#else
    , ideclImportList = Nothing
#endif

#if __GLASGOW_HASKELL__ < 810
    , ideclQualified = qualified
#else
    , ideclQualified = if qualified then QualifiedPre else NotQualified
#endif
#if __GLASGOW_HASKELL__ < 900
    , ideclSource = False
#else
    , ideclSource = NotBoot
#endif
  }

conPat :: Located RdrName -> HsConPatDetails GhcPs -> Pat GhcPs
#if __GLASGOW_HASKELL__ < 900
conPat x y = ConPatIn x y
#else
conPat x y = ConPat (GHC.EpAnnNotUsed) (GHC.wrapXRec @(GhcPs) (GHC.unLoc x)) y
#endif

mkFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)] -> HsBind GhcPs
#if __GLASGOW_HASKELL__ < 810
mkFunBind = GHC.mkFunBind
#else
mkFunBind lrName lMatch = GHC.mkFunBind Generated (GHC.l2n $ GHC.reLocA lrName) lMatch
#endif

#if __GLASGOW_HASKELL__ < 900
type HsModule = GHC.HsModule GhcPs
#else
type HsModule = GHC.HsModule
#endif

#if __GLASGOW_HASKELL__ < 906
type LHsModule = Located HsModule
#else
type LHsModule = XRec GhcPs (HsModule GhcPs)
#endif

type LRdrName  = Located RdrName

#if __GLASGOW_HASKELL__ < 900
putLogMsg :: DynFlags -> WarnReason -> Severity -> SrcSpan -> SDoc -> IO ()
putLogMsg flags reason sev srcspan =
    GHC.putLogMsg flags reason sev srcspan (defaultErrStyle flags)
#else
putLogMsg :: GHC.LogAction
putLogMsg = GHC.defaultLogAction
#endif

{-------------------------------------------------------------------------------
  Extensions
-------------------------------------------------------------------------------}

class HasDefaultExt a where
  defExt :: a

#if __GLASGOW_HASKELL__ < 810
instance HasDefaultExt NoExt where
  defExt = noExt
#else
instance HasDefaultExt NoExtField where
  defExt = noExtField
#endif

#if __GLASGOW_HASKELL__ < 906
instance HasDefaultExt LayoutInfo where
  defExt = NoLayoutInfo
instance HasDefaultExt (EpAnn [AddEpAnn]) where
  defExt = GHC.noAnn
instance HasDefaultExt SrcSpan where
  defExt = GHC.noSrcSpan
#endif

{-------------------------------------------------------------------------------
  Generalized @forall@ in 9.0
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 900
type  HsTyVarBndr pass =  GHC.HsTyVarBndr () pass
type LHsTyVarBndr pass = GHC.LHsTyVarBndr () pass
#endif

#if __GLASGOW_HASKELL__ < 900
hsFunTy :: XFunTy pass -> LHsType pass -> LHsType pass -> HsType pass
hsFunTy = HsFunTy
#elif __GLASGOW_HASKELL__ < 906
hsFunTy :: XFunTy pass -> LHsType pass -> LHsType pass -> HsType pass
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NormalSyntax)
#else
hsFunTy :: XFunTy pass -> LHsType pass -> LHsType pass -> HsType pass
hsFunTy ext l1 l2 = 
  let arrow = unsafeCoerce (noLocA (unLoc noHsUniTok))
  in HsFunTy ext arrow l1 l2
#endif

#if __GLASGOW_HASKELL__ < 900
userTyVar ::
     XUserTyVar pass
  -> Located (IdP pass)
  -> HsTyVarBndr pass
userTyVar = UserTyVar
#else
userTyVar ::
     XUserTyVar pass
  -> XRec pass (IdP pass)
  -> HsTyVarBndr pass
userTyVar ext loc = UserTyVar ext () loc
#endif

#if __GLASGOW_HASKELL__ < 900
kindedTyVar ::
     XKindedTyVar pass
  -> Located (IdP pass)
  -> LHsKind pass
  -> HsTyVarBndr pass
kindedTyVar = KindedTyVar
#else
kindedTyVar ::
     XKindedTyVar pass
  -> XRec pass (IdP pass)
  -> LHsKind pass
  -> HsTyVarBndr pass
kindedTyVar ext = KindedTyVar ext ()
#endif

-- | Like 'hsTyVarName', but don't throw away the location information
hsTyVarLName :: HsTyVarBndr GhcPs -> GHC.LIdP GhcPs
#if __GLASGOW_HASKELL__ < 900
hsTyVarLName (UserTyVar   _ n  ) = n
hsTyVarLName (KindedTyVar _ n _) = n
hsTyVarLName _ = panic "hsTyVarLName"
#else
hsTyVarLName (UserTyVar   _ _ n  ) = n
hsTyVarLName (KindedTyVar _ _ n _) = n
#endif

#if __GLASGOW_HASKELL__ < 900
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr pass
setDefaultSpecificity = id
#else
setDefaultSpecificity :: GHC.LHsTyVarBndr () GhcPs -> GHC.LHsTyVarBndr Specificity GhcPs
setDefaultSpecificity x = GHC.wrapXRec @(GhcPs) $ case GHC.unXRec @(GhcPs) x of
    UserTyVar   ext () name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext () name kind -> KindedTyVar ext SpecifiedSpec name kind
    -- XTyVarBndr  ext              -> XTyVarBndr  ext
#endif

patLoc :: SrcSpan -> Pat (GhcPass id) -> LPat (GhcPass id)
#if __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ <= 920
patLoc l p = L (GHC.noAnnSrcSpan l) p
#else
patLoc _ p = p
#endif

#if __GLASGOW_HASKELL__ < 810
viewConPat :: LPat (GhcPass id) -> Maybe (Located (IdP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (ConPatIn a b) = Just (a, b)
#elif __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ < 900
viewConPat :: LPat (GhcPass id) -> Maybe (Located (IdP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (L _ (ConPatIn a b)) = Just (a, b)
#elif __GLASGOW_HASKELL__ >= 900
viewConPat :: LPat (GhcPs) -> Maybe (Located (ConLikeP (GhcPs)), HsConPatDetails (GhcPs))
viewConPat (L _ (ConPat _ext a b)) = Just (GHC.reLocN a, b)
#endif
viewConPat _ = Nothing

