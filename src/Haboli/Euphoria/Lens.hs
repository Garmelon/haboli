module Haboli.Euphoria.Lens
  ( makeLensesL
  ) where

import Language.Haskell.TH
import Lens.Micro.TH
import Lens.Micro

rename :: Name -> [Name] -> Name -> [DefName]
rename _ _ name = [TopName $ mkName $ nameBase name ++ "L"]

makeLensesL :: Name -> DecsQ
makeLensesL = makeLensesWith $ lensRules & lensField .~ rename
