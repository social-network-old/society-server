{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Society Server implementation.
--

--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Spar.TypesSpec where

import Data.Id
import Data.UUID
import Imports
import Spar.Types
import Test.Hspec
import URI.ByteString
import URI.ByteString.QQ
import Web.Cookie

spec :: Spec
spec = do
  describe "mkVerdictGrantedFormatMobile" $ do
    it "1" $ do
      mkVerdictGrantedFormatMobile [uri|society://granted/$cookie/$userid|] def (Id nil)
        `shouldBe` Right [uri|society://granted/name=value/00000000-0000-0000-0000-000000000000|]
    it "2" $ do
      mkVerdictGrantedFormatMobile ([uri|http://$cookie:1039/granted|] :: URI) def (Id nil)
        `shouldBe` Right [uri|http://name=value:1039/granted|]
  describe "mkVerdictDeniedFormatMobile" $ do
    it "1" $ do
      mkVerdictDeniedFormatMobile [uri|society://$label|] "forbidden"
        `shouldBe` Right [uri|society://forbidden|]
    it "2" $ do
      mkVerdictDeniedFormatMobile [uri|http://bad/?label=$label|] "forbidden"
        `shouldBe` Right [uri|http://bad/?label=forbidden|]
