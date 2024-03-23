module VilleSpec (spec) where

import Test.Hspec
import Ville
import Zone
import Forme

spec :: Spec
spec = do
  describe "construit" $ do
    it "maintient l'invariant de la ville" $ do
      let villeInitiale = Ville { villeZones = Map.empty, villeCitoyens = Map.empty }
          zoneAAjouter = Route (Rectangle (C 10 10) 1 1)
          villeApresConstruit = construit villeInitiale zoneAAjouter
      invariantVille villeApresConstruit `shouldBe` True

    it "ajoute correctement une zone à la ville" $ do
      let villeInitiale = Ville { villeZones = Map.empty, villeCitoyens = Map.empty }
          zoneAAjouter = Route (Rectangle (C 10 10) 1 1)
          villeApresConstruit = construit villeInitiale zoneAAjouter
      Map.size (villeZones villeApresConstruit) `shouldBe` 1
      Map.lookup (prochainZonId villeInitiale) (villeZones villeApresConstruit) `shouldBe` Just zoneAAjouter
