{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ProduitSpec where 
import Produit
import Types
import Control.Exception (evaluate)
import Test.Hspec


produit1 :: Produit
produit1 = Produit (ProdId 1) "Pain" 0.5 (Alimentaire Pain Frais) Local
produit2 :: Produit
produit2 = Produit (ProdId 2) "Poisson" 2.5 (Alimentaire Poisson Frais) Local
produit3 :: Produit
produit3 = Produit (ProdId 3) "IPhone 15 pro max" 1740 Electronique (Importation (Pays "Chine"))


stockProduit :: StockProduit
stockProduit = StockProduit [(produit1, Quantite 10), (produit2, Quantite 5), (produit3, Quantite 1)]

produit4 :: Produit
produit4 = Produit (ProdId 4) "Pain" 0.5 (Alimentaire Pain Frais) Local
s :: StockProduit
s = removeProduct stockProduit produit3

produitSpec :: Spec
produitSpec = do
    describe "affichage du stock" $ do 
        it "affiche le stock de produits" $ do
            show stockProduit `shouldBe` "StockProduit [(Produit {idProd = ProdId 1, nomProd = \"Pain\", prixProd = 0.5, typeProd = Alimentaire Pain Frais, production = Local},Quantite 10),(Produit {idProd = ProdId 2, nomProd = \"Poisson\", prixProd = 2.5, typeProd = Alimentaire Poisson Frais, production = Local},Quantite 5),(Produit {idProd = ProdId 3, nomProd = \"IPhone 15 pro max\", prixProd = 1740.0, typeProd = Electronique, production = Importation (Pays \"Chine\")},Quantite 1)]"
    describe "getProduitQuantite" $ do
        it "récupère la quantité d'un produit qui existe " $ do
            getQuantite (getProduitQuantite stockProduit produit3) `shouldBe` Quantite 1
        it "récupère la quantité d'un produit qui n'existe pas" $ do
            evaluate ( getQuantite (getProduitQuantite stockProduit produit4)) `shouldThrow` errorCall "Produit non trouvé"

    describe "invariantStockProduit" $ do
        it "vérifie l'invariant du stock de produit" $ do
            invariantStockProduit stockProduit `shouldBe` True
        it "vérifie l'invariant du stock de produit après suppression" $ do
            invariantStockProduit s `shouldBe` True
        it "invariant avec stock vide" $ do
            invariantStockProduit (StockProduit []) `shouldBe`  True
        it "invariant avec un produit avec quantite negative" $ do
            invariantStockProduit (StockProduit [(produit1, Quantite (-1))]) `shouldBe` False
    describe "produitConsommable" $ do
        it "un produit consommable" $ do
            produitConsommable produit1 `shouldBe` True
        it "un produit non consommable" $ do
            produitConsommable produit3 `shouldBe` False
    describe "removeProduct" $ do
        it "suppression d'un produit qui n'existe pas" $ do
            evaluate (removeProduct stockProduit produit4) `shouldThrow` errorCall "Le produit ne se trouve pas dans le stock"
        it "suppression d'un produit qui existe" $ do
            show (removeProduct stockProduit produit3) `shouldBe` "StockProduit [(Produit {idProd = ProdId 1, nomProd = \"Pain\", prixProd = 0.5, typeProd = Alimentaire Pain Frais, production = Local},Quantite 10),(Produit {idProd = ProdId 2, nomProd = \"Poisson\", prixProd = 2.5, typeProd = Alimentaire Poisson Frais, production = Local},Quantite 5),(Produit {idProd = ProdId 3, nomProd = \"IPhone 15 pro max\", prixProd = 1740.0, typeProd = Electronique, production = Importation (Pays \"Chine\")},Quantite 0)]"
    describe "acheter un produit " $ do 
        it "achat d'un produit qui est fausse " $ do
            acheterProduit produit1 0.2 `shouldBe` False
        it "achat d'un produit qui est fausse " $ do
            acheterProduit produit1 0.4 `shouldBe` False
        it "achat d'un produit qui est vrai" $ do
            acheterProduit produit1 1 `shouldBe` True
        it "achat d'un produit qui est vrai" $ do
            acheterProduit produit1 0.5 `shouldBe` True