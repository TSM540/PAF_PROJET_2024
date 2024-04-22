module ProduitTest where 

import Produit
import Types



produit1 = Produit (ProdId 1) "Pain" 0.5 (Alimentaire Pain Frais) Local
produit2 = Produit (ProdId 2) "Poisson" 2.5 (Alimentaire Poisson Frais) Local
produit3 = Produit (ProdId 3) "IPhone 15 pro max" 1740 Electronique (Importation (Pays "Chine"))


stockProduit = StockProduit [(produit1, Quantite 10), (produit2, Quantite 5), (produit3, Quantite 1)]

-- >>> :t stockProduit
-- stockProduit :: StockProduit

-- >>> getQuantite (getProduitQuantite stockProduit produit3)
-- Quantite 1

-- >>> invariantStockProduit stockProduit
-- True
-- >>> produitConsommable produit1
-- True
-- >>> produitConsommable produit3
-- False

produit4 = Produit (ProdId 4) "Pain" 0.5 (Alimentaire Pain Frais) Local
s = removeProduct stockProduit produit3
-- >>>  s
-- StockProduit [(Produit {idProd = ProdId 1, nomProd = "Pain", prixProd = 0.5, typeProd = Alimentaire Pain Frais, production = Local},Quantite 10),(Produit {idProd = ProdId 2, nomProd = "Poisson", prixProd = 2.5, typeProd = Alimentaire Poisson Frais, production = Local},Quantite 5),(Produit {idProd = ProdId 3, nomProd = "IPhone 15 pro max", prixProd = 1740.0, typeProd = Electronique, production = Importation (Pays "Chine")},Quantite 0)]

-- >>> invariantStockProduit s
-- True

-- >>> removeProduct stockProduit produit4
-- Le produit ne se trouve pas dans le stock

-- >>> removeProduct stockProduit produit3
-- StockProduit [(Produit {idProd = ProdId 1, nomProd = "Pain", prixProd = 0.5, typeProd = Alimentaire Pain Frais, production = Local},Quantite 10),(Produit {idProd = ProdId 2, nomProd = "Poisson", prixProd = 2.5, typeProd = Alimentaire Poisson Frais, production = Local},Quantite 5),(Produit {idProd = ProdId 3, nomProd = "IPhone 15 pro max", prixProd = 1740.0, typeProd = Electronique, production = Importation (Pays "Chine")},Quantite 0)]

-- >>> acheterProduit produit1 0.2
-- False
