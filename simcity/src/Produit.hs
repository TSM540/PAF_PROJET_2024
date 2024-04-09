module Produit where 
import Types
-- Constructeurs


data Produit = Produit {
    idProd :: ProdId,
    nomProd :: String,
    prixProd :: Float,
    typeProd :: TypeProduit,
    production :: TypeProduction    

} deriving (Show, Eq)


-- data StockProduit = StockProduit {
--     produit :: Produit,
--     quantite :: Int
-- } deriving (Show, Eq)
newtype StockProduit = StockProduit [(Produit, Quantite)] deriving (Show, Eq)
newtype Quantite = Quantite Integer deriving (Show, Eq, Ord)




data TypeProduit = Alimentaire TypeAlimentation EtatProduit
                    | Electronique 
                    | Meuble 
                    | Hygiene 
                     deriving (Show, Eq)



data TypeAlimentation =  Viande 
                        | Poisson 
                        | Legume  
                        | Fruit 
                        | Lait 
                        | Pain 
                        | Patisserie 
                        | Boisson deriving (Show, Eq)
data EtatProduit = Frais 
                    | Perime 
                    | Cuit deriving (Show, Eq)

data TypeProduction = Local
                        | Importation Pays deriving (Show, Eq)


acheterProduit :: Produit -> Float -> Bool
acheterProduit (Produit _ _ prixProd _ _) argent = prixProd  <= argent

produitConsommable :: Produit -> Bool
produitConsommable (Produit _ _ _ (Alimentaire _ _) _) = True
produitConsommable _ = False

getProduitQuantite :: StockProduit -> Produit -> (Produit,Quantite )
getProduitQuantite (StockProduit []) _ = error "Produit non trouvé"
getProduitQuantite (StockProduit ((p,q):xs)) produit = 
                if p == produit then 
                  (p,q)
                else getProduitQuantite (StockProduit xs) produit

removeProduct :: StockProduit -> Produit -> StockProduit
removeProduct (StockProduit []) _ = error "Produit non trouvé"
removeProduct (StockProduit ((p,q):xs)) produit = 
                if p == produit then 
                    let q' = quantiteToInteger q in
                    StockProduit ((p, Quantite(q'-1)):xs)
                else removeProduct (StockProduit xs) produit

quantiteToInteger :: Quantite -> Integer
quantiteToInteger (Quantite q) = q

getProduite :: (Produit, Quantite) -> Produit
getProduite (p,_) = p

getQuantite :: (Produit, Quantite) -> Quantite
getQuantite (_,q) = q
-- ! invariants

invariantStockProduit :: StockProduit -> Bool
invariantStockProduit  = stockEmptyOrPositif 

-- les valeurs de quantité sont positives ou 0

quantityEmptyOrPositif :: (Produit,Quantite) -> Bool
quantityEmptyOrPositif (_,Quantite q) = q >= 0

-- on map sur la liste de produit pour vérifier que les quantités sont positives ou 0
stockEmptyOrPositif :: StockProduit -> Bool
stockEmptyOrPositif (StockProduit []) = True
stockEmptyOrPositif (StockProduit (x:xs)) = 
                quantityEmptyOrPositif x 
                && stockEmptyOrPositif (StockProduit xs)
