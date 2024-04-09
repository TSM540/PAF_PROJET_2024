module Citoyen where
import Forme
import Occupation
import Types
import Maladies
import Produit

data Citoyen =    Immigrant   Personne Vie
                | Habitant    Personne  Vie ViePersonnelle
                | Emigrant    Personne
            deriving(Eq)

-- instance Citoyen 

instance Show Citoyen where
    show (Immigrant personne vie) = "Immigrant : " ++ show personne ++ ", Vie : " ++ show vie
    show (Habitant personne vie viePersonnelle) = "Habitant : " ++ show personne ++ ", Vie : " ++ show vie ++ ", Vie Personnelle : " ++ show viePersonnelle
    show (Emigrant personne) = "Emigrant : " ++ show personne
-- transformer un immigrant en habitant
immigrantToHabitant :: Citoyen -> ViePersonnelle -> Maybe Citoyen
immigrantToHabitant c vp= case c of
                        Immigrant p v ->  case vp of
                                -- dans le cas où la travail est nothing (voir Types.hs ViePersonnelle)
                                ViePersonnelle _ Nothing _ -> Nothing
                                  -- dans le cas où les courses est nothing (voir Types.hs ViePersonnelle)
                                ViePersonnelle _ _ Nothing -> Nothing
                                -- le cas où tout est bien
                                ViePersonnelle _ (Just _) (Just _) -> Just (Habitant p v vp)
                        _ ->  Nothing
maybeValue :: Maybe a -> a
maybeValue (Just a) = a -- note that Just a is wrapped
maybeValue Nothing = error "Nothing value"

creerImmigrant :: CitId -> Coord -> Occupation -> Nationalite -> Citoyen
creerImmigrant id c o nat = undefined

-- retourne la personne d'un citoyen
getPersonne :: Citoyen -> Personne
getPersonne  (Habitant p _ _) = p
getPersonne  (Immigrant p _  ) = p
getPersonne  (Emigrant p) = p

-- retourner la vie d'un citoyen
getVie :: Citoyen -> Vie
getVie  (Habitant _ v _) = v
getVie  (Immigrant _ v) = v 
getVie _ = error "Emigrant n'a pas de vie"

-- retourner la vie personnelle d'un citoyen
getViePersonnelle :: Citoyen -> ViePersonnelle
getViePersonnelle  (Habitant _ _ vp) = vp
getViePersonnelle _ = error "Emigrant ou immigrant n'a pas de vie personnelle"


-- -- Données personnelles de l'immigrant
-- personne = Personne {
--   coord = C 10 10,
--   occupation = Travailler 100.0,
--   crimes = [],
--   nationalite = Etranger (Salarie (AnsResidence 1))
-- }

-- -- Vie de l'immigrant
-- vie = Vie {
--   argentEnBanque = 500,
--   sante = 80,
--   niveauFaim = 60
-- }

-- -- L'immigrant n'a pas encore de vie personnelle dans la ville
-- vp = ViePersonnelle {
--   maison = BatId 0, -- Pas de maison attribuée
--   travail = Nothing, -- Pas de travail encore
--   courses = Nothing  -- Pas de courses prévues
-- }

-- immigrant = Immigrant personne vie

-- -- On attribue une vie personnelle à l'immigrant
-- nouvelleViePersonnelle = ViePersonnelle {
--   maison = BatId 1, -- On lui attribue la maison BatId 1
--   travail = Just (BatId 2), -- On lui attribue le travail BatId 2
--   courses = Just (BatId 3)  -- Pas de courses prévues pour le moment
-- }

-- -- On tente de transformer l'immigrant en habitant
-- maybeHabitant = immigrantToHabitant immigrant nouvelleViePersonnelle

-- -- On utilise maybeValue pour extraire l'habitant si la transformation a réussi
-- habitant = maybeValue maybeHabitant

-- -- Affichage de l'habitant (si la transformation a réussi)

-- -- >>>   immigrantToHabitant immigrant vp
-- -- Nothing

-- -- >>> show habitant
-- -- "Habitant : Personne {coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Etranger (Salarie (AnsResidence 1))}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 60}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3)}"



-- invariant Citoyen
invCitoyen :: Citoyen -> Bool
invCitoyen (Immigrant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue)) = argentPositif argent
                                                                  && entre0et100 sante
                                                                  && entre0et100 nivFaim
                                                                  && entre0et100 nivFatigue
                                                                  && propNationalite nat

invCitoyen (Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp) = argentPositif argent
                                                                    && entre0et100 sante
                                                                    && entre0et100 nivFaim
                                                                    && entre0et100 nivFatigue
                                                                      && propNationalite nat
invCitoyen (Emigrant (Personne id c o cri nat m)) =   propNationalite nat

-- que la santé/faim/fatigue est entre 0 et 100
entre0et100 :: Integer -> Bool
entre0et100 s = s >= 0 && s <= 100


-- un étudiant doit avoir un diplome obtenu sous deux ansou en cours avec un nombre d'année d'étude supérieur à 0
propNationalite :: Nationalite -> Bool
propNationalite nat = case nat of
                           Francais -> True
                           Etranger typesejour ->
                                 case typesejour of
                                   Etudiant (AnsSejour ans) d
                                       -> case d of
                                           Obtenu -> ans >= 1
                                           EnCours (AnneesEtudesPrevu ans) -> ans >= 1

                                   Salarie (AnsResidence ans) -> ans >= 0


-- que l'argent en banque est positif
argentPositif :: Float -> Bool
argentPositif argent = argent >= 0

-- si la fatigue est supérieure 90 revenir a la maison et son occupation c'est travailer 
-- alors on rajoute la soomme journalière a son argent en banque, on le fait deplacer a la maison, diminuer la fatigue a 0 et augmenter la santé de 100
revenirMaison :: Citoyen -> Coord-> Maybe Citoyen
-- on prend notre citoyen et les coordonnées de sa maison, supposons que dès qu'il arrive a la maison il est plus fatigué de 5 point, et quand il se deplace il a mangé et n'a plus faim
revenirMaison c coordMaison = case c of
                    Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp
                      -> if nivFatigue >= 90 &&(case o of Travailler _ -> True; _ -> False) -- si il est fatigué et il  travail
                          then Just (Habitant (Personne id c (SeDeplacer coordMaison) cri nat m ) (Vie (argent + sommeJournaliere o) sante 0 (max (nivFatigue - 5) 0) ) vp)
                          else Nothing
                    _ -> Nothing

dormir :: Maybe Citoyen -> Int -> Citoyen
dormir ( Just (Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp ) ) nb=
                                       Habitant (Personne id c (Dormir nb) cri nat m) (Vie argent 100 nivFaim 0) vp
                                        --- on change l'occupation de notre citoyen a dormir, on met sa fatigue a 0 et la santé a 100 
dormir Nothing  _ = error "Il n'y a pas de citoyen fournit"



-- Citoyen avec des valeurs valides
citoyenValide = Habitant Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Etranger (Etudiant (AnsSejour 2) Obtenu),
      maladies = []
      }
    Vie
      { argentEnBanque = 500.0
      , sante = 80
      , niveauFaim = 60
      , niveauFatigue = 40
      }
   ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3)
      }


-- Citoyen avec une santé négative
citoyenSanteNegative = Habitant
  Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Francais,
      maladies = []
      }
  Vie
      { argentEnBanque = 500.0
      , sante = -10
      , niveauFaim = 60
      , niveauFatigue = 40
      }
   ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3)
      }




-- Test de l'invariant
-- putStrLn $ show $ invCitoyen citoyenValide -- True
-- putStrLn $ show $ invCitoyen citoyenSanteNegative -- False

-- >>> invCitoyen citoyenValide
-- True
-- >>> invCitoyen citoyenSanteNegative
-- False

-- Habitant fatigué et qui travaille
habitantFatigueTravail = Habitant
  Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Francais,
      maladies = []
      }
  Vie
      { argentEnBanque = 500.0
      , sante = 80
      , niveauFaim = 60
      , niveauFatigue = 95
      }
  ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3)
      }

-- Coordonnées de la maison
coordMaison = C 1 1

-- Test de la fonction
maybeHabitant = revenirMaison habitantFatigueTravail coordMaison
val = maybeValue maybeHabitant
-- >>> show val
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Se d\233placer vers C {cx = 1, cy = 1}, crimes = [], nationalite = Francais}, Vie : Vie {argentEnBanque = 600.0, sante = 80, niveauFaim = 0, niveauFatigue = 90}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3)}"

-- Habitant fatigué
habitantFatigue = Habitant
  Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Francais,
      maladies = []
      }
   Vie
      { argentEnBanque = 500.0
      , sante = 80
      , niveauFaim = 60
      , niveauFatigue = 95
      }
  ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3)
      }


-- Nombre d'heures de sommeil
nbHeuresSommeil = 8

-- Test de la fonction
habitant = dormir (Just habitantFatigue) nbHeuresSommeil

-- Affichage de l'habitant
-- >>> show habitant
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Dormir 8 heures, crimes = [], nationalite = Francais}, Vie : Vie {argentEnBanque = 500.0, sante = 100, niveauFaim = 60, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3)}"

seReveiller :: Citoyen -> Citoyen
seReveiller (Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp) =
                    Habitant (Personne id c ALaMaison cri nat m) (Vie argent sante nivFaim nivFatigue) vp

seDeplacer :: Citoyen -> Coord -> Citoyen
seDeplacer (Habitant (Personne id c o cri nat m) v vp) coord =
                    Habitant (Personne id coord (SeDeplacer coord) cri nat m) v vp


-- manger réduit le niveau de faim à 0
manger :: Citoyen -> Produit -> Citoyen
manger (Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp) produit =
                    if produitConsommable produit then Habitant (Personne id c Manger cri nat m) (Vie argent sante 0 nivFatigue) vp 
                    else error "Le produit n'est pas consommable"

-- cuisiner rajoutes le niveau de fatigue de 5
cuisiner :: Citoyen -> Produit-> (Citoyen,Produit)
cuisiner citoyen@(Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp) 
            produit =
              case typeProd produit of 
                Alimentaire t etat -> if etat == Frais || etat == Cuit then 
                                        (Habitant (Personne id c Cuisiner cri nat m) (Vie argent sante nivFaim (min (nivFatigue+5) 100)) vp
                                        ,produit {typeProd = Alimentaire t Cuit})
                                      else error "Le produit n'est pas consommable"
                _ -> error "Le citoyen ne peut pas cuisiner ce produit"

mourir :: Citoyen -> String
mourir c = "Le citoyen est mort"

tomberMalade :: Citoyen -> Maladie -> Citoyen 
tomberMalade citoyen@(Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp)  maladie = 
    case typeMaladie maladie of 
            Mortelle -> error "La maladie est mortelle"
            -- si la maladie est infecteieuse on 
            Infectieuse -> Habitant (Personne id c o cri nat (maladie:m)) (Vie argent (max 0 (sante - 10)) nivFaim nivFatigue) vp
            Chronique -> Habitant (Personne id c o cri nat (maladie:m)) (Vie argent sante nivFaim nivFatigue) vp


guerirD'uneMaladie :: Citoyen -> Maladie -> Citoyen
guerirD'uneMaladie citoyen@(Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp) maladie = 
    case typeMaladie maladie of 
            Mortelle -> error "La maladie est mortelle"
            -- si la maladie est infecteieuse on 
            Infectieuse -> Habitant (Personne id c o cri nat (filter (/= maladie) m)) (Vie argent (min 100 (sante + 10)) nivFaim nivFatigue) vp
            Chronique -> Habitant (Personne id c o cri nat (filter (/= maladie) m)) (Vie argent sante nivFaim nivFatigue) vp

