{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Citoyen where
import Forme
import Occupation
import Types
import Maladies
import Produit
import Vehicule 
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
                                ViePersonnelle _ Nothing _ _ -> Nothing
                                  -- dans le cas où les courses est nothing (voir Types.hs ViePersonnelle)
                                ViePersonnelle _ _ Nothing _ -> Nothing
                                -- le cas où tout est bien
                                ViePersonnelle _ (Just _) (Just _) _ -> Just (Habitant p v vp)
                        _ ->  Nothing
maybeValue :: Maybe a -> a
maybeValue (Just a) = a -- note that Just a is wrapped
maybeValue Nothing = error "Nothing value"

creerImmigrant :: CitId -> Coord -> Occupation -> Nationalite -> Citoyen
creerImmigrant id c o nat = Immigrant (Personne id c o [] nat []) (Vie 0 100 100 100)


rajouterAnsSejour :: TypeSejour -> Integer -> TypeSejour
rajouterAnsSejour (Etudiant (AnsSejour ans) d) annee = Etudiant (AnsSejour (ans + annee)) d
rajouterAnsSejour (Salarie (AnsResidence ans)) annee = Salarie (AnsResidence (ans + annee))

changerDiplomeVersObtenu :: TypeSejour -> TypeSejour
changerDiplomeVersObtenu (Etudiant ansSejour _) = 
         if ansSejour >= AnsSejour 1 then Etudiant ansSejour Obtenu
          else error "L'étudiant n'a pas encore obtenu son diplome car il n'a pas étudier plus d'un ans"
changerDiplomeVersObtenu _ = error "Le type de séjour n'est pas un étudiant"



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


-- acheterVehicule :: Citoyen -> Vehicule -> Citoyen-> Citoyen
-- acheterVehicule (Habitant (Personne id c o cri nat m) v@(Vie argent sante nivFaim nivFatigue) vp) 
--             vehicule@(Vehicule idv typev imm prop pass prix) = 
--                let nv = achatVehicule vehicule (VehiculeCitoyen id) in
--                 if (argent - prix < 0) then error "Vous n'avez pas assez d'argent pour" 
--                 Habitant (Personne id c o cri nat m) (Vie (argent - prix) sante nivFaim nivFatigue) (vp {vehicules = idv : vehicules vp})
               
acheterVehiculeParCitoyen :: Citoyen -> Vehicule -> (Citoyen, Vehicule)
acheterVehiculeParCitoyen (Habitant (Personne id c o cri nat m) v@(Vie argent sante nivFaim nivFatigue) vp) 
            vehicule@(Vehicule idv typev imm prop pass prix cap) = 
               let nv = achatVehicule vehicule (VehiculeCitoyen id) in
                if argent - prix < 0 then error "Le citoyen n'a pas assez d'argent pour acheter ce véhicule"
                else ( 
                      Habitant (Personne id c o cri nat m) 
                      (Vie (argent - prix) sante nivFaim nivFatigue) 
                      (vp {vehicules = idv : vehicules vp}) 
                    ,
                    nv{passagers= []}
                )


getCitoyenId :: Citoyen -> CitId
getCitoyenId (Habitant (Personne id _ _ _ _ _) _ _) = id
