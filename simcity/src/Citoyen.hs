module Citoyen where 
import Forme 
import Occupation
import Types


data Citoyen =    Immigrant   Personne Vie   
                | Habitant    Personne  Vie ViePersonnelle
                | Emigrant    Personne   

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
