module Prefecture where
import Citoyen
import Types
import Forme
import Occupation


data Prefecture = Prefecture {
    idPrefecture :: PrefId,
    nom :: String,
    citoyens :: [Citoyen]
}deriving(Eq)

-- instance de show
instance Show Prefecture where
    show (Prefecture idPrefecture nom citoyens) = "Prefecture " ++ show idPrefecture ++ " " ++ nom ++ " et gére " ++ show (length citoyens) ++ " citoyens"


naturalisation :: Prefecture -> Citoyen ->  Personne
naturalisation (Prefecture _ _ citoyens) c =
    if c `elem` citoyens
        then 
        case c of
          Habitant personne vie vp ->
              -- dans le cas où il est a des crimes
            if aDesCrimes personne
              then error "Vous avez des crimes, vous ne pouvez pas être naturalisé"
              else naturaliserEtranger personne
          Immigrant _ _ -> error "Vous Immigrant, vous ne pouvez pas être naturalisé car vous ne dispôsez pas des ressources nécessaires (Habitat, Travail)"
          Emigrant  _ -> error "Vous êtes bien émigré, vous ne pouvez pas être naturalisé "
    else error " Vous n'êtes dépendant de cette préfecture"


naturaliserEtranger :: Personne ->  Personne
naturaliserEtranger personne =
  case nationalite personne of
    Etranger (Etudiant  ansSejour diplome ) ->
                                            if (diplome ==  Obtenu) &&( ansSejour >= AnsSejour 2)
                                              then  personne {nationalite = Francais}
                                              else error " diplome non obtenu ou ansSejour < 2"
    Etranger (Salarie  ansResidence) ->
      if ansResidence >= AnsResidence 5
        then  personne {nationalite = Francais}
        else error " ansResidence < 5"
    _ -> error "Vous êtes déjà français"

aDesCrimes :: Personne -> Bool
aDesCrimes personne = not (null (crimes personne))


-- c1= Habitant
--           ( Personne
--               { 
--                 idCit = CitId 1
--               , coord = C 10 10
--               , occupation = Travailler 100.0
--               , crimes = []
--               , nationalite = Etranger (Salarie (AnsResidence 5))
--               }
--           )
--           ( Vie
--               { argentEnBanque = 500
--               , sante = 80
--               , niveauFaim = 60
--               }
--           )
--           ( ViePersonnelle
--               { maison = BatId 1
--               , travail = Just (BatId 2)
--               , courses = Just (BatId 3)
--               }
--           )
-- c2= Habitant
--           ( Personne
--               { 
--                 idCit = CitId 2
--               , coord = C 10 10
--               , occupation = Travailler 100.0
--               , crimes = []
--               , nationalite = Etranger (Etudiant (AnsSejour 2 ) Obtenu)
--               }
--           )
--           ( Vie
--               { argentEnBanque = 500
--               , sante = 80
--               , niveauFaim = 60
--               }
--           )
--           ( ViePersonnelle
--               { maison = BatId 1
--               , travail = Just (BatId 2)
--               , courses = Just (BatId 3)
--               }
--           )
-- c3= Habitant
--           ( Personne
--               { 
--                 idCit = CitId 3
--               , coord = C 10 10
--               , occupation = Travailler 100.0
--               , crimes = []
--               , nationalite = Francais
--               }
--           )
--           ( Vie
--               { argentEnBanque = 500
--               , sante = 80
--               , niveauFaim = 60
--               }
--           )
--           ( ViePersonnelle
--               { maison = BatId 1
--               , travail = Just (BatId 2)
--               , courses = Just (BatId 3)
--               }
--           )
-- prefecture = Prefecture
--   { idPrefecture = PrefId 1
--   , nom = "Paris"
--   , citoyens =
--       [ 
--         c1,c2,c3
--       ]
--   }

-- -- Naturalisation de l'habitant
-- np1 = naturalisation prefecture c1

-- -- >>> show np1
-- -- "Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais}"
-- np2 = naturalisation prefecture c2
-- -- >>> show np2
-- -- "Personne {idCit = CitId 2, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais}"
-- np3 = naturalisation prefecture c3

-- -- >>> show np3
-- Vous êtes déjà français
