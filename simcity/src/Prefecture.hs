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

