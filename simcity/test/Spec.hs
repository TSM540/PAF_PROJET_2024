import Test.Hspec ( hspec )
import FormeSpec as FS 
import ZoneSpec as ZS
import VilleSpec as VS
import VehiculeSpec
import ProduitSpec as PS
import ParkingSpec as PAS
import PrefectureSpec as PRS
import EntrepriseSpec as ES
main :: IO ()
main = hspec $ do
    FS.formeSpec
    ZS.zoneSpec
    VS.villeSpec
    VehiculeSpec.vehiculeSpec
    PS.produitSpec
    PAS.parkingSpec
    PRS.prefectureSpec
    ES.entrepriseSpec