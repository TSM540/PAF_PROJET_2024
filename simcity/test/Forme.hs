import Test.Hspec

import FormeSpec as FS

main :: IO ()
main = hspec $ do
  -- Forme
  FS.spec
