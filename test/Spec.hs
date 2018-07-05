

import Language.JavaScript.Prelude

import Language.JavaScript.Optics
import Language.JavaScript.Parser
import Protolude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Language.JavaScript.Optics" $ do
  let helloWorldSrc    = "console.log(\"hello world\");"
      helloWorldParsed = readJs helloWorldSrc

      helloWorldAstProgramGuts = case helloWorldParsed of
        (JSAstProgram a b) -> (a, b)
        _                  -> panic "unpossible!"

  context "given a simple valid javascript source" $ do

    context "_JSAST" $ do

      it "should be able to manifest a proper JSAST object" $ do

        helloWorldSrc ^? _JSAST `shouldBe` Just helloWorldParsed

  context "given a simple valid JSAST object" $ do

    context "_JSAstProgram" $ do

      it "should extract the appropriate program guts" $ do

        helloWorldParsed ^? _JSAstProgram `shouldBe` Just helloWorldAstProgramGuts
