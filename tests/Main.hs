module Main
    ( main
    ) where

import qualified Gimlight.Action.ConsumeSpec
import qualified Gimlight.Action.DropSpec
import qualified Gimlight.Action.MeleeSpec
import qualified Gimlight.Action.MoveOneSquareSpec
import qualified Gimlight.Action.PickUpSpec
import qualified Gimlight.Action.WaitSpec
import qualified Gimlight.Actor.WalkingImagesSpec
import qualified Gimlight.ActorSpec
import qualified Gimlight.Data.EitherSpec
import qualified Gimlight.Data.MaybeSpec
import qualified Gimlight.Dungeon.Generate.ConfigSpec
import qualified Gimlight.Dungeon.GenerateSpec
import qualified Gimlight.Dungeon.Map.CellSpec
import qualified Gimlight.Dungeon.Map.JSONReaderSpec
import qualified Gimlight.Dungeon.Map.Tile.JSONReaderSpec
import qualified Gimlight.FovSpec
import qualified Gimlight.System.PathSpec
import qualified Gimlight.System.RandomSpec
import           Test.Hspec                               (Spec, describe,
                                                           hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Gimlight.Action.Consume" Gimlight.Action.ConsumeSpec.spec
    describe "Gimlight.Action.Drop" Gimlight.Action.DropSpec.spec
    describe "Gimlight.Action.Melee" Gimlight.Action.MeleeSpec.spec
    describe
        "Gimlight.Action.MoveOneSquare"
        Gimlight.Action.MoveOneSquareSpec.spec
    describe "Gimlight.Action.PickUp" Gimlight.Action.PickUpSpec.spec
    describe "Gimlight.Action.Wait" Gimlight.Action.WaitSpec.spec
    describe "Gimlight.Actor" Gimlight.ActorSpec.spec
    describe
        "Gimlight.Actor.WalkingImages"
        Gimlight.Actor.WalkingImagesSpec.spec
    describe "Gimlight.Data.Either" Gimlight.Data.EitherSpec.spec
    describe "Gimlight.Data.Maybe" Gimlight.Data.MaybeSpec.spec
    describe "Gimlight.Dungeon.Generate" Gimlight.Dungeon.GenerateSpec.spec
    describe
        "Gimlight.Dungeon.Generate.Config"
        Gimlight.Dungeon.Generate.ConfigSpec.spec
    describe "Gimlight.Dungeon.Map.Cell" Gimlight.Dungeon.Map.CellSpec.spec
    describe
        "Gimlight.Dungeon.Map.JSONReader"
        Gimlight.Dungeon.Map.JSONReaderSpec.spec
    describe
        "Gimlight.Dungeon.Map.Tile.JSONReader"
        Gimlight.Dungeon.Map.Tile.JSONReaderSpec.spec
    describe "Gimlight.Fov" Gimlight.FovSpec.spec
    describe "Gimlight.System.Path" Gimlight.System.PathSpec.spec
    describe "Gimlight.System.Random" Gimlight.System.RandomSpec.spec
