port module Main exposing (..)

import KsSignalTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit KsSignalTests.all


port emit : ( String, Value ) -> Cmd msg
