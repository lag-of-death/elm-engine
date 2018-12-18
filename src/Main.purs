module Main where

import Prelude ((<>), ($), (<$>), Unit, bind, show)

import Effect (Effect)
import Effect.Console
import Node.HTTP (createServer, listen)
import StaticServe (staticHandler)
import Node.Process (lookupEnv)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)


parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


main :: Effect Unit
main = do
  let portEnvAsMaybeInEffect = lookupEnv "PORT"

  port <- (\portEnvAsMaybe -> parseInt $ fromMaybe "5000" portEnvAsMaybe ) <$> portEnvAsMaybeInEffect
  server <- createServer $ staticHandler { root: "./public", maxAge: 0, historyAPIFallback: true }

  listen server { hostname: "0.0.0.0", port: port, backlog: Nothing } $ log ("Server running on port: " <> show port)