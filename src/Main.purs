module Main where

import Prelude

import Effect (Effect)
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
  server <- createServer $ staticHandler { root: "./public", maxAge: 60, historyAPIFallback: true }

  listen server { hostname: "0.0.0.0", port: port, backlog: Nothing } $ pure unit