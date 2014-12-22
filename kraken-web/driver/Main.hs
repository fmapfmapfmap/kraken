{-# LANGUAGE TemplateHaskell #-}
module Main where


import           Kraken.Web
import           Kraken.Web.Client ( mkJqueryBindings )

main :: IO ()
main = mkJqueryBindings "static/js/kraken-web-client.js" >> run
