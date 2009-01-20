#!/bin/bash
runhaskell Setup.hs configure -p
runhaskell Setup.hs build
runhaskell Setup.hs install