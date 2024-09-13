module Happy.Backend.GLR where

import Paths_happy_lib

glrBackendDataDir :: IO String
glrBackendDataDir = getDataDir
