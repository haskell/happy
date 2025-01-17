module Happy.Backend.GLR where

import Happy.Paths

glrBackendDataDir :: IO String
glrBackendDataDir = getDataDir
