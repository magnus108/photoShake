module PhotoShake.ShakeError
    ( ShakeError(..)
    , throw
    ) where


import Control.Exception


data ShakeError
    = OutDirectoryMissing
    | DumpMissing
    | DumpConfigFileMissing
    | ConfigMissing
    | ConfigDumpMissing
    | ConfigOutMissing
    | ConfigLocationMissing
    | ReadLocationFile
    | ParseLocationFile
    | FindPhotographee
    deriving (Eq)


instance Show ShakeError where
    show DumpMissing = "Kunne ikke finde dumpmappe"
    show OutDirectoryMissing = "Kunne ikke finde outmappe"
    show ConfigMissing = "Konfigurations filen mangler eller indeholder fejl"
    show ConfigDumpMissing = "Konfigurations filen mangler dump konfiguration"
    show DumpConfigFileMissing = "Dump konfigurationen findes ikke"
    show ConfigOutMissing = "Konfigurations filen mangler out konfiguration"
    show ConfigLocationMissing = "Konfigurations filen mangler lokations konfiguration"
    show ReadLocationFile = "Kunne ikke finde lokationsfil"
    show ParseLocationFile = "Der er fejl i lokationsfil"
    show FindPhotographee = "Kunne ikke finde elev i lokations fil"


instance Exception ShakeError
