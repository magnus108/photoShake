module PhotoShake.ShakeError
    ( ShakeError(..)
    , throw
    ) where


import Control.Exception


data ShakeError
    = DoneshootingDirectoryMissing
    | DagsdatoDirectoryMissing
    | DumpMissing
    | DumpConfigFileMissing
    | DoneshootingConfigFileMissing
    | DagsdatoConfigFileMissing
    | ConfigMissing
    | ConfigDumpMissing
    | ConfigDoneshootingMissing
    | ConfigDagsdatoMissing
    | ConfigLocationMissing
    | ConfigShootingMissing
    | ReadLocationFile
    | ParseLocationFile
    | FindPhotographee
    | ShootingConfigFileMissing
    | LocationConfigFileMissing
    deriving (Eq)


instance Show ShakeError where
    show DumpMissing = "Kunne ikke finde dumpmappe"
    show DoneshootingDirectoryMissing = "Kunne ikke finde doneshootingmappe"
    show DagsdatoDirectoryMissing = "Kunne ikke finde dagsdatomappe"
    show ConfigMissing = "Konfigurations filen mangler eller indeholder fejl"
    show ConfigDumpMissing = "Konfigurations filen mangler dump konfiguration"
    show DumpConfigFileMissing = "Dump konfigurationen findes ikke"
    show DoneshootingConfigFileMissing = "Doneshooting konfigurationen findes ikke"
    show DagsdatoConfigFileMissing = "Dagsdato konfigurationen findes ikke"
    show ConfigDoneshootingMissing = "Konfigurations filen mangler doneshooting konfiguration"
    show ConfigDagsdatoMissing = "Konfigurations filen mangler dagsdato konfiguration"
    show ConfigLocationMissing = "Konfigurations filen mangler lokations konfiguration"
    show ConfigShootingMissing = "Konfigurations filen mangler shooting type"
    show ReadLocationFile = "Kunne ikke finde lokationsfil"
    show ParseLocationFile = "Der er fejl i lokationsfil"
    show FindPhotographee = "Kunne ikke finde elev i lokations fil"
    show ShootingConfigFileMissing = "Shooting type mangler"
    show LocationConfigFileMissing = "lokations konfiguration mangler"


instance Exception ShakeError
