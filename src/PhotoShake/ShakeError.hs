module PhotoShake.ShakeError
    ( ShakeError(..)
    , throw
    ) where


import Control.Exception


data ShakeError
    = DoneshootingDirectoryMissing
    | ConfigIdMissing 
    | ConfigGradeMissing
    | PhotographersConfigFileMissing
    | BuiltConfigFileMissing
    | DagsdatoDirectoryMissing
    | DumpMissing
    | ConfigPhotographerMissing
    | DumpConfigFileMissing
    | DoneshootingConfigFileMissing
    | DagsdatoConfigFileMissing
    | ConfigBuiltMissing
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
    | BadCsv
    | JPGMissing
    | SessionsConfigFileMissing
    | ConfigSessionMissing
    | SessionsConfigParseError
    | GradeConfigFileMissing 
    | IdConfigFileMissing 
    deriving (Eq)


instance Show ShakeError where
    show BadCsv = "lokationsfil er ikke en csv"
    show JPGMissing = "CR3 og JPG stemmer ikke overens"
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
    show SessionsConfigFileMissing = "Session type mangler"
    show SessionsConfigParseError = "Session file parse fejl"
    show ConfigSessionMissing = "Session mangler konfiguration"
    show ConfigPhotographerMissing = "Konfigurations filen mangler photograf konfiguration"
    show PhotographersConfigFileMissing = "Photograf mangler"
    show BuiltConfigFileMissing = "Built mangler"
    show ConfigGradeMissing = "Klasser mangler"
    show ConfigBuiltMissing = "Built mangler1"
    show GradeConfigFileMissing = "Klasser mangler"
    show IdConfigFileMissing = "Id mangler"
    show ConfigIdMissing = "Id mangler"


instance Exception ShakeError
