
 /*------------------------------------------------------------------------
    File        : TelleHode
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Thu Aug 09 10:37:07 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="TelleNr").
	@openapi.openedge.entity.required (fields="Beskrivelse,TelleNr,TTId").
	
DEFINE TEMP-TABLE ttTelleHode BEFORE-TABLE bttTelleHode
FIELD TelleNr AS INTEGER INITIAL "0" LABEL "Telling"
FIELD EDato AS DATE LABEL "Endret"
FIELD ETid AS INTEGER INITIAL "0" LABEL "Endret tid"
FIELD BrukerID AS CHARACTER LABEL "Bruker"
FIELD RegistrertDato AS DATE LABEL "Registrert dato"
FIELD RegistrertTid AS INTEGER INITIAL "0" LABEL "Registreringstidspunkt"
FIELD RegistrertAv AS CHARACTER LABEL "Registrert av"
FIELD Beskrivelse AS CHARACTER LABEL "Beskrivelse"
FIELD TTId AS INTEGER INITIAL "0" LABEL "TransTypeId"
FIELD TBId AS INTEGER INITIAL "0" LABEL "Transaksjonstype beskrivelse"
FIELD Notat AS CHARACTER LABEL "Notat"
FIELD StartDato AS DATE LABEL "StartDato"
FIELD Oppdatert AS DATE LABEL "Oppdatert"
FIELD AntallPar AS DECIMAL INITIAL "0"
FIELD AntallTalt AS DECIMAL INITIAL "0" LABEL "AntallTalt"
FIELD OpptVerdi AS DECIMAL INITIAL "0" LABEL "OpptVerdi"
FIELD VerdiDiff AS DECIMAL INITIAL "0" LABEL "DiffVerdi"
FIELD AntallDiff AS DECIMAL INITIAL "0" LABEL "AntallDiff"
FIELD OpprVerdi AS DECIMAL INITIAL "0" LABEL "OpprVerdi"
FIELD ButikkListe AS CHARACTER LABEL "Butikkliste"
FIELD AntLinjer AS INTEGER INITIAL "0" LABEL "Antall linjer i telling"
FIELD TilButikk AS INTEGER INITIAL "0" LABEL "Til butikk"
FIELD OrdreNr AS DECIMAL INITIAL "0" LABEL "OrdreNr"
FIELD PkSdlNr AS DECIMAL INITIAL "0" LABEL "Pakkseddel"
FIELD BatchNr AS INTEGER INITIAL "0" LABEL "BatchNummer"
FIELD TelleType AS INTEGER INITIAL "1"
FIELD KobletTilTelleNr AS INTEGER INITIAL "0"
FIELD LokasjonsId AS CHARACTER LABEL "Lok.id"
FIELD BrukerIdPDA AS CHARACTER LABEL "Bruker"
FIELD FilDatoPDA AS DATE LABEL "Fildato"
FIELD FilTidPDA AS INTEGER INITIAL "0" LABEL "Klokkeslett"
FIELD VerdiNegDiff AS DECIMAL INITIAL "0" LABEL "Neg.diff verdi"
FIELD VerdiPosDiff AS DECIMAL INITIAL "0" LABEL "Verdi pos diff"
FIELD AntallNegDiff AS DECIMAL INITIAL "0" LABEL "Antall neg diff"
FIELD AntallPosDiff AS DECIMAL INITIAL "0" LABEL "Antall pos diff"
FIELD FilId AS DECIMAL INITIAL "0" LABEL "Filid"
INDEX BatchNr  BatchNr  ASCENDING 
INDEX Beskrivelse  Beskrivelse  ASCENDING  TelleNr  ASCENDING 
INDEX Bruker  BrukerIdPDA  ASCENDING 
INDEX FilId  FilId  ASCENDING 
INDEX KobletTilTelleliste  KobletTilTelleNr  ASCENDING 
INDEX Lokasjonsid  LokasjonsId  ASCENDING 
INDEX Oppdatert  Oppdatert  ASCENDING  TelleNr  ASCENDING 
INDEX Ordre  OrdreNr  ASCENDING 
INDEX Pakkseddel  PkSdlNr  ASCENDING 
INDEX StartDato  StartDato  ASCENDING  TelleNr  ASCENDING 
INDEX TelleHode IS  PRIMARY  UNIQUE  TelleNr  ASCENDING 
INDEX Telletype  TelleType  ASCENDING 
INDEX TransType  TTId  ASCENDING  TBId  ASCENDING  TelleNr  ASCENDING . 


DEFINE DATASET dsTelleHode FOR ttTelleHode.