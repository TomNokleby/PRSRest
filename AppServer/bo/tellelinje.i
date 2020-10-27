
 /*------------------------------------------------------------------------
    File        : Tellelinje
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Thu Aug 09 10:42:43 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="TelleNr,ArtikkelNr,Butik,Storl").
	@openapi.openedge.entity.required (fields="Merknad,TelleNr").
	
DEFINE TEMP-TABLE ttTelleLinje BEFORE-TABLE bttTelleLinje
FIELD TelleNr AS INTEGER INITIAL "0" LABEL "Telling"
FIELD EDato AS DATE LABEL "Endret"
FIELD ETid AS INTEGER INITIAL "0" LABEL "Endret tid"
FIELD BrukerID AS CHARACTER LABEL "Bruker"
FIELD RegistrertDato AS DATE LABEL "Registrert dato"
FIELD RegistrertTid AS INTEGER INITIAL "0" LABEL "Registreringstidspunkt"
FIELD RegistrertAv AS CHARACTER LABEL "Registrert av"
FIELD Merknad AS CHARACTER LABEL "Merknad"
FIELD Oppdatert AS LOGICAL INITIAL "no" LABEL "Oppdatert"
FIELD Vg AS INTEGER INITIAL "0" LABEL "VgNr"
FIELD LopNr AS INTEGER INITIAL "0" LABEL "LpNr"
FIELD LevKod AS CHARACTER LABEL "LevArtNr"
FIELD AntallPar AS DECIMAL INITIAL "0"
FIELD AntallTalt AS DECIMAL INITIAL "0" LABEL "AntallTalt"
FIELD OpprVerdi AS DECIMAL INITIAL "0" LABEL "OpprVerdi"
FIELD OpptVerdi AS DECIMAL INITIAL "0" LABEL "OpptVerdi"
FIELD VerdiDiff AS DECIMAL INITIAL "0" LABEL "DiffVerdi"
FIELD AntallDiff AS DECIMAL INITIAL "0" LABEL "AntallDiff"
FIELD Nedskrevet AS DECIMAL INITIAL "0" LABEL "Nedskrevet"
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD Butik AS INTEGER INITIAL "0" LABEL "Butikknummer"
FIELD Storl AS CHARACTER LABEL "Str"
FIELD LevNr AS INTEGER INITIAL "0" LABEL "Leverandør"
FIELD Farg AS INTEGER INITIAL "0" LABEL "Farge"
FIELD Sasong AS INTEGER INITIAL "0" LABEL "Sesong"
FIELD MatKod AS INTEGER INITIAL "0" LABEL "Materialkode"
FIELD VVareKost AS DECIMAL INITIAL "0" LABEL "VVareKost"
FIELD VgLopNr AS CHARACTER
FIELD RabKr AS DECIMAL INITIAL "0" LABEL "Rabatt"
FIELD Kode AS CHARACTER LABEL "Strekkode"
FIELD Beskr AS CHARACTER LABEL "Beskrivelse"
FIELD SeqNr AS INTEGER INITIAL "0"
FIELD LevFargKod AS CHARACTER LABEL "LevFargKod"
FIELD OpprAntalTalt AS DECIMAL INITIAL "0" LABEL "Oppr.talt"
FIELD LinjeNr AS INTEGER INITIAL "0" LABEL "Linjenr"
INDEX ArtikkelNr  ArtikkelNr  ASCENDING 
INDEX Beskr  TelleNr  ASCENDING  Beskr  ASCENDING 
INDEX Butikk  TelleNr  ASCENDING  Butik  ASCENDING  Vg  ASCENDING  LopNr  ASCENDING  Storl  ASCENDING 
INDEX LevFargKod  TelleNr  ASCENDING  LevFargKod  ASCENDING 
INDEX LevKod  TelleNr  ASCENDING  Butik  ASCENDING  LevKod  ASCENDING  Vg  ASCENDING  LopNr  ASCENDING  Storl  ASCENDING 
INDEX LinjeNr  TelleNr  ASCENDING  LinjeNr  ASCENDING 
INDEX SeqNr  TelleNr  ASCENDING  ArtikkelNr  ASCENDING  Butik  ASCENDING  SeqNr  ASCENDING 
INDEX SortVglopNrStorl  TelleNr  ASCENDING  Butik  ASCENDING  VgLopNr  ASCENDING  Storl  ASCENDING 
INDEX Strekkode  TelleNr  ASCENDING  Butik  ASCENDING  Kode  ASCENDING 
INDEX TelleLinje IS  PRIMARY  UNIQUE  TelleNr  ASCENDING  ArtikkelNr  ASCENDING  Butik  ASCENDING  Storl  ASCENDING 
INDEX VgLopNr  TelleNr  ASCENDING  Vg  ASCENDING  LopNr  ASCENDING  Butik  ASCENDING  Storl  ASCENDING . 


DEFINE DATASET dsTelleLinje FOR ttTelleLinje.