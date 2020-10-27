
 /*------------------------------------------------------------------------
    File        : LokasjonsListe
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Thu Aug 09 09:38:37 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="TelleNr,ArtikkelNr,Butik,Storl").
	@openapi.openedge.entity.required (fields="Merknad,TelleNr").
	
DEFINE TEMP-TABLE ttTelleLinje SERIALIZE-NAME 'ttLokasjonsListe' BEFORE-TABLE bttTelleLinje
FIELD id            AS CHARACTER
FIELD seq           AS INTEGER      INITIAL ?
FIELD TelleNr AS INTEGER INITIAL "0" LABEL "Telling"
FIELD LinjeNr AS INTEGER INITIAL "0" LABEL "Linjenr"
FIELD Butik AS INTEGER INITIAL "0" LABEL "Butikknummer"
FIELD Kode AS CHARACTER LABEL "Strekkode"
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD Storl AS CHARACTER LABEL "Str"
FIELD AntallTalt AS DECIMAL INITIAL "0" LABEL "AntallTalt"
FIELD VVareKost AS DECIMAL INITIAL "0" LABEL "VVareKost"
FIELD Merknad AS CHARACTER LABEL "Merknad"
FIELD Oppdatert AS LOGICAL INITIAL "no" LABEL "Oppdatert"
FIELD Beskr AS CHARACTER LABEL "Beskrivelse"
FIELD SeqNr AS INTEGER INITIAL "0"
FIELD BrukerID AS CHARACTER LABEL "Bruker"
FIELD RegistrertDato AS DATE LABEL "Registrert dato"
FIELD RegistrertTid AS INTEGER INITIAL "0" LABEL "Registreringstidspunkt"
FIELD RegistrertAv AS CHARACTER LABEL "Registrert av"
FIELD LokTelleNr AS INTEGER INITIAL "0" LABEL "Telling"
INDEX seq IS PRIMARY UNIQUE seq
INDEX idxLinjeNr  TelleNr  ASCENDING  LinjeNr  ASCENDING 
INDEX idxTelleLinje TelleNr  ASCENDING  ArtikkelNr  ASCENDING  Butik  ASCENDING  Storl  ASCENDING 
. 

