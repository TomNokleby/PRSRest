
 /*------------------------------------------------------------------------
    File        : Vare
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Mon Jul 30 11:42:31 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="Kode").
@openapi.openedge.entity.required (fields="Kode").
	
DEFINE TEMP-TABLE ttVare SERIALIZE-NAME 'Vare' BEFORE-TABLE bttVare
FIELD id            AS CHARACTER
FIELD seq           AS INTEGER      INITIAL ?
FIELD Kode AS CHARACTER LABEL "Strekkode"
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD Beskr AS CHARACTER 
FIELD LevKod AS CHARACTER 
FIELD LevFargKod AS CHARACTER 
FIELD StrKode AS INTEGER INITIAL "0" 
FIELD Storl AS CHARACTER 
FIELD Vg AS INTEGER FORMAT ">>>>>>9" 
FIELD VgBeskr AS CHARACTER 
FIELD Hg AS INTEGER FORMAT ">>>>>>9"
FIELD HgBeskr AS CHARACTER 
FIELD AvdNr AS INTEGER FORMAT ">>>>>>9"
FIELD AvdBeskr AS CHARACTER 
FIELD VmId AS INTEGER FORMAT ">>>>>9"
FIELD VmBeskr AS CHARACTER 
FIELD Farg AS INTEGER FORMAT ">>>>>>9"
FIELD FarBeskr AS CHARACTER
FIELD Sasong AS INTEGER FORMAT ">>>>>>9"
FIELD SasBeskr AS CHARACTER  
FIELD LevNr AS INTEGER FORMAT ">>>>>>9"
FIELD LevBeskr AS CHARACTER 
FIELD EDato AS DATE FORMAT "99/99/9999"
FIELD Pris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD InnkjopsPris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD Varekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD ProfilNr AS INTEGER FORMAT ">>>>>>9"
FIELD Mva% AS DECIMAL FORMAT "->>9.99"
FIELD TilbPris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD TilbFraDatoTid AS DATETIME
FIELD TilbTilDatoTid AS DATETIME
FIELD NOS AS CHARACTER 
INDEX seq IS PRIMARY UNIQUE seq
INDEX idxStrekKode IS  UNIQUE  Kode  ASCENDING 
INDEX idxArtikkel  ArtikkelNr  ASCENDING  Kode  ASCENDING 
INDEX idxStrKode  StrKode  ASCENDING 
. 

DEFINE TEMP-TABLE ttVarePris SERIALIZE-NAME 'VarePris' BEFORE-TABLE bttVarePris
FIELD seq AS INTEGER INITIAL ?
FIELD Kode AS CHARACTER LABEL "Strekkode"
FIELD ProfilNr AS INTEGER FORMAT ">>>>>>9"
FIELD ButikkNr AS INTEGER FORMAT ">>>>>9"
FIELD EDato AS DATE FORMAT "99/99/9999"
FIELD Pris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD InnkjopsPris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD Varekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD Mva% AS DECIMAL FORMAT "->>9.99"
FIELD TilbPris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD TilbFraDatoTid AS DATETIME
FIELD TilbTilDatoTid AS DATETIME
INDEX seq IS PRIMARY UNIQUE seq ProfilNr ButikkNr
. 
