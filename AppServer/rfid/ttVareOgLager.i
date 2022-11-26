
 /*------------------------------------------------------------------------
    File        : Lager
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Mon Jul 30 11:50:22 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="Kode").
	
@openapi.openedge.entity.field.property (field="Kode", name="semanticType", value="Text").
DEFINE TEMP-TABLE ttVareLager SERIALIZE-NAME 'ttVareLager' BEFORE-TABLE bttVareLager
FIELD id            AS CHARACTER
FIELD seq           AS INTEGER      INITIAL ?
FIELD Kode AS CHARACTER LABEL "Strekkode"
FIELD ButikkNr AS INTEGER FORMAT ">>>>>>>>9"
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD Beskr AS CHARACTER 
FIELD LevKod AS CHARACTER 
FIELD LevFargKod AS CHARACTER  
FIELD VmId AS INTEGER FORMAT ">>>>>9" 
FIELD VmBeskr AS CHARACTER 
FIELD StrKode AS INTEGER INITIAL "0" 
FIELD Storl AS CHARACTER 
FIELD Sasong AS INTEGER FORMAT ">>>>>>9"
FIELD SasBeskr AS CHARACTER  
FIELD LevNr AS INTEGER FORMAT ">>>>>>9"
FIELD LevBeskr AS CHARACTER 
FIELD LagAnt AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD vVarekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD Pris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD InnkjopsPris AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD Varekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD EDato AS DATE LABEL "Endret"
FIELD EndretDatoTid AS DATETIME LABEL "EndretDatoTid"
INDEX seq IS PRIMARY UNIQUE seq
INDEX Artikkel  ArtikkelNr  ASCENDING  Kode  ASCENDING 
INDEX StrekKode IS  UNIQUE  Kode  ASCENDING 
INDEX StrKode  StrKode  ASCENDING 
. 
