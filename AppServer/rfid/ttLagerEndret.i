
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
DEFINE TEMP-TABLE ttLagerEndret SERIALIZE-NAME 'ttLagerEndret' BEFORE-TABLE bttLagerEndret
FIELD id            AS CHARACTER
FIELD seq           AS INTEGER      INITIAL ?
FIELD Kode AS CHARACTER LABEL "Strekkode"
FIELD ButikkNr AS INTEGER FORMAT ">>>>>>>>9"
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD StrKode AS INTEGER INITIAL "0" 
FIELD Storl AS CHARACTER 
FIELD LagAnt AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD vVarekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
FIELD EndretDato AS DATETIME LABEL "Endret"
INDEX seq IS PRIMARY UNIQUE seq
INDEX idxStrekKode IS  UNIQUE  Kode  ASCENDING 
. 
