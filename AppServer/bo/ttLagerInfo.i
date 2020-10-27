
 /*------------------------------------------------------------------------
    File        : Lager
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Wed Aug 01 11:15:23 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="ArtikkelNr,Butik").
	
DEFINE TEMP-TABLE ttLager SERIALIZE-NAME 'ttLager' BEFORE-TABLE bttLager
FIELD id            AS CHARACTER
FIELD seq           AS INTEGER      INITIAL ?
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD Butik AS INTEGER INITIAL "0" LABEL "Butikknummer"
FIELD LagAnt AS DECIMAL INITIAL "0" LABEL "Antall på lager"
FIELD VVarekost AS DECIMAL INITIAL "0" LABEL "Vektet varekost"
FIELD EDato AS DATE LABEL "Endret"
INDEX seq IS PRIMARY UNIQUE seq
INDEX idxButikk  Butik  ASCENDING  ArtikkelNr  ASCENDING 
INDEX idxLager IS  UNIQUE  ArtikkelNr  ASCENDING  Butik  ASCENDING . 
