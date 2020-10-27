
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
   
DEFINE DATASET dsVareLager SERIALIZE-NAME 'dsVareLager' FOR ttVareLager, ttLager
DATA-RELATION drsVareLager FOR ttVareLager, ttLager NESTED
RELATION-FIELDS (ArtikkelNr,ArtikkelNr)
.

