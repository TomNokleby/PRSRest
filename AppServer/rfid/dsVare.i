
 /*------------------------------------------------------------------------
    File        : Vare
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Mon Jul 30 11:42:31 CEST 2018
    Notes       : Ny test på commit.
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
DEFINE DATASET dsVare SERIALIZE-NAME 'dsVare' FOR ttVare, ttVarePris
  DATA-RELATION drVare FOR ttVare, ttVarePris RELATION-FIELDS (Kode, Kode) NESTED
  .
