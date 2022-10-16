&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER cBatchtype   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cEkstOrdreNr AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iButikknr    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cOrderstatus AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dtEdato      AS DATETIME    NO-UNDO.
DEFINE INPUT  PARAMETER lcBlob       AS LONGCHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER iReturn      AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN NYdigiorderbatch.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-NYdigiorderbatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NYdigiorderbatch Procedure 
PROCEDURE NYdigiorderbatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR iBatchnr AS INTEGER NO-UNDO.
    FIND LAST digiorderbatch USE-INDEX batchnr NO-LOCK NO-ERROR.
    iBatchNr = IF AVAIL digiorderbatch THEN digiorderbatch.batchnr + 1 ELSE 1.
    
    CREATE digiorderbatch.
    ASSIGN digiorderbatch.batchnr     = iBatchNr
           digiorderbatch.EkstOrdreNr = cEkstOrdreNr
           digiorderbatch.Batchtype   = cBatchtype
           digiorderbatch.Butikknr    = iButikkNr
           digiorderbatch.dtEdato     = dtEdato
           digiorderbatch.Orderstatus = cOrderstatus.
/*            digiorderbatch.OppdStatus */
           COPY-LOB FROM lcBlob TO digiorderbatch.blobdata.
    ASSIGN digiorderbatch.OppdStatus = 1.
    FIND CURRENT digiorderbatch NO-LOCK.
    RELEASE digiorderbatch.
iReturn = 200.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

