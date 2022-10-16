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

USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Lang.Object.
USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Core.WidgetHandle.
/* PROPATH = PROPATH + "," + "C:\Progressx86\OpenEdge\src\netlib\OpenEdge.Net.pl" + "," + */
/*                         "C:\Progressx86\OpenEdge\tty\netlib\OpenEdge.Net.pl".          */

DEFINE INPUT PARAMETER oOrderArray AS JsonArray  NO-UNDO.
DEFINE INPUT PARAMETER cObjectType AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER iStatusCode AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER iTotAntOrder AS INTEGER      NO-UNDO.

DEFINE VARIABLE cType               AS CHARACTER         NO-UNDO.
DEFINE VARIABLE lcCutomerList       AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE oClient             AS IHttpClient       NO-UNDO.
DEFINE VARIABLE oURI                AS URI               NO-UNDO.
DEFINE VARIABLE oCredentials        AS Credentials       NO-UNDO.
DEFINE VARIABLE oRequest            AS IHttpRequest      NO-UNDO.
DEFINE VARIABLE oResponse           AS IHttpResponse     NO-UNDO.
DEFINE VARIABLE iNumCookies         AS INTEGER           NO-UNDO.
DEFINE VARIABLE oCookies            AS Cookie            EXTENT NO-UNDO.
DEFINE VARIABLE oHeaderObject       AS JsonObject        NO-UNDO.
DEFINE VARIABLE oDummyObject        AS JsonObject        NO-UNDO.
DEFINE VARIABLE oEntity             AS Object            NO-UNDO.
DEFINE VARIABLE lcHTML              AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE hXmlDoc             AS HANDLE            NO-UNDO. 
DEFINE VARIABLE iInternnr           AS INTEGER           NO-UNDO.
DEFINE VARIABLE ii                  AS INTEGER           NO-UNDO.
DEFINE VARIABLE iRequest            AS INTEGER           NO-UNDO.
DEFINE VARIABLE lcCurrOrder         AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE lcOrder             AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE oParser             AS ObjectModelParser.
DEFINE VARIABLE oCurrentOrder       AS JsonObject        NO-UNDO.
DEFINE VARIABLE cOrderPath          AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cOrderConfigPath    AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cOrderCancelledPath AS CHARACTER         NO-UNDO.
DEFINE VARIABLE iAntorder           AS INTEGER           NO-UNDO.
DEFINE VARIABLE oGetOrderArray      AS JsonArray         NO-UNDO.
DEFINE VARIABLE cOldDateformat      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cGetOrderTid        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE dtTmp               AS DATETIME          NO-UNDO.
DEFINE VARIABLE dtLastChDt          AS DATETIME          NO-UNDO.
DEFINE VARIABLE iTimeTmp            AS INTEGER           NO-UNDO.
DEFINE VARIABLE iRadnr              AS INTEGER           NO-UNDO.

DEFINE VARIABLE iSKIP               AS INTEGER           NO-UNDO.
DEFINE VARIABLE iFETCH              AS INTEGER           NO-UNDO.

DEFINE VARIABLE cTmpUrl             AS CHARACTER         NO-UNDO.
DEFINE VARIABLE lTmpUrl             AS LOGICAL           NO-UNDO.
DEFINE VARIABLE hSourceProc         AS HANDLE            NO-UNDO.

DEFINE TEMP-TABLE tt_orderHeader NO-UNDO /* SERIALIZE-NAME "Orderhode" */
  FIELD internnr                  AS INTEGER 
  FIELD orderId                   AS CHARACTER
  FIELD storeNum                  AS INTEGER /* DIGIFÄLT */
  FIELD orderType                 AS CHARACTER
  FIELD orderStatus               AS CHARACTER
  FIELD creationDateTime          AS CHARACTER SERIALIZE-NAME "cdt" SERIALIZE-HIDDEN
  FIELD lastModificationDateTime  AS CHARACTER SERIALIZE-NAME "mct" SERIALIZE-HIDDEN
  FIELD opprettetdt               AS DATETIME
  FIELD endretdt                  AS DATETIME
  FIELD customerId                AS CHARACTER
  FIELD note                      AS CHARACTER
  FIELD shipmentServiceLevelCode  AS CHARACTER
  FIELD sh_name                   AS CHARACTER
  FIELD sh_addressLine            AS CHARACTER /* sh = ship */
  FIELD sh_addressLine2           AS CHARACTER /* sh = ship */
  FIELD sh_cityName               AS CHARACTER
  FIELD sh_countrySubDivisionCode AS CHARACTER
  FIELD sh_countryCode            AS CHARACTER
  FIELD sh_postalCode             AS CHARACTER
  FIELD bi_name                   AS CHARACTER /* bi = bill */
  FIELD bi_addressLine            AS CHARACTER
  FIELD bi_addressLine2           AS CHARACTER
  FIELD bi_cityName               AS CHARACTER
  FIELD bi_countrySubDivisionCode AS CHARACTER
  FIELD bi_countryCode            AS CHARACTER
  FIELD bi_postalCode             AS CHARACTER
  FIELD carrierCode               AS CHARACTER
  FIELD carrierName               AS CHARACTER
  FIELD mobile                    AS CHARACTER SERIALIZE-NAME "mobile" /*   SERIALIZE-HIDDEN  /* DIGI */ */
  FIELD email                     AS CHARACTER SERIALIZE-NAME "email"  /*   SERIALIZE-HIDDEN  /* DIGI */ */
  FIELD firstName                 AS CHARACTER SERIALIZE-NAME "firstName" SERIALIZE-HIDDEN  /* DIGI */
  FIELD lastName                  AS CHARACTER SERIALIZE-NAME "lastName" SERIALIZE-HIDDEN /* DIGI */
  FIELD storeName                 AS CHARACTER SERIALIZE-NAME "st#" SERIALIZE-HIDDEN /* DIGI fält */
  FIELD orderAmount               AS DECIMAL   SERIALIZE-NAME "oAm" SERIALIZE-HIDDEN /* DIGI fält */
  FIELD orderTaxAmount            AS DECIMAL   SERIALIZE-NAME "oTam" SERIALIZE-HIDDEN /* DIGI fält */
  FIELD so_name                   AS CHARACTER SERIALIZE-NAME "so_name" SERIALIZE-HIDDEN /* selectedShippingOption name */
  FIELD so_fee                    AS DECIMAL   SERIALIZE-NAME "so_fee" SERIALIZE-HIDDEN /* selectedShippingOption fee */
  FIELD so_taxRate                AS DECIMAL   SERIALIZE-NAME "so_taxRate" SERIALIZE-HIDDEN /* selectedShippingOption taxRate */
  FIELD so_taxAmount              AS DECIMAL   SERIALIZE-NAME "so_taxAmount" SERIALIZE-HIDDEN /* selectedShippingOption taxAmount */
  FIELD shipdoc1                  AS CLOB      SERIALIZE-NAME "shipdoc1" SERIALIZE-HIDDEN /* selectedShippingOption taxRate */
  FIELD shipdoc2                  AS CLOB      SERIALIZE-NAME "shipdoc2" SERIALIZE-HIDDEN /* selectedShippingOption taxAmount */
  FIELD giftWrapping              AS CHARACTER /* Ny */
  INDEX internnr IS PRIMARY UNIQUE internnr.
DEFINE TEMP-TABLE tmp_OrderHeader SERIALIZE-NAME "tt_OrderHeader" LIKE tt_orderHeader.

DEFINE TEMP-TABLE tt_orderLine NO-UNDO /* SERIALIZE-NAME "Orderlinje" */
  FIELD internnr                 AS INTEGER
  FIELD orderID                  AS CHARACTER 
  FIELD lineId                   AS CHARACTER
  FIELD TYPE                     AS CHARACTER
  FIELD transactionType          AS CHARACTER
  FIELD creationDateTime         AS CHARACTER SERIALIZE-NAME "cdt" SERIALIZE-HIDDEN
  FIELD opprettetdt              AS DATETIME
  FIELD note                     AS CHARACTER
  FIELD upcid                    AS CHARACTER /* required */
  FIELD description              AS CHARACTER
  FIELD season                   AS CHARACTER
  FIELD quantity                 AS INTEGER /* req */
  FIELD amount                   AS DECIMAL /* req */
  FIELD currencyCode             AS CHARACTER /* req */
  FIELD totalAmount              AS DECIMAL
  FIELD taxRate                  AS DECIMAL
  FIELD taxSystemCode            AS CHARACTER
  FIELD taxModelCode             AS CHARACTER
  FIELD taxAmount                AS DECIMAL
  FIELD requiredDeliveryDateTime AS CHARACTER
  FIELD discountPercent          AS DECIMAL
  FIELD discountAmount           AS DECIMAL
  FIELD discountDescription      AS CHARACTER 
  INDEX internnr IS PRIMARY internnr orderID.
DEFINE TEMP-TABLE tmp_OrderLine SERIALIZE-NAME "tt_orderLine" LIKE tt_orderLine.

DEFINE TEMP-TABLE tt_payments NO-UNDO /* SERIALIZE-NAME "Orderlinje" */
  FIELD internnr     AS INTEGER
  FIELD orderID      AS CHARACTER 
  FIELD amount       AS DECIMAL
  FIELD currencyCode AS CHARACTER
  FIELD TYPE         AS CHARACTER
  FIELD psp          AS CHARACTER SERIALIZE-NAME "psp" SERIALIZE-HIDDEN
  FIELD fee          AS DECIMAL   SERIALIZE-NAME "fee" SERIALIZE-HIDDEN
  FIELD feeTaxRate   AS DECIMAL   SERIALIZE-NAME "feeTaxRate" SERIALIZE-HIDDEN
  FIELD feeTaxAmount AS DECIMAL   SERIALIZE-NAME "feeTaxAmount" SERIALIZE-HIDDEN
  FIELD referenceId  AS CHARACTER
  INDEX internnr IS PRIMARY internnr orderID.
DEFINE TEMP-TABLE tmp_payments SERIALIZE-NAME "tt_payments" LIKE tt_payments.

DEFINE TEMP-TABLE tt_fulfillments NO-UNDO /* SERIALIZE-NAME "fulfillments" */
  FIELD internnr            AS INTEGER
  FIELD orderID             AS CHARACTER
  /*     FIELD lineId        AS CHAR */
  FIELD upcid               AS CHARACTER
  FIELD fulfillableQuantity AS INTEGER
  FIELD fulfilledQuantity   AS INTEGER
  FIELD cstatus             AS CHARACTER SERIALIZE-NAME "status"
  INDEX internnr IS PRIMARY internnr orderID.
DEFINE TEMP-TABLE tmp_fulfillments SERIALIZE-NAME "tt_fulfillments" LIKE tt_fulfillments.

DEFINE TEMP-TABLE ttRowid
  FIELD rRowid AS ROWID
  FIELD BatchNr AS DECIMAL
  FIELD iStatus AS INTEGER INITIAL 0 
  .

DEFINE VARIABLE OrderDataSet    AS HANDLE NO-UNDO.
DEFINE VARIABLE TmpOrderDataSet AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDT Procedure 
FUNCTION getDT RETURNS DATETIME
  ( INPUT cDT AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 24.43
         WIDTH              = 69.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

hSourceProc = SOURCE-PROCEDURE.
cOldDateformat = SESSION:DATE-FORMAT.

SESSION:DATE-FORMAT = "ymd".
FIND FIRST lastGetOrder NO-LOCK NO-ERROR.
IF NOT AVAILABLE lastGetOrder THEN 
DO:
  CREATE lastGetOrder.
  FIND CURRENT lastGetOrder NO-LOCK.
END.

dtTmp = NOW - (TIMEZONE * 60 * 1000).
dtTmp = dtTmp - 60000.
/* dtTmp = dtTmp - 15000. */

cType = "NEW".

CREATE DATASET OrderDataSet.
ASSIGN
  /*     StockDataSet:SERIALIZE-HIDDEN = TRUE */
OrderDataSet:SERIALIZE-NAME = "Order".
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_orderHeader:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_orderLine:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_payments:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_fulfillments:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-RELATION(BUFFER tt_orderHeader:HANDLE, BUFFER tt_orderLine:HANDLE,"internnr,internnr").
OrderDataSet:ADD-RELATION(BUFFER tt_orderHeader:HANDLE, BUFFER tt_payments:HANDLE,"internnr,internnr").
OrderDataSet:ADD-RELATION(BUFFER tt_orderHeader:HANDLE, BUFFER tt_fulfillments:HANDLE,"internnr,internnr").

CREATE DATASET tmpOrderDataSet.
ASSIGN
  /*     StockDataSet:SERIALIZE-HIDDEN = TRUE */
tmpOrderDataSet:SERIALIZE-NAME = "Order".
tmpOrderDataSet:ADD-BUFFER(TEMP-TABLE tmp_orderHeader:DEFAULT-BUFFER-HANDLE).
tmpOrderDataSet:ADD-BUFFER(TEMP-TABLE tmp_orderLine:DEFAULT-BUFFER-HANDLE).
tmpOrderDataSet:ADD-BUFFER(TEMP-TABLE tmp_payments:DEFAULT-BUFFER-HANDLE).
tmpOrderDataSet:ADD-BUFFER(TEMP-TABLE tmp_fulfillments:DEFAULT-BUFFER-HANDLE).
tmpOrderDataSet:ADD-RELATION(BUFFER tmp_orderHeader:HANDLE, BUFFER tmp_orderLine:HANDLE,"internnr,internnr").
tmpOrderDataSet:ADD-RELATION(BUFFER tmp_orderHeader:HANDLE, BUFFER tmp_payments:HANDLE,"internnr,internnr").
tmpOrderDataSet:ADD-RELATION(BUFFER tmp_orderHeader:HANDLE, BUFFER tmp_fulfillments:HANDLE,"internnr,internnr").


/* /integration/orders?skip=SKIP&fetch=FETCH&status=open&changedSince=SENASTETID */


/* Vi använder den här tillsvidare */
/* /integration/orders?skip=SKIPPA&fetch=FETCHA&changedSince=SENASTETID */

iSKIP  = 0.
iFETCH = 1.
IF cType = "NEW" THEN 
DO:
  cGetOrderTid = lastGetOrder.lasttime .
END.
ELSE IF cType = "CANCELLED" THEN 
  DO:
    cGetOrderTid = lastGetOrder.canceltime.
  END.
  ELSE
    RETURN.

ORDERHANDLINGBLOKK:
DO:
  IF cObjectType = "NODATA" THEN
    LEAVE ORDERHANDLINGBLOKK.
  IF iStatusCode <> 200 THEN 
  DO:
    LEAVE ORDERHANDLINGBLOKK.
  END.
  
  oParser = NEW ObjectModelParser().
  DO  iAntorder = 1 TO oOrderArray:LENGTH:
    oCurrentOrder = (oOrderArray:GetJsonObject(iAntorder)).
    oCurrentOrder:WRITE(lcCurrOrder,TRUE).

    RUN BehandlaOrder (oCurrentOrder,lcCurrOrder).
    RUN SkapaTT.
  END.
  iSKIP = iSKIP + iFETCH.
END. /* ORDERHANDLINGBLOKK */

/* skapa blob till BO */
IF CAN-FIND(FIRST tt_orderHeader) THEN 
DO:
  RUN fixaBlobbar.

  EMPTY TEMP-TABLE tt_orderHeader.
  EMPTY TEMP-TABLE tt_orderLine.
  EMPTY TEMP-TABLE tt_payments.
  EMPTY TEMP-TABLE tmp_orderHeader.
  EMPTY TEMP-TABLE tmp_orderLine.
  EMPTY TEMP-TABLE tmp_payments.

  DELETE OBJECT OrderDataSet NO-ERROR.
  DELETE OBJECT tmpOrderDataSet NO-ERROR.
        
  IF lTmpUrl = FALSE AND iStatusCode = 200 THEN 
  DO:
    IF cType = "NEW" THEN 
    DO:
      /* vi hämtar först cancelled och sen new. */
      FIND FIRST lastGetOrder.
      IF dtLastChDt > DATETIME(lastGetOrder.lasttime) THEN
        lastGetOrder.lasttime = REPLACE(REPLACE(STRING(dtLastChDt + 1)," ","T"),",",".").
      ELSE
        lastGetOrder.lasttime = cGetOrderTid.
      FIND CURRENT lastGetOrder NO-LOCK.
    END.
    ELSE IF cType = "CANCELLED" THEN 
      DO:
        /* vi hämtar först cancelled och sen new.  */
        FIND FIRST lastGetOrder.
        lastGetOrder.canceltime = cGetOrderTid.
        FIND CURRENT lastGetOrder NO-LOCK.
      END.
  END.
END.

DELETE OBJECT oParser NO-ERROR.
SESSION:DATE-FORMAT = cOldDateformat.

MESSAGE 'Finner ttRowId' CAN-FIND(FIRST ttRowId)
VIEW-AS ALERT-BOX.
IF CAN-FIND(FIRST ttRowId) THEN
DO:
  MESSAGE 'starter sendOrder2prs'
  VIEW-AS ALERT-BOX. 
  
  RUN sendOrder2prs.
END.
IF CAN-FIND(FIRST ttRowId WHERE ttRowId.iStatus = 1) THEN
DO: 
  RUN oppdaterOrderPrs.
END.

EMPTY TEMP-TABLE ttRowId.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AssignaAdress) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignaAdress Procedure 
PROCEDURE AssignaAdress :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oAdress AS JsonObject   NO-UNDO.
  DEFINE VARIABLE lcAdressinfo AS LONGCHAR   NO-UNDO.
  DEFINE VARIABLE oAdressinfo  AS JsonObject NO-UNDO.
  DEFINE INPUT  PARAMETER cType   AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE c_Name                   AS CHARACTER.
  DEFINE VARIABLE c_addressLine            AS CHARACTER. /* sh = ship */
  DEFINE VARIABLE c_addressLine2           AS CHARACTER. /* sh = ship */
  DEFINE VARIABLE c_cityName               AS CHARACTER.
  DEFINE VARIABLE c_countrySubDivisionCode AS CHARACTER.
  DEFINE VARIABLE c_countryCode            AS CHARACTER.
  DEFINE VARIABLE c_postalCode             AS CHARACTER.
  DEFINE VARIABLE cNames                   AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE cNamesAdr                AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii                       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i2                       AS INTEGER   NO-UNDO.
  cNames = oAdress:getNames().
    
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "name" THEN
        c_Name = oAdress:GetJsonText(cNames[ii]).
      WHEN "address" THEN 
        DO:
          /*                 MESSAGE "address"                      */
          /*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
          lcAdressInfo = oAdress:GetJsonText(cNames[ii]).
          /* OUTPUT TO ".\json\adress.json" APPEND.         */
          /*     PUT UNFORMATTED STRING(lcAdressInfo) SKIP. */
          /* OUTPUT CLOSE.                                  */

          oAdressInfo = CAST(oParser:Parse(lcAdressInfo),JsonObject).
          cNamesAdr = oAdressInfo:getNames().
          DO i2 = 1 TO EXTENT(cNamesAdr):
            CASE cNamesAdr[i2]:
              WHEN "addressLine"            THEN 
                c_addressLine            = oAdressinfo:GetCharacter(cNamesAdr[i2]).
              WHEN "addressLine2"           THEN 
                c_addressLine2           = oAdressinfo:GetCharacter(cNamesAdr[i2]).
              WHEN "cityName"               THEN 
                c_cityName               = oAdressinfo:GetCharacter(cNamesAdr[i2]).
              WHEN "countrySubDivisionCode" THEN 
                c_countrySubDivisionCode = oAdressinfo:GetCharacter(cNamesAdr[i2]).
              WHEN "countryCode"            THEN 
                c_countryCode            = oAdressinfo:GetCharacter(cNamesAdr[i2]).
              WHEN "postalCode"             THEN 
                c_postalCode             = oAdressinfo:GetCharacter(cNamesAdr[i2]).
            END CASE.
          END.
        END.
    END CASE.
  END.
  IF cType = "sh" THEN 
  DO:
    ASSIGN
      tmp_OrderHeader.sh_name                   = c_Name
      tmp_OrderHeader.sh_addressLine            = c_addressLine
      tmp_OrderHeader.sh_addressLine2           = c_addressLine2
      tmp_OrderHeader.sh_cityName               = c_cityName
      tmp_OrderHeader.sh_countrySubDivisionCode = c_countrySubDivisionCode
      tmp_OrderHeader.sh_countryCode            = c_countryCode
      tmp_OrderHeader.sh_postalCode             = c_postalCode.
  END.
  ELSE 
  DO:
    ASSIGN 
      tmp_OrderHeader.bi_name                   = c_Name
      tmp_OrderHeader.bi_addressLine            = c_addressLine
      tmp_OrderHeader.bi_cityName               = c_cityName
      tmp_OrderHeader.bi_countrySubDivisionCode = c_countrySubDivisionCode
      tmp_OrderHeader.bi_countryCode            = c_countryCode
      tmp_OrderHeader.bi_postalCode             = c_postalCode.
  END.
  DELETE OBJECT oAdress NO-ERROR.
  DELETE OBJECT oAdressinfo NO-ERROR.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignaCarrier) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignaCarrier Procedure 
PROCEDURE AssignaCarrier :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oCarrier AS JsonObject   NO-UNDO.

  DEFINE VARIABLE cNames AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
  cNames = oCarrier:getNames().
    
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "carrierCode" THEN 
        tmp_OrderHeader.carrierCode = oCarrier:GetCharacter(cNames[ii]).
      WHEN "name"        THEN 
        tmp_OrderHeader.carrierName = oCarrier:GetCharacter(cNames[ii]).
    END CASE.
  END.
  DELETE OBJECT oCarrier NO-ERROR.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignaDiscount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignaDiscount Procedure 
PROCEDURE AssignaDiscount :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oObject AS JsonObject   NO-UNDO.
  DEFINE VARIABLE cNames AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
  cNames = oObject:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "discountAmount" THEN 
        tmp_OrderLine.discountAmount = oObject:GetDecimal(cNames[ii]).
      WHEN "description" THEN 
        tmp_OrderLine.discountDescription = oObject:GetCharacter(cNames[ii]).
    END CASE.
  END.
  DELETE OBJECT oObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignaItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignaItem Procedure 
PROCEDURE AssignaItem :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oObject AS JsonObject   NO-UNDO.
  DEFINE VARIABLE cNames       AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii           AS INTEGER   NO-UNDO.
  cNames = oObject:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "upcid"       THEN 
        tmp_OrderLine.upcid       = oObject:GetCharacter(cNames[ii]).
      WHEN "description" THEN 
        DO:
          cDescription              = oObject:GetCharacter(cNames[ii]) NO-ERROR.
          tmp_OrderLine.description = cDescription.
        END.
    /*             WHEN "season"      THEN tmp_OrderLine.season       = oObject:GetCharacter(cNames[ii]). */
    END CASE.
  END.

  DELETE OBJECT oObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-assignaPayment) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignaPayment Procedure 
PROCEDURE assignaPayment :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oObject AS JsonObject   NO-UNDO.
  DEFINE VARIABLE cNames AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
  cNames = oObject:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "amount"       THEN 
        tmp_Payments.amount       = oObject:GetDecimal(cNames[ii]).
      WHEN "currencyCode" THEN 
        tmp_Payments.currencyCode = oObject:GetCharacter(cNames[ii]).
      WHEN "type"         THEN 
        tmp_Payments.type         = oObject:GetCharacter(cNames[ii]).
      WHEN "referenceId"  THEN 
        tmp_Payments.referenceId  = oObject:GetCharacter(cNames[ii]).
    END CASE.
  END.
    
  DELETE OBJECT oObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignaTax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignaTax Procedure 
PROCEDURE AssignaTax :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oObject AS JsonObject   NO-UNDO.
  DEFINE VARIABLE cNames AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
  cNames = oObject:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "taxRate"       THEN 
        tmp_OrderLine.taxRate       = oObject:GetDecimal(cNames[ii]).
      WHEN "taxSystemCode" THEN 
        tmp_OrderLine.taxSystemCode = oObject:GetCharacter(cNames[ii]).
      WHEN "taxModelCode"  THEN 
        tmp_OrderLine.taxModelCode  = oObject:GetCharacter(cNames[ii]).
      WHEN "taxAmount"     THEN 
        tmp_OrderLine.taxAmount     = oObject:GetDecimal(cNames[ii]).
    END CASE.
  END.
    
  DELETE OBJECT oObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignaUnitPrice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignaUnitPrice Procedure 
PROCEDURE AssignaUnitPrice :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/

  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oObject AS JsonObject   NO-UNDO.
  DEFINE VARIABLE cNames AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
  cNames = oObject:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "amount"       THEN 
        tmp_OrderLine.amount       = oObject:GetDecimal(cNames[ii]).
      WHEN "currencyCode" THEN 
        tmp_OrderLine.currencyCode = oObject:GetCharacter(cNames[ii]).
    END CASE.
  END.
  DELETE OBJECT oObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Assigna_Wrapping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Assigna_Wrapping Procedure 
PROCEDURE Assigna_Wrapping :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oWrapping AS JsonObject   NO-UNDO.

  DEFINE VARIABLE cNames     AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE lWrap      AS LOG       NO-UNDO.
  DEFINE VARIABLE oWrapArray AS JsonArray NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i2         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cTxt       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lcc        AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.

  FIX-CODEPAGE(lcc) = "UTF-8".

  cNames = oWrapping:getNames().
    
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "giftWrap"     THEN 
        DO:
          lWrap = oWrapping:GetLogical(cNames[ii]).
          IF NOT lWrap THEN
            LEAVE.
        END.
      WHEN "messageLines" THEN 
        DO:
          oWrapArray = oWrapping:GetJsonArray(cNames[ii]).
          DO i2 = 1 TO oWrapArray:LENGTH:
            ctxt = oWrapArray:GetJsonText(i2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
              COPY-LOB FROM lcc TO FILE ".\json\wrp.txt".
              OUTPUT TO .\json\wrp.txt APPEND.
              PUT UNFORMATTED CHR(10).
              OUTPUT CLOSE.
              INPUT FROM ".\json\wrp.txt" CONVERT TARGET "iso8859-1" SOURCE "utf-8".
              IMPORT UNFORMATTED cTxt.
              INPUT CLOSE.
              cTxt = REPLACE(ctxt,"~?", "").
            END.
            ctxt = TRIM(cTxt).
            IF cTxt <> "" THEN
              cMessage = cMessage + (IF cMessage = "" THEN "" ELSE "|") + cTxt.
          END.
        END.
    END CASE.
  END.
  IF lWrap = TRUE AND cMessage = "" THEN
    cMessage = "INGEN_TEKST".
  IF cMessage <> "" THEN
    tmp_OrderHeader.giftWrapping = cMessage.
  DELETE OBJECT oWrapping NO-ERROR.
  DELETE OBJECT oWrapArray NO-ERROR.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BehandlaOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandlaOrder Procedure 
PROCEDURE BehandlaOrder :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER oOrder1 AS JsonObject NO-UNDO.
  DEFINE INPUT PARAMETER loHeaderChar AS LONGCHAR NO-UNDO.
  DEFINE VARIABLE cPropNames      AS CHARACTER  EXTENT NO-UNDO.
  DEFINE VARIABLE oDigiObject     AS JsonObject NO-UNDO.
  DEFINE VARIABLE oHeader         AS JsonObject NO-UNDO.
  DEFINE VARIABLE aDigiArray      AS JsonArray  NO-UNDO.
  DEFINE VARIABLE iAntObj         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE oOrder          AS JsonObject NO-UNDO.
  /*     DEFINE VARIABLE oLines    AS JsonObject NO-UNDO. */
  DEFINE VARIABLE iAntLines       AS INTEGER    NO-UNDO.
    
  DEFINE VARIABLE cOrderId        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStoreNum       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStoreName      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStatus         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCurrency       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dOrderAmount    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dOrderTaxAmount AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE lGiftWrapping   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cGiftWrapping   AS CHARACTER  NO-UNDO.

        
  oOrder = CAST(oParser:Parse(loHeaderChar), jsonObject).
  /*     RETURN.                                */
  cPropNames = oOrder:getNames().
  /* "Här har vi 2 namn, men testar oavsett om object/array */
  DO iAntObj = 1 TO EXTENT(cPropNames):
    CASE cPropNames[iAntObj]:
      WHEN "orderId" THEN 
        DO:
          cOrderId = STRING(oOrder:GetInteger(cPropNames[iAntObj])).
          iInternnr = iInternnr + 1.
          iTotAntOrder = iInternnr.
          CREATE tmp_OrderHeader.
          ASSIGN 
            tmp_OrderHeader.internnr = iInternnr
            tmp_OrderHeader.orderId  = cOrderId.
        END.
      WHEN "storeNum" THEN 
        DO:
          tmp_OrderHeader.storeNum = oOrder:GetInteger(cPropNames[iAntObj]).
        END.
      WHEN "storeName" THEN 
        DO:
          tmp_OrderHeader.storeName = oOrder:GetCharacter(cPropNames[iAntObj]).
        END.
      WHEN "reference" THEN 
        DO:
          tmp_OrderHeader.note = oOrder:GetCharacter(cPropNames[iAntObj]).
        END.
      WHEN "status" THEN 
        DO:
          cStatus = oOrder:GetCharacter(cPropNames[iAntObj]).
          tmp_OrderHeader.orderStatus = cStatus.
        END.
      WHEN "customer" THEN 
        DO: /* object */
          loHeaderChar = oOrder:GetJsonText(cPropNames[iAntObj]).
          oDigiObject = CAST(oParser:Parse(loHeaderChar),JsonObject).
          RUN saxa_DIGI ("customer",oDigiObject).
        END.
      WHEN "shippingAddress" THEN 
        DO: /* object */
          loHeaderChar = oOrder:GetJsonText(cPropNames[iAntObj]).
          oDigiObject = CAST(oParser:Parse(loHeaderChar),JsonObject).
          RUN saxa_DIGI ("shippingAddress",oDigiObject).
        END.
      WHEN "billingAddress" THEN 
        DO: /* object */
          loHeaderChar = oOrder:GetJsonText(cPropNames[iAntObj]).
          oDigiObject = CAST(oParser:Parse(loHeaderChar),JsonObject).
          RUN saxa_DIGI ("billingAddress",oDigiObject).
        END.
      WHEN "selectedShippingOption" THEN 
        DO: /* object */
          loHeaderChar = oOrder:GetJsonText(cPropNames[iAntObj]).
          oDigiObject = CAST(oParser:Parse(loHeaderChar),JsonObject).
          RUN saxa_DIGI ("selectedShippingOption",oDigiObject).
        END.
      WHEN "payments" THEN 
        DO: /* array */
          IF AVAILABLE tmp_OrderHeader THEN 
          DO: /* den måste finnas */
            aDigiArray = oOrder:GetJsonArray(cPropNames[iAntObj]).
            DO iAntLines = 1 TO aDigiArray:LENGTH:
              oDigiObject = (aDigiArray:GetJsonObject(iAntLines)).
              CREATE tmp_Payments.
              ASSIGN 
                tmp_Payments.internnr = tmp_OrderHeader.internnr
                tmp_Payments.orderID  = tmp_OrderHeader.orderID.
              RUN saxa_DIGI ("payments",oDigiObject).
            END.
          END.
        END.
            
      WHEN "orderlines" THEN 
        DO: /* array */
          iRadnr = 0.
          IF AVAILABLE tmp_OrderHeader THEN 
          DO: /* den måste finnas */
            aDigiArray = oOrder:GetJsonArray(cPropNames[iAntObj]).
            DO iAntLines = 1 TO aDigiArray:LENGTH:
              oDigiObject = (aDigiArray:GetJsonObject(iAntLines)).
              /*                         CREATE tmp_OrderLine. */
              /*                         ASSIGN tmp_OrderLine.internnr = tmp_OrderHeader.internnr */
              /*                                tmp_OrderLine.orderID = tmp_OrderHeader.orderID. */
              RUN saxa_DIGI ("orderLines",oDigiObject).
            END.
          END.
          /* Här skapar vi extra orderlines baserat på shipping och avgifter på betalning */
          RUN skapa_DIGI_extra.
                
        END.
      WHEN "fulfillments" THEN 
        DO: /* array */
          IF AVAILABLE tmp_OrderHeader AND tmp_OrderHeader.orderStatus = "shipped" THEN 
          DO: /* den måste finnas */
            aDigiArray = oOrder:GetJsonArray(cPropNames[iAntObj]).
            DO iAntLines = 1 TO aDigiArray:LENGTH:
              oDigiObject = (aDigiArray:GetJsonObject(iAntLines)).
              RUN saxa_DIGI ("fulfillments",oDigiObject).
              LEAVE.
            END.
          END.
        END.
      WHEN "currency" THEN 
        DO:
          cCurrency = oOrder:GetCharacter(cPropNames[iAntObj]).
          FOR EACH tmp_Payments WHERE tmp_Payments.internnr = tmp_OrderHeader.internnr AND
            tmp_Payments.orderID  = tmp_OrderHeader.orderID:
            tmp_Payments.currencyCode = cCurrency.
          END.
          FOR EACH tmp_OrderLine WHERE tmp_OrderLine.internnr = tmp_OrderHeader.internnr AND
            tmp_OrderLine.orderID  = tmp_OrderHeader.orderID:
            tmp_OrderLine.currencyCode = cCurrency.
          END.
        END.
      WHEN "orderAmount" THEN 
        DO:
          tmp_OrderHeader.orderAmount = oOrder:GetDecimal(cPropNames[iAntObj]) / 100.
        END.
      WHEN "orderTaxAmount" THEN 
        DO:
          tmp_OrderHeader.orderTaxAmount = oOrder:GetDecimal(cPropNames[iAntObj]) / 100.
        END.
      WHEN "giftWrapping" THEN 
        DO:
          lgiftWrapping = oOrder:GetLogical(cPropNames[iAntObj]).
          IF lgiftWrapping THEN
            tmp_OrderHeader.giftWrapping = "INGEN_TEKST".
        END.
      WHEN "giftWrappingText" THEN 
        DO:
          IF lgiftWrapping = TRUE THEN 
          DO:
            cGiftWrapping = oOrder:GetCharacter(cPropNames[iAntObj]) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
              cGiftWrapping = "INGEN_TEKST".
            tmp_OrderHeader.giftWrapping = cGiftWrapping NO-ERROR.
          END.
        END.
      WHEN "created" THEN 
        DO:
          tmp_OrderHeader.opprettetdt = oOrder:GetDateTime(cPropNames[iAntObj]).
        END.
      WHEN "updated" THEN 
        DO:
          tmp_OrderHeader.endretdt = oOrder:GetDateTime(cPropNames[iAntObj]).
        END.
            
    END CASE.
  END.
  DELETE OBJECT oOrder NO-ERROR.
  DELETE OBJECT oHeader NO-ERROR.
  DELETE OBJECT aDigiArray NO-ERROR.
  DELETE OBJECT oDigiObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixaBlobbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixaBlobbar Procedure 
PROCEDURE fixaBlobbar :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  /*     OrderDataSet:WRITE-JSON("longchar",lcOrder,TRUE).         */
  /*     OrderDataSet:WRITE-JSON("file",".\json\order.json",TRUE). */
  /*     RUN LagraBlob("ORDER",lcOrder).                           */

  FOR EACH tt_OrderHeader:
    CREATE tmp_OrderHeader.
    BUFFER-COPY tt_OrderHeader TO tmp_OrderHeader.
    FOR EACH tt_OrderLine WHERE tt_Orderline.orderID = tt_Orderheader.orderID:
      CREATE tmp_OrderLine.
      BUFFER-COPY tt_OrderLine TO tmp_OrderLine.
    /*                 DELETE tmp_OrderLine. */
    END.
    FOR EACH tt_Payments WHERE tt_Payments.orderID = tt_Orderheader.orderID:
      CREATE tmp_Payments.
      BUFFER-COPY tt_Payments TO tmp_Payments.
    /*                 DELETE tmp_Payments. */
    END.
    FOR EACH tt_fulfillments WHERE tt_fulfillments.orderID = tt_Orderheader.orderID:
      CREATE tmp_fulfillments.
      BUFFER-COPY tt_fulfillments TO tmp_fulfillments.
    /*                 DELETE tmp_Payments. */
    END.
    /*             DELETE tmp_OrderHeader. */
    tmpOrderDataSet:WRITE-JSON("longchar",lcOrder,TRUE).
    RUN LagraBlob("ORDER",lcOrder).
    EMPTY TEMP-TABLE tmp_orderHeader.
    EMPTY TEMP-TABLE tmp_orderLine.
    EMPTY TEMP-TABLE tmp_payments.
    EMPTY TEMP-TABLE tmp_fulfillments.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagraBlob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagraBlob Procedure 
PROCEDURE LagraBlob :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cBatchType AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER lcData     AS LONGCHAR    NO-UNDO.
  DEFINE VARIABLE dBatchnr AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE dtNow    AS DATETIME NO-UNDO.
  dtNow = NOW.      
  FIND LAST sendtoprstrans NO-LOCK NO-ERROR.
  DO TRANSACTION:
    dBatchnr = IF NOT AVAILABLE sendtoprstrans THEN 1 ELSE sendtoprstrans.batchnr + 1.

    CREATE sendtoprstrans.
    ASSIGN 
      sendtoprstrans.batchnr        = dBatchNr
      sendtoprstrans.batchtype      = cBatchType
      sendtoprstrans.received       = dtNow
      sendtoprstrans.transferstatus = 0 /* inte klar för överföring */
      sendtoprstrans.Butikknr       = tt_OrderHeader.storeNum
      sendtoprstrans.Endretdato     = tt_OrderHeader.endretdt
      sendtoprstrans.Orderstatus    = tt_OrderHeader.orderStatus
      sendtoprstrans.orderId        = tt_OrderHeader.orderId. 
    IF dtLastChDt = ? THEN
      dtLastChDt = tt_OrderHeader.endretdt.
    ELSE IF tt_OrderHeader.endretdt > dtLastChDt THEN
        dtLastChDt = tt_OrderHeader.endretdt.
    CREATE sendtoprsdata.
    ASSIGN 
      sendtoprsdata.batchnr = dBatchNr.
    COPY-LOB FROM lcData TO sendtoprsdata.blobdata NO-CONVERT.

    IF tt_OrderHeader.orderStatus = "partiallyreturned" OR tt_OrderHeader.orderStatus = "onhold" OR 
    /* tt_OrderHeader.orderStatus = "cancelled" OR */ (tt_OrderHeader.orderStatus = "returned" AND tt_OrderHeader.orderAmount > 0) THEN
      sendtoprstrans.transferstatus = 222.
    ELSE DO:
      sendtoprstrans.transferstatus = 1.
      CREATE ttRowid.
      ASSIGN 
        ttRowId.rRowId      = ROWID(sendtoprstrans)
        ttRowId.BatchNr     = dBatchNr
        .
    END.    

    IF tt_OrderHeader.orderStatus = "shipped" THEN 
    DO:
      dBatchnr = dBatchnr + 1.
      CREATE sendtoprstrans.
      ASSIGN 
        sendtoprstrans.batchnr        = dBatchNr
        sendtoprstrans.batchtype      = "SHIPDOC1"
        sendtoprstrans.received       = dtNow
        sendtoprstrans.transferstatus = 0 /* inte klar för överföring */
        sendtoprstrans.Butikknr       = tt_OrderHeader.storeNum
        sendtoprstrans.Endretdato     = tt_OrderHeader.endretdt
        sendtoprstrans.Orderstatus    = tt_OrderHeader.orderStatus
        sendtoprstrans.orderId        = tt_OrderHeader.orderId. 
                   
      CREATE sendtoprsdata.
      ASSIGN 
        sendtoprsdata.batchnr = dBatchNr.
      COPY-LOB FROM tt_OrderHeader.shipdoc1 TO lcData.
      COPY-LOB FROM lcData TO sendtoprsdata.blobdata NO-CONVERT.
      sendtoprstrans.transferstatus = 1.    
      CREATE ttRowid.
      ASSIGN 
        ttRowId.rRowId      = ROWID(sendtoprstrans)
        ttRowId.BatchNr     = dBatchNr
        .
        
      dBatchnr = dBatchnr + 1.
      CREATE sendtoprstrans.
      ASSIGN 
        sendtoprstrans.batchnr        = dBatchNr
        sendtoprstrans.batchtype      = "SHIPDOC2"
        sendtoprstrans.received       = dtNow
        sendtoprstrans.transferstatus = 0 /* inte klar för överföring */
        sendtoprstrans.Butikknr       = tt_OrderHeader.storeNum
        sendtoprstrans.Endretdato     = tt_OrderHeader.endretdt
        sendtoprstrans.Orderstatus    = tt_OrderHeader.orderStatus
        sendtoprstrans.orderId        = tt_OrderHeader.orderId. 
                   
      CREATE sendtoprsdata.
      ASSIGN 
        sendtoprsdata.batchnr = dBatchNr.
      COPY-LOB FROM tt_OrderHeader.shipdoc2 TO lcData.
      COPY-LOB FROM lcData TO sendtoprsdata.blobdata NO-CONVERT.
      sendtoprstrans.transferstatus = 1.    
      CREATE ttRowid.
      ASSIGN 
        ttRowId.rRowId      = ROWID(sendtoprstrans)
        ttRowId.BatchNr     = dBatchNr
        .
    END.
    RELEASE sendtoprstrans.
    RELEASE sendtoprsdata.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdaterOrderPrs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterOrderPrs Procedure
PROCEDURE oppdaterOrderPrs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

MESSAGE '  subrutine oppdaterOrderPrs'
VIEW-AS ALERT-BOX.

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-saxa_DIGI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saxa_DIGI Procedure 
PROCEDURE saxa_DIGI :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER oDIGIobject AS JsonObject   NO-UNDO.

  DEFINE VARIABLE oAdress                AS JsonObject NO-UNDO.
  DEFINE VARIABLE lcAdress               AS LONGCHAR   NO-UNDO.
  DEFINE VARIABLE oPayArray              AS JsonArray  NO-UNDO.
  DEFINE VARIABLE oPayment               AS JsonObject NO-UNDO.
  DEFINE VARIABLE ofulfillObject         AS JsonObject NO-UNDO.
  DEFINE VARIABLE cNames                 AS CHARACTER  EXTENT NO-UNDO.
  DEFINE VARIABLE ii                     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntPayments           AS INTEGER    NO-UNDO.
    
  DEFINE VARIABLE olctype                AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE olcname                AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE longcname              AS LONGCHAR   NO-UNDO.

  DEFINE VARIABLE cc                     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE olcean                 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE oliquantity            AS INTEGER    NO-UNDO.
  DEFINE VARIABLE oldunitPrice           AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE oldtaxRate             AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE oldtotalAmount         AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE oldtotalDiscountAmount AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE oldtotalTaxAmount      AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iInt                   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntLines              AS INTEGER    NO-UNDO.
  DEFINE VARIABLE aDigiArray             AS JsonArray  NO-UNDO.

  cNames = oDIGIobject:getNames().
  CASE cType:
    WHEN "customer" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "mobile"     THEN 
              DO: 
                tmp_OrderHeader.mobile    = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "email"     THEN 
              DO: 
                tmp_OrderHeader.email     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "firstName"     THEN 
              DO: 
                tmp_OrderHeader.firstName = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "lastName"     THEN 
              DO: 
                tmp_OrderHeader.lastName  = oDIGIobject:GetCharacter(cNames[ii]).
              END.
          END CASE.
        END.
      END.
    WHEN "shippingAddress" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "name"     THEN 
              DO: 
                tmp_OrderHeader.sh_name     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "street"     THEN 
              DO: 
                tmp_OrderHeader.sh_addressLine     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "postalCode"     THEN 
              DO: 
                tmp_OrderHeader.sh_postalCode     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "city"     THEN 
              DO: 
                tmp_OrderHeader.sh_cityName     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
          END CASE.
        END.
      END.
    WHEN "billingAddress" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "name"     THEN 
              DO: 
                tmp_OrderHeader.bi_name     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "street"     THEN 
              DO: 
                tmp_OrderHeader.bi_addressLine     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "postalCode"     THEN 
              DO: 
                tmp_OrderHeader.bi_postalCode     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "city"     THEN 
              DO: 
                tmp_OrderHeader.bi_cityName     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
          END CASE.
        END.
      END.
    WHEN "selectedShippingOption" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "name"     THEN 
              DO: 
                tmp_OrderHeader.so_name     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "shippingMethod"     THEN 
              DO: 
                tmp_OrderHeader.shipmentServiceLevelCode     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "carrier"     THEN 
              DO: 
                tmp_OrderHeader.carrierCode     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "fee"     THEN 
              DO: 
                tmp_OrderHeader.so_fee     = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
            WHEN "taxRate"     THEN 
              DO: 
                tmp_OrderHeader.so_taxRate     = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
            WHEN "taxAmount"     THEN 
              DO: 
                tmp_OrderHeader.so_taxAmount     = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
          END CASE.
        END.
      END.
    WHEN "payments" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "psp"     THEN 
              DO: 
                tmp_Payments.psp     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "type"     THEN 
              DO: 
                tmp_Payments.type     = oDIGIobject:GetCharacter(cNames[ii]).
              END.
            WHEN "amount"     THEN 
              DO: 
                tmp_Payments.amount     = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
            WHEN "fee"     THEN 
              DO: 
                tmp_Payments.fee      = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
            WHEN "feeTaxRate"     THEN 
              DO: 
                tmp_Payments.feeTaxRate      = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
            WHEN "feeTaxAmount"     THEN 
              DO: 
                tmp_Payments.feeTaxAmount      = oDIGIobject:GetDecimal(cNames[ii]) / 100.
              END.
          END CASE.
        END.
      END.
    WHEN "fulfillments" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "lineItems" THEN 
              DO:
                aDigiArray = oDIGIobject:GetJsonArray(cNames[ii]).
                DO iAntLines = 1 TO aDigiArray:LENGTH:
                  ofulfillObject = (aDigiArray:GetJsonObject(iAntLines)).
                  CREATE tmp_fulfillments.
                  ASSIGN 
                    tmp_fulfillments.internnr = tmp_OrderHeader.internnr
                    tmp_fulfillments.orderID  = tmp_OrderHeader.orderID.
                  RUN saxa_DIGIlineItems (ofulfillObject).
                END.
              END.
            WHEN "labelPng"        THEN 
              DO: 
                tmp_OrderHeader.shipdoc1 = oDIGIobject:GetJsonText(cNames[ii]).
              END.
            WHEN "returnLabelPNG"  THEN 
              DO:
                tmp_OrderHeader.shipdoc2 = oDIGIobject:GetJsonText(cNames[ii]).
              END.

          END CASE.
        END.
      END.
    WHEN "orderLines" THEN 
      DO:
        DO ii = 1 TO EXTENT(cNames):
          CASE cNames[ii]:
            WHEN "type"        THEN 
              DO: 
                olctype = oDIGIobject:GetCharacter(cNames[ii]).     
              END.
            WHEN "name"        THEN 
              DO: 
                olcname = oDIGIobject:GetCharacter(cNames[ii]) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:
                  olcname = "".
                  FIX-CODEPAGE(longcname) = "UTF-8".
                  longcname = oDIGIobject:GetLongchar(cNames[ii],"UTF-8").
                /*                                                         do ii = 1 to length(longcname). */
                /*                                                             cc = substr(longcname,ii,1) no-error. */
                /*                                                             if not error-status:error then do: */
                /*                                                                 iInt = asc(cc) no-error. */
                /*                                                                 if iInt < 256 then */
                /*                                                                     olcname = olcname + cc. */
                /*                                                             end. */
                /*                                                         end. */
                END.
              END.
            WHEN "ean"         THEN 
              DO: 
                olcean                         = oDIGIobject:GetCharacter(cNames[ii]).     
              END.
            WHEN "quantity"    THEN 
              DO: 
                oliquantity                    = oDIGIobject:GetInteger(cNames[ii]).      
              END.
            WHEN "unitPrice"   THEN 
              DO: 
                oldunitPrice                   = oDIGIobject:GetDecimal(cNames[ii]) / 100. 
              END.
            WHEN "taxRate"     THEN 
              DO: 
                oldtaxRate                     = oDIGIobject:GetDecimal(cNames[ii]) / 100. 
              END.
            WHEN "totalAmount" THEN 
              DO: 
                oldtotalAmount                 = oDIGIobject:GetDecimal(cNames[ii]) / 100. 
              END.
            WHEN "totalDiscountAmount" THEN 
              DO: 
                oldtotalDiscountAmount = oDIGIobject:GetDecimal(cNames[ii]) / 100. 
              END.
            WHEN "totalTaxAmount" THEN 
              DO: 
                oldtotalTaxAmount           = oDIGIobject:GetDecimal(cNames[ii]) / 100. 
              END.
          END CASE.
        END.
        IF olctype <> "return" THEN 
        DO ii = 1 TO oliquantity:
          iRadnr = iRadnr + 1.
          CREATE tmp_OrderLine.
          ASSIGN 
            tmp_OrderLine.internnr        = tmp_OrderHeader.internnr
            tmp_OrderLine.orderID         = tmp_OrderHeader.orderID
            tmp_OrderLine.lineId          = STRING(iRadnr)
            tmp_OrderLine.type            = "product"
            tmp_OrderLine.transactionType = "sales"
            tmp_OrderLine.upcid           = olcean                         
            tmp_OrderLine.quantity        = 1                         
            tmp_OrderLine.description     = olcname
            tmp_OrderLine.amount          = oldunitPrice                          
            tmp_OrderLine.taxRate         = oldtaxRate
            tmp_OrderLine.totalAmount     = oldtotalAmount / oliquantity
            tmp_OrderLine.taxAmount       = oldtotalTaxAmount / oliquantity
            tmp_OrderLine.discountAmount  = oldtotalDiscountAmount / oliquantity.
          IF tmp_OrderLine.discountAmount > 0 THEN
            tmp_OrderLine.discountPercent = ROUND(tmp_OrderLine.discountAmount / (tmp_OrderLine.totalAmount + tmp_OrderLine.discountAmount) * 100,1) NO-ERROR.
        END.
        ELSE 
        DO ii = 1 TO (oliquantity * -1):
          iRadnr = iRadnr + 1.
          CREATE tmp_OrderLine.
          ASSIGN 
            tmp_OrderLine.internnr        = tmp_OrderHeader.internnr
            tmp_OrderLine.orderID         = tmp_OrderHeader.orderID
            tmp_OrderLine.lineId          = STRING(iRadnr)
            tmp_OrderLine.type            = "product"
            tmp_OrderLine.transactionType = "sales"
            tmp_OrderLine.upcid           = olcean                         
            tmp_OrderLine.quantity        = -1                         
            tmp_OrderLine.description     = olcname
            tmp_OrderLine.amount          = oldunitPrice                          
            tmp_OrderLine.taxRate         = oldtaxRate
            tmp_OrderLine.totalAmount     = oldtotalAmount / (oliquantity * -1)
            tmp_OrderLine.taxAmount       = oldtotalTaxAmount / (oliquantity * -1)
            tmp_OrderLine.discountAmount  = oldtotalDiscountAmount / (oliquantity * -1).
          /*                            if tmp_OrderLine.discountAmount <> 0 then                                                                                                             */
          /*                                tmp_OrderLine.discountPercent = round(tmp_OrderLine.discountAmount / (tmp_OrderLine.totalAmount + tmp_OrderLine.discountAmount) * 100,1) no-error */
          .
        END.
      END.
  END CASE.
  DELETE OBJECT aDigiArray NO-ERROR.
  DELETE OBJECT ofulfillObject NO-ERROR.
  DELETE OBJECT oDIGIobject NO-ERROR.
  DELETE OBJECT oAdress NO-ERROR.
  DELETE OBJECT oPayArray NO-ERROR.
  DELETE OBJECT oPayment NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saxa_DIGIlineItems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saxa_DIGIlineItems Procedure 
PROCEDURE saxa_DIGIlineItems :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oDIGIobject AS JsonObject   NO-UNDO.
  DEFINE VARIABLE cNames AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
  cNames = oDIGIobject:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "ean"     THEN 
        DO: 
          tmp_fulfillments.upcid    = oDIGIobject:GetCharacter(cNames[ii]).
        END.
      WHEN "fulfillableQuantity"     THEN 
        DO: 
          tmp_fulfillments.fulfillableQuantity     = oDIGIobject:GetInteger(cNames[ii]).
        END.
      WHEN "fulfilledQuantity"     THEN 
        DO: 
          tmp_fulfillments.fulfilledQuantity = oDIGIobject:GetInteger(cNames[ii]).
        END.
      WHEN "status"     THEN 
        DO: 
          tmp_fulfillments.cstatus  = oDIGIobject:GetCharacter(cNames[ii]).
        END.
    END CASE.
  END.
/*     output to ".\json\full.txt" append. */
/*             put unformatted tmp_OrderHeader.internnr " " tmp_OrderHeader.orderID " " tmp_fulfillments.internnr " " tmp_fulfillments.orderID " " */
/*                  tmp_fulfillments.upcid " " tmp_fulfillments.fulfillableQuantity " " tmp_fulfillments.fulfilledQuantity " " tmp_fulfillments.cstatus skip. */
/*     output close. */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saxa_Header) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saxa_Header Procedure 
PROCEDURE saxa_Header :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oHeader AS JsonObject   NO-UNDO.

  DEFINE VARIABLE oAdress      AS JsonObject NO-UNDO.
  DEFINE VARIABLE lcAdress     AS LONGCHAR   NO-UNDO.
  DEFINE VARIABLE oPayArray    AS JsonArray  NO-UNDO.
  DEFINE VARIABLE oPayment     AS JsonObject NO-UNDO.
  DEFINE VARIABLE cNames       AS CHARACTER  EXTENT NO-UNDO.
  DEFINE VARIABLE ii           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntPayments AS INTEGER    NO-UNDO.
  cNames = oHeader:getNames().

  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "orderId"     THEN 
        DO: 
          tmp_OrderHeader.orderId     = oHeader:GetCharacter(cNames[ii]).
        END.
      WHEN "orderType"   THEN 
        tmp_OrderHeader.orderType   = oHeader:GetCharacter(cNames[ii]).
      WHEN "orderStatus" THEN 
        tmp_OrderHeader.orderStatus = oHeader:GetCharacter(cNames[ii]).
      WHEN "creationDateTime" THEN 
        DO:
          tmp_OrderHeader.creationDateTime = oHeader:GetCharacter(cNames[ii]).
          tmp_OrderHeader.opprettetdt = DYNAMIC-FUNCTION('getDT',tmp_OrderHeader.creationDateTime).
        END.
      WHEN "lastModificationDateTime" THEN 
        DO:
          tmp_OrderHeader.lastModificationDateTime = oHeader:GetCharacter(cNames[ii]).
          tmp_OrderHeader.endretdt = DYNAMIC-FUNCTION('getDT',tmp_OrderHeader.lastModificationDateTime).
        END.
      WHEN "customerId"  THEN 
        DO: 
          tmp_OrderHeader.customerId  = oHeader:GetCharacter(cNames[ii]).
          lcCutomerList = lcCutomerList + (IF lcCutomerList <> "" THEN "," ELSE "") + tmp_OrderHeader.customerId.
        END.
      WHEN "note"        THEN 
        tmp_OrderHeader.note        = oHeader:GetCharacter(cNames[ii]).
      WHEN "shipmentServiceLevelCode" THEN 
        tmp_OrderHeader.shipmentServiceLevelCode = oHeader:GetCharacter(cNames[ii]).
      WHEN "shipToParty" THEN 
        DO:
          lcAdress = oHeader:GetJsonText(cNames[ii]).
          oAdress = CAST(oParser:Parse(lcAdress),JsonObject).
          RUN AssignaAdress (oAdress,"sh").
        END.
      WHEN "billToParty" THEN 
        DO:
          lcAdress = oHeader:GetJsonText(cNames[ii]).
          oAdress = CAST(oParser:Parse(lcAdress),JsonObject).
          RUN AssignaAdress (oAdress,"bi").
        END.
      WHEN "carrierParty" THEN 
        DO:
          lcAdress = oHeader:GetJsonText(cNames[ii]).
          oAdress = CAST(oParser:Parse(lcAdress),JsonObject).
          RUN AssignaCarrier (oAdress).
        END.
      WHEN "giftWrapping" THEN 
        DO:
          lcAdress = oHeader:GetJsonText(cNames[ii]).
          oAdress = CAST(oParser:Parse(lcAdress),JsonObject).
          RUN Assigna_Wrapping (oAdress).
        END.
      WHEN "payments" THEN 
        DO:
          oPayArray = oHeader:GetJsonArray(cNames[ii]).
          DO iAntPayments = 1 TO oPayArray:LENGTH:
            oPayment = (oPayArray:GetJsonObject(iAntPayments)).
            CREATE tmp_Payments.
            ASSIGN 
              tmp_Payments.internnr = tmp_OrderHeader.internnr
              tmp_Payments.orderID  = tmp_OrderHeader.orderID.
            RUN assignaPayment (oPayment).
          END.
        END.
    END CASE.
  END.
  DELETE OBJECT oHeader NO-ERROR.
  DELETE OBJECT oAdress NO-ERROR.
  DELETE OBJECT oPayArray NO-ERROR.
  DELETE OBJECT oPayment NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saxa_Line) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saxa_Line Procedure 
PROCEDURE saxa_Line :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER oLine AS JsonObject   NO-UNDO.

  DEFINE VARIABLE oObject  AS JsonObject NO-UNDO.
  DEFINE VARIABLE loObject AS LONGCHAR   NO-UNDO.
  DEFINE VARIABLE cNames   AS CHARACTER  EXTENT NO-UNDO.
  DEFINE VARIABLE ii       AS INTEGER    NO-UNDO.
       
  cNames = oLine:getNames().
  DO ii = 1 TO EXTENT(cNames):
    CASE cNames[ii]:
      WHEN "lineId"                   THEN 
        tmp_OrderLine.lineId            = oLine:GetCharacter(cNames[ii]).
      WHEN "type"                     THEN 
        tmp_OrderLine.TYPE              = oLine:GetCharacter(cNames[ii]).
      WHEN "transactionType"          THEN 
        tmp_OrderLine.transactionType   = oLine:GetCharacter(cNames[ii]).
      WHEN "creationDateTime"          THEN 
        DO:
          tmp_OrderLine.creationDateTime = oLine:GetCharacter(cNames[ii]).
          tmp_OrderLine.opprettetdt = DYNAMIC-FUNCTION('getDT',tmp_OrderLine.creationDateTime).
        END.
      WHEN "note"                     THEN 
        tmp_OrderLine.note              = oLine:GetCharacter(cNames[ii]).
      WHEN "quantity"                 THEN 
        tmp_OrderLine.quantity          = oLine:GetInteger(cNames[ii]).
      WHEN "totalAmount"              THEN 
        DO:
          tmp_OrderLine.totalAmount       = oLine:GetDecimal(cNames[ii]).
        /*                 IF tmp_OrderLine.totalAmount > 0 THEN                                              */
        /*                        tmp_OrderLine.taxAmount = ROUND(tmp_OrderLine.totalAmount * .2,2) NO-ERROR. */
        END.
      WHEN "requiredDeliveryDateTime" THEN 
        tmp_OrderLine.requiredDeliveryDateTime = oLine:GetCharacter(cNames[ii]). 
      WHEN "item" THEN 
        DO:
          IF oLine:GetType(cNames[ii]) = 4 THEN 
          DO:
            loObject = oLine:GetJsonText(cNames[ii]).
            oObject = CAST(oParser:Parse(loObject),JsonObject).
            RUN AssignaItem (oObject).
          END.
        END.
      WHEN "unitPrice" THEN 
        DO:
          IF oLine:GetType(cNames[ii]) = 4 THEN 
          DO:
            loObject = oLine:GetJsonText(cNames[ii]).
            oObject = CAST(oParser:Parse(loObject),JsonObject).
            RUN AssignaUnitprice (oObject).
          END.
        END.
      WHEN "tax" THEN 
        DO:
          IF oLine:GetType(cNames[ii]) = 4 THEN 
          DO:
            loObject = oLine:GetJsonText(cNames[ii]).
            oObject = CAST(oParser:Parse(loObject),JsonObject).
            RUN AssignaTax (oObject).
          END.
        END.
      WHEN "discount" THEN 
        DO:
          IF oLine:GetType(cNames[ii]) = 4 THEN 
          DO:
            loObject = oLine:GetJsonText(cNames[ii]).
            oObject = CAST(oParser:Parse(loObject),JsonObject).
            RUN AssignaDiscount (oObject).
          END.
        END.
        
    END CASE.
  END.
  DELETE OBJECT oObject NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendOrder2prs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendOrder2prs Procedure
PROCEDURE sendOrder2prs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lcBlobData AS LONGCHAR     NO-UNDO.
  DEFINE VARIABLE lOk AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
  DEF VAR iStatus AS INTE NO-UNDO.
  
  DEFINE BUFFER bufSendtoprstrans FOR Sendtoprstrans.

  FOR EACH ttRowId WHERE 
    ttRowId.iStatus = 0:
    FIND LAST sendtoprstrans NO-LOCK WHERE 
      sendtoprstrans.BatchNr = ttRowid.BatchNr NO-ERROR.
    IF NOT AVAIL sendtoprstrans 
      THEN NEXT.
    FIND FIRST sendtoprsdata OF sendtoprstrans NO-LOCK NO-ERROR.
    IF AVAILABLE sendtoprsdata THEN 
    DO:
      COPY-LOB sendtoprsdata.blobdata TO lcBlobData CONVERT TARGET CODEPAGE "UTF-8".
      
      RUN bo\asPutFromDIGI.p (sendtoprstrans.batchtype,sendtoprstrans.orderId,sendtoprstrans.butikknr,sendtoprstrans.Orderstatus,sendtoprstrans.Endretdato,lcBlobData,OUTPUT iStatus).
  
      DO FOR bufSendtoprstrans TRANSACTION:
        FIND bufSendtoprstrans WHERE ROWID(bufSendtoprstrans) = ROWID(Sendtoprstrans).
        bufSendtoprstrans.transferstatus = iStatus.
        RELEASE bufSendtoprstrans.
      END.
      
      ASSIGN 
        ttRowId.iStatus = 1.
    END.
  END. 

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SkapaTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT Procedure 
PROCEDURE SkapaTT :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  /*     CURRENT-WINDOW:WIDTH = 300. */
  IF CAN-FIND(FIRST tmp_OrderHeader) THEN 
  DO:
    FOR EACH tmp_OrderHeader:
      CREATE tt_OrderHeader.
      BUFFER-COPY tmp_OrderHeader TO tt_OrderHeader.
      FOR EACH tmp_OrderLine WHERE tmp_Orderline.orderID = tmp_Orderheader.orderID:
        CREATE tt_OrderLine.
        BUFFER-COPY tmp_OrderLine TO tt_OrderLine.
        DELETE tmp_OrderLine.
      END.
      FOR EACH tmp_Payments WHERE tmp_Payments.orderID = tmp_Orderheader.orderID:
        CREATE tt_Payments.
        BUFFER-COPY tmp_Payments TO tt_Payments.
        DELETE tmp_Payments.
      END.
      FOR EACH tmp_fulfillments WHERE tmp_fulfillments.orderID = tmp_fulfillments.orderID:
        CREATE tt_fulfillments.
        BUFFER-COPY tmp_fulfillments TO tt_fulfillments.
        DELETE tmp_fulfillments.
      END.
      DELETE tmp_OrderHeader.
    END.
        
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-skapa_DIGI_extra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_DIGI_extra Procedure 
PROCEDURE skapa_DIGI_extra :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  IF tmp_OrderHeader.so_fee > 0 THEN 
  DO:
    iRadnr = iRadnr + 1.
    CREATE tmp_OrderLine.
    ASSIGN 
      tmp_OrderLine.internnr        = tmp_OrderHeader.internnr
      tmp_OrderLine.orderID         = tmp_OrderHeader.orderID
      tmp_OrderLine.lineId          = STRING(iRadnr)
      tmp_OrderLine.type            = "shipping"
      tmp_OrderLine.transactionType = "sales"
      tmp_OrderLine.quantity        = 1                         
      tmp_OrderLine.description     = tmp_OrderHeader.so_name
      tmp_OrderLine.amount          = tmp_OrderHeader.so_fee                          
      tmp_OrderLine.taxRate         = tmp_OrderHeader.so_taxRate
      tmp_OrderLine.taxAmount       = tmp_OrderHeader.so_taxAmount
      tmp_OrderLine.totalAmount     = tmp_OrderLine.amount.
  END.
  FOR EACH tmp_payments WHERE tmp_payments.internnr = tmp_OrderHeader.internnr AND
    tmp_payments.orderID  = tmp_OrderHeader.orderId:
    IF tmp_payments.fee > 0 THEN 
    DO:
      iRadnr = iRadnr + 1.
      CREATE tmp_OrderLine.
      ASSIGN 
        tmp_OrderLine.internnr        = tmp_OrderHeader.internnr
        tmp_OrderLine.orderID         = tmp_OrderHeader.orderID
        tmp_OrderLine.lineId          = STRING(iRadnr)
        tmp_OrderLine.type            = "gebyr"
        tmp_OrderLine.transactionType = "sales"
        tmp_OrderLine.quantity        = 1                         
        tmp_OrderLine.description     = tmp_payments.type
        tmp_OrderLine.amount          = tmp_payments.fee
        tmp_OrderLine.taxRate         = tmp_payments.feeTaxRate
        tmp_OrderLine.taxAmount       = tmp_payments.feeTaxAmount
        tmp_OrderLine.totalAmount     = tmp_OrderLine.amount.
    END.
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDT Procedure 
FUNCTION getDT RETURNS DATETIME
  ( INPUT cDT AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE DTZ     AS DATETIME-TZ NO-UNDO.
  DEFINE VARIABLE DTZHere AS DATETIME    NO-UNDO.

  DTZ = DATETIME-TZ(REPLACE(cDT,"Z","+00:00")) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
    DTZHere = DTZ.
  ELSE 
  DO:
  /*       MESSAGE cDT "ERROR"                    */
  /*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  END.

  RETURN DTZHere.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

