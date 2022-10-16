 
/*------------------------------------------------------------------------
   File        : KundeOrdreHandler
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : tomn
   Created     : Sat Sep 24 08:56:09 CEST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Web.WebResponseWriter.
USING OpenEdge.Net.HTTP.StatusCodeEnum.
USING OpenEdge.Web.WebHandler.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS KundeOrdreHandler INHERITS WebHandler: 

  DEFINE VARIABLE cLogg AS CHARACTER INITIAL 'KundeOrdreWebHandler' NO-UNDO.

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

  DEFINE TEMP-TABLE ttRespons NO-UNDO 
    FIELD cStatus AS CHARACTER SERIALIZE-NAME 'Status'
    FIELD cTekst  AS CHARACTER SERIALIZE-NAME 'Tekst'
    FIELD cB_Id   AS CHARACTER SERIALIZE-NAME 'B_id'  
    .

  METHOD OVERRIDE PROTECTED INTEGER HandleNotAllowedMethod( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

  END METHOD.

  METHOD OVERRIDE PROTECTED INTEGER HandleNotImplemented( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

  END METHOD.

  
  
  /*------------------------------------------------------------------------------
            Purpose: Default handler for the HTTP GET method. The request being 
                     serviced and an optional status code is returned. A zero or 
                     null value means this method will deal with all errors.                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
  METHOD OVERRIDE PROTECTED INTEGER HandleGet( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
   
  
    DEFINE VARIABLE oResponse AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO.
    DEFINE VARIABLE oWriter   AS OpenEdge.Web.WebResponseWriter  NO-UNDO.
    DEFINE VARIABLE oBody     AS OpenEdge.Core.String            NO-UNDO.
            
    /* The WebResponse body is a wrapper around an entire HTTP response message.
       It contains a status code and reason; headers; cookies and a message body.
           
       API-level doc for this and related classes can be found at
       https://documentation.progress.com/output/oehttpclient/      */
    ASSIGN 
      oResponse            = NEW OpenEdge.Web.WebResponse()
      oResponse:StatusCode = INTEGER(StatusCodeEnum:OK)
      .
    /* This body object can be a string or something else (JsonObject for instance) */               
    ASSIGN 
      oBody = NEW OpenEdge.Core.String(
                             'Hello World - i am not born yet!'
                           + '~r~n':u   /*CRLF */
                           + 'This message was returned by HandleGet in KundeOrdreHandler.'
                           ).
        
    ASSIGN 
      oResponse:Entity        = oBody
      /* HTTP messages require a content type */
      oResponse:ContentType   = 'text/plain':u
      /* ContentLength is good too */
      oResponse:ContentLength = oBody:Size
      .
        
    /* The WebResponseWriter ensures that the status line and
       all headers are writted out before the message body/entity. */
    ASSIGN 
      oWriter = NEW WebResponseWriter(oResponse).
    oWriter:Open().
        
    /* Finish writing the response message */
    oWriter:Close().
        
    /* A response of 0 means that this handler will build the entire response;
       a non-zero value is mapped to a static handler in the webapp's /static/error folder.
       The mappings are maintained in the webapps's WEB-INF/web.xml 
       A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum 
       enumeration */
    RETURN 0.
    
  END METHOD. 
   
   
  /*------------------------------------------------------------------------------
          Purpose:                                                                
          Notes:                                                                        
  ------------------------------------------------------------------------------*/
  METHOD OVERRIDE PROTECTED INTEGER HandlePut( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").    
  END METHOD.
    
     
  /*------------------------------------------------------------------------------
           Purpose:                                                                
           Notes:                                                                        
   ------------------------------------------------------------------------------*/
  METHOD OVERRIDE PROTECTED INTEGER HandlePost( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    DEFINE VARIABLE oResponse AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO.
    DEFINE VARIABLE oWriter   AS OpenEdge.Web.WebResponseWriter  NO-UNDO.
    DEFINE VARIABLE oBody     AS OpenEdge.Core.String            NO-UNDO.

    DEFINE VARIABLE oData        AS JsonObject                      NO-UNDO.
    DEFINE VARIABLE oArray       AS JsonArray                       NO-UNDO.
    DEFINE VARIABLE oStatus      AS JsonObject                      NO-UNDO.
    
    DEFINE VARIABLE pcStatus     AS CHARACTER                       NO-UNDO.
    DEFINE VARIABLE pcTekst      AS CHARACTER                       NO-UNDO.
    DEFINE VARIABLE pcLogg       AS CHARACTER                       NO-UNDO.
    DEFINE VARIABLE pcJsonString AS LONGCHAR                        NO-UNDO.
    DEFINE VARIABLE piStatusCode AS INTEGER                         NO-UNDO.
    DEFINE VARIABLE piArrayLoop  AS INTEGER                         NO-UNDO.
    DEFINE VARIABLE piOrderId    AS INTEGER                         NO-UNDO.
    DEFINE VARIABLE piStoreNum   AS INTEGER                         NO-UNDO.
    DEFINE VARIABLE pcOrderStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piTotAntOrder AS INTEGER NO-UNDO.
            
    MESSAGE 'POST HandleKundeOrdre POST'.  
      
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
    FIX-CODEPAGE(pcJsonString) = 'UTF-8'.
    ASSIGN 
      pcLogg = cLogg + STRING(TODAY,'99999999') + STRING(TIME,'HH:MM:SS')
      .
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'POST: ' + 'Behandler mottatt kundeordre.'
      ).
            
    IF poRequest:ContentType = 'application/json' THEN
    APPBLOKK: 
    DO:
      oArray = CAST(poRequest:Entity, JsonArray).
      CAST(oArray, JSONArray):Write(pcJsonString).
      MESSAGE 'ArrayLengde: ' + STRING(oArray:LENGTH)
      VIEW-AS ALERT-BOX.
      ORDRELOOP:
      DO piArrayLoop = 1 TO oArray:LENGTH:
        oData = oArray:GetJsonObject(piArrayLoop).
        
        /* Henter ut nødvendig info om ordren. */
        piOrderId     = oData:GETINTEGER ("orderId").
        piStoreNum    = oData:GETINTEGER ("storeNum").
        pcOrderStatus = oData:GetCharacter("status").

        CAST(oData, JsonObject):Writefile('C:\home\lindbak\ankommet\kundeordre\' + STRING(piStoreNum) + '_' + STRING(piOrderId) + '_' + pcOrderStatus + '_' + STRING(TODAY,"99999999") + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json', TRUE).
        CAST(oData, JSONObject):Write(pcJsonString).


        MESSAGE 'Loop: ' + STRING(piArrayLoop) + ' Odre mottatt: ' + STRING(piStoreNum) + '_' + STRING(piOrderId) + '_' + pcOrderStatus
        VIEW-AS ALERT-BOX.
        
/*        DELETE OBJECT oData NO-ERROR.*/
      END. /* ORDRELOOP */
      
      /* Prepper status til utpakkningsrutinen. */
      ASSIGN 
        piStatusCode = 200
        pcStatus     = 'OK'
        pcTekst      = 'Antall mottatte ordre ' + STRING(oArray:LENGTH) + '.'
        .

        CAST(oArray, JsonArray):Writefile('C:\home\lindbak\ankommet\kundeordre\' + 'Array' + STRING(TODAY,"99999999") + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json', TRUE).

      /* Lagrer ordren i databasen. */  
      RUN bo\DigitrollOrders2prs.p (oArray, poRequest:ContentType, piStatusCode, OUTPUT piTotAntOrder) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN            
      DO:
        ASSIGN 
          piStatusCode = 500
          pcStatus     = 'FEIL'
          pcTekst      = 'Feil ved oppdatering av mottatte ordre.'
          .
      END. 
      ELSE DO:
        ASSIGN 
          piStatusCode = 200
          pcStatus     = 'OK'
          pcTekst      = 'Antall mottatte og oppdaterte ordre ' + STRING(piTotAntOrder) + '.'
          .
      END.           
    END. /* APPBLOKK */
    ELSE DO:
      pcTekst      = 'Feil ContentType: ' + poRequest:ContentType + '. Skal være: application/json'.
      piStatusCode = 400.
      pcStatus     = 'FEIL'.
    END.
    MESSAGE '  ' + pcStatus + ' ' + STRING(piStatusCode) + ' ' + pcTekst   
    VIEW-AS ALERT-BOX.
            
    oStatus = NEW JSonObject().
    
    CREATE ttRespons.
    ASSIGN 
      ttRespons.cStatus = pcStatus
      ttRespons.cTekst  = pcTekst
      .
    oStatus:ADD("Status", ttRespons.cStatus).
    oStatus:ADD("Tekst", ttRespons.cTekst).

    ASSIGN 
      oResponse               = NEW OpenEdge.Web.WebResponse()
      /*oResponse:StatusCode  = INTEGER(StatusCodeEnum:OK)*/
      oResponse:StatusCode    = piStatusCode
      oResponse:Entity        = oStatus
      oResponse:ContentType   = 'application/json':u
      .
    /* The WebResponseWriter ensures that the status line and
       all headers are writted out before the message body/entity. */
    oWriter = NEW WebResponseWriter(oResponse).
    oWriter:Open().
    oWriter:Close().
        
    /* The WebResponseWriter ensures that the status line and
       all headers are writted out before the message body/entity. */
    ASSIGN 
      oWriter = NEW WebResponseWriter(oResponse).
    oWriter:Open().
        
    /* Finish writing the response message */
    oWriter:Close().
        
    /* A response of 0 means that this handler will build the entire response;
       a non-zero value is mapped to a static handler in the webapp's /static/error folder.
       The mappings are maintained in the webapps's WEB-INF/web.xml 
       A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum 
       enumeration */
    RETURN 0.
    
  /*        UNDO, THROW NEW Progress.Lang.AppError("Feil ved mottak av kundeordre.").*/
    
    
    
  END METHOD.
    
  /*------------------------------------------------------------------------------
          Purpose:                                                                
          Notes:                                                                        
  ------------------------------------------------------------------------------*/
  METHOD OVERRIDE PROTECTED INTEGER HandleDelete( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").      
  END METHOD.
    
  
END CLASS.