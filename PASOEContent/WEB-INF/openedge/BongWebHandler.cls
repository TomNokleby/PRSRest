 
/*------------------------------------------------------------------------
   File        : BongWebHandler
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : tomn
   Created     : Mon Aug 01 13:10:19 CEST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Web.WebResponseWriter.
USING OpenEdge.Net.HTTP.StatusCodeEnum.
USING OpenEdge.Web.WebHandler.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS BongWebHandler INHERITS WebHandler: 

  {bo\bong.i}

  DEFINE VARIABLE cLogg AS CHARACTER INITIAL 'BongWebHandler' NO-UNDO.

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
                             'Hello tomn'
                           + '~r~n':u   /*CRLF */
                           + 'This message was returned by HandleGet in BongWebHandler.'
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
    DEFINE VARIABLE oResponse    AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO.
    DEFINE VARIABLE oWriter      AS OpenEdge.Web.WebResponseWriter  NO-UNDO.
    DEFINE VARIABLE oData        AS JsonObject                      NO-UNDO.
    DEFINE VARIABLE oStatus      AS JsonObject                      NO-UNDO.
      
    DEFINE VARIABLE pcStatus     AS CHARACTER                       NO-UNDO.
    DEFINE VARIABLE pcTekst      AS CHARACTER                       NO-UNDO.
    DEFINE VARIABLE pcJsonString AS LONGCHAR                        NO-UNDO.
    DEFINE VARIABLE piStatusCode AS INTEGER                         NO-UNDO.
  
    MESSAGE 'POST HandlePost for BongWebHAndler'.
      
    EMPTY TEMP-TABLE ttBongHode.
    EMPTY TEMP-TABLE ttBongLinje.
      
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
    ASSIGN 
      cLogg = 'BongWebHandler' + STRING(TODAY,'99999999') + '_' + REPLACE(STRING(TIME,'HH:MM:SS'),':','')
      .
    MESSAGE 'Behandle POST i BongWebHandler'.  
      
    initParam(  ).
      
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'POST: ' + 'Behandle POST i BongWebHandler.'
      ).
    
    IF poRequest:ContentType = 'application/json' THEN 
    DO:
      oData = CAST(poRequest:Entity, JsonObject).
      CAST(oData, JSONObject):Write(pcJsonString).
      
      MESSAGE STRING(pcJsonString).
      OUTPUT TO VALUE('BongMottatt_' + STRING(TODAY,"99999999") + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '_' + STRING(ETIME) + '_' + '.json').
      PUT UNFORMATTED STRING(pcJsonString) SKIP. 
      OUTPUT CLOSE.
      
      DATASET dsBongHode:READ-JSON ("longchar", pcJsonString,"EMPTY") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        DO:
          pcTekst      = 'Feil syntaks i JSon melding. Kan ikke mappes til datasett.'.
          piStatusCode = 400.
          pcStatus     = 'FEIL'.
          MESSAGE pcstatus STRING(piStatusCode) pcTekst.
        END.
      FIND FIRST ttBongHode NO-ERROR.
      IF NOT AVAILABLE ttBongHode THEN 
        DO:
          pcTekst      = 'Ugyldig eller tom datasett i melding.'.          
          piStatusCode = 400.
          pcStatus     = 'FEIL'.
          MESSAGE pcstatus STRING(piStatusCode) pcTekst.
        END.
      ELSE DO:
        MESSAGE pcstatus STRING(piStatusCode) pcTekst.
        RUN bo\lagrebong.p(INPUT DATASET dsBongHode,
                           OUTPUT pcStatus,
                           OUTPUT pcTekst,
                           OUTPUT piStatusCode).
      END.
    END.
    ELSE DO:
      pcTekst      = 'Feil ContentType: ' + poRequest:ContentType + '. Skal være: application/json'.
      piStatusCode = 400.
      pcStatus     = 'FEIL'.
      MESSAGE pcstatus STRING(piStatusCode) pcTekst.
    END.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'POST: ' + ' StatusCode: ' + STRING(piStatusCode) + ' Status: ' + pcStatus + ' ' + pcTekst   
      ).
    
    oStatus = NEW JSonObject().
        
    CREATE ttRespons.
    ASSIGN 
      /*ttRespons.cStatus = IF AVAILABLE ttBongHode THEN 'OK' ELSE 'FEIL'*/
      ttRespons.cStatus = pcStatus
      ttRespons.cTekst  = pcTekst
      ttRespons.cB_Id   = IF AVAILABLE ttBongHode THEN STRING(ttBongHode.B_Id) ELSE '' 
      .
    oStatus:ADD("Status", ttRespons.cStatus).
    oStatus:ADD("Tekst", ttRespons.cTekst).
    oStatus:ADD("cB_id", ttRespons.cB_Id).

    ASSIGN 
      oResponse             = NEW OpenEdge.Web.WebResponse()
      /*oResponse:StatusCode  = INTEGER(StatusCodeEnum:OK)*/
      oResponse:StatusCode  = piStatusCode
      oResponse:Entity      = oStatus
      oResponse:ContentType = 'application/json':u
      .
      
    /* The WebResponseWriter ensures that the status line and
       all headers are writted out before the message body/entity. */
    oWriter = NEW WebResponseWriter(oResponse).
    oWriter:Open().
    oWriter:Close().

    /* A response of 0 means that this handler will build the entire response;
       a non-zero value is mapped to a static handler in the webapp's /static/error folder.
       The mappings are maintained in the webapps's WEB-INF/web.xml 
       A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum 
       enumeration */
    MESSAGE 'Ferdig behandle bong.'.
    RETURN 0.
    
    
    
  /*      UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").*/
  END METHOD.
    
  /*------------------------------------------------------------------------------
          Purpose:                                                                
          Notes:                                                                        
  ------------------------------------------------------------------------------*/
  METHOD OVERRIDE PROTECTED INTEGER HandleDelete( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").      
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID initParam(  ):

    ASSIGN 
      cLogg = cLogg + STRING(TODAY,'99999999')
      .
    
    RETURN.

  END METHOD.    
  
END CLASS.