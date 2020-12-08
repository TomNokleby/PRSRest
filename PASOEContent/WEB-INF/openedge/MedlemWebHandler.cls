 
 /*------------------------------------------------------------------------
    File        : MedlemWebHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Mon Dec 07 11:12:55 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Web.WebResponseWriter.
USING OpenEdge.Net.HTTP.StatusCodeEnum.
USING OpenEdge.Web.WebHandler.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS MedlemWebHandler INHERITS WebHandler FINAL: 
  
  {cls\dintero\ttCustomerObj.i}
  {cls\dintero\dsCustomerObj.i}
  {cls\dintero\ttMedlem.i}
  {cls\dintero\dsMedlem.i}

  DEFINE VARIABLE rCustomerDintero AS cls.Dintero.customerDintero NO-UNDO.
  
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
    DEFINE VARIABLE oJsonArray  AS JsonArray NO-UNDO.
    DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO.
          
    DEFINE VARIABLE cEksterntMedlemsNr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcJson AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE ix AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.      

    rCustomerDintero = NEW cls.Dintero.customerDintero( ) NO-ERROR.
          
    /* The WebResponse body is a wrapper around an entire HTTP response message.
       It contains a status code and reason; headers; cookies and a message body.
       
       API-level doc for this and related classes can be found at
       https://documentation.progress.com/output/oehttpclient/      */
    ASSIGN 
        oResponse            = NEW OpenEdge.Web.WebResponse()
        cEksterntMedlemsNr = poRequest:GetPathParameter( 'Id' ) 
        oResponse:StatusCode = INTEGER(StatusCodeEnum:OK)
        .
        
        
MESSAGE 'TEST cEksterntMedlemsNr:' cEksterntMedlemsNr
VIEW-AS ALERT-BOX.        

    oJsonObject = NEW JSonObject().        
    oJsonArray  = NEW JsonArray().
    
    IF cEksterntMedlemsNr <> '' THEN
      DO:
        FIND FIRST Medlem NO-LOCK WHERE 
          Medlem.EksterntMedlemsNr = cEksterntMedlemsNr NO-ERROR.
        IF AVAILABLE Medlem THEN
        DO:  
          RUN cls\dintero\fyllDatasettMedlem.p (Medlem.MedlemsNr, FALSE, INPUT-OUTPUT DATASET dsMedlem BY-REFERENCE).
          rCustomerDintero:datasetMedlemToJSon( INPUT DATASET dsMedlem, OUTPUT oJsonObject).
          oJsonArray:Add(oJsonObject).
        END.  
      END.
    ELSE DO:
      FOR EACH Medlem NO-LOCK WHERE 
/*        Medlem.EksterntMedlemsNr > '':*/
        Medlem.EksterntMedlemsNr = '13a96f75-c3f0-4fdf-8307-2022db97139b':
          MESSAGE 'test-1'
          VIEW-AS ALERT-BOX.
        RUN cls\dintero\fyllDatasettMedlem.p (Medlem.MedlemsNr, FALSE, INPUT-OUTPUT DATASET dsMedlem BY-REFERENCE).
          FIND FIRST ttMedlem.
          MESSAGE 'test-2' ttMedlem.EksterntMedlemsNr
          VIEW-AS ALERT-BOX.
        rCustomerDintero:datasetMedlemToJSon( INPUT DATASET dsMedlem, OUTPUT oJsonObject) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DO:
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:    
              cTekst = cTekst + 
                     (IF cTekst <> '' THEN CHR(10) ELSE '') + 
                      STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
              MESSAGE 'cTekst:' cTekst
              VIEW-AS ALERT-BOX.
            END.
          END.
          MESSAGE 'test-3'
          VIEW-AS ALERT-BOX.
        oJsonArray:Add(oJsonObject).
          MESSAGE 'test-4'
          VIEW-AS ALERT-BOX.
      END.
    END.
/*    /* Fant ikke medlemmet. */      */
/*    ELSE DO:                        */
/*        oJsonArray:Add(oJsonObject).*/
/*    END.                            */
    
    /* Respons objektet. */
    lcJson = oJsonArray:GetJsonText().
    ASSIGN 
        oBody = NEW OpenEdge.Core.String(lcJSON)
        NO-ERROR.            

    /* This body object can be a string or something else (JsonObject for instance) */               
/*    ASSIGN                                                                            */
/*        oBody = NEW OpenEdge.Core.String(                                             */
/*                         'Hello tomn'                                                 */
/*                       + '~r~n':u   /*CRLF */                                         */
/*                       + 'This message was returned by HandleGet in MedlemWebHandler.'*/
/*                       ).                                                             */


    ASSIGN 
        oResponse:Entity        = oBody
        /* HTTP messages require a content type */
/*        oResponse:ContentType   = 'text/plain':u*/
        oResponse:ContentType   = 'application/json':u
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
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").    
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
     METHOD OVERRIDE PROTECTED INTEGER HandleDelete( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
    
    UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").      
    END METHOD.
    
  
END CLASS.