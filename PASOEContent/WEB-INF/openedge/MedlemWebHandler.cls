 
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
  
  DEFINE TEMP-TABLE ttRespons NO-UNDO 
    FIELD cStatus AS CHARACTER SERIALIZE-NAME 'Status'
    FIELD cTekst AS CHARACTER SERIALIZE-NAME 'Tekst'
    FIELD cCustomer_Id AS CHARACTER SERIALIZE-NAME 'customer_id'  
    .

  DEFINE VARIABLE cLogg AS CHARACTER INITIAL 'MedlemWebHandler' NO-UNDO.
  DEFINE VARIABLE cLongchar AS LONGCHAR NO-UNDO.
  
  DEFINE VARIABLE rCustomerDintero AS cls.Dintero.customerDintero NO-UNDO.
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  
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
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTekstArray AS CHARACTER EXTENT NO-UNDO.  
        

    rCustomerDintero = NEW cls.Dintero.customerDintero( ) NO-ERROR.
          
    /* The WebResponse body is a wrapper around an entire HTTP response message.
       It contains a status code and reason; headers; cookies and a message body.
       
       API-level doc for this and related classes can be found at
       https://documentation.progress.com/output/oehttpclient/      */
    ASSIGN 
        oResponse            = NEW OpenEdge.Web.WebResponse()
        cEksterntMedlemsNr   = poRequest:URI:GetQueryValue('Id')
        cEksterntMedlemsNr   = IF cEksterntMedlemsNr = ? THEN '' ELSE cEksterntMedlemsNr 
        oResponse:StatusCode = INTEGER(StatusCodeEnum:OK)
        .
        
    MESSAGE '  PARAMETRE:'.   
    MESSAGE '    cEksterntMedlemsNr: ' cEksterntMedlemsNr.   
    MESSAGE '    HasQueryName ' STRING(poRequest:URI:HasQueryName('Id')). 
    MESSAGE '    poRequest:URI:GetQueryNames (output cTtekstArray) '  poRequest:URI:GetQueryNames(OUTPUT cTekstArray).
    DO iLoop = 1 TO EXTENT(cTekstArray):
      MESSAGE '      cTekstArray:' iLoop  cTekstArray[iLoop] poRequest:URI:GetQueryValue( cTekstArray[iLoop] ).
    END. 
    MESSAGE '    poRequest:PathParameterNames:' poRequest:PathParameterNames.
    MESSAGE '    poRequest:UriTemplate' poRequest:UriTemplate.
    MESSAGE '    poRequest:GetPathParameter '  poRequest:GetPathParameter( 'Id' ). 
    MESSAGE '    poRequest:URI:GetQueryValue( Id )' IF poRequest:URI:GetQueryValue('Id') = ? THEN '?' ELSE poRequest:URI:GetQueryValue('Id').
    MESSAGE '    poRequest:URI:BaseURI:' poRequest:URI:BaseURI.
    MESSAGE '    poRequest:URI:Path:' poRequest:URI:Path.
    MESSAGE '    poRequest:URI:QueryString:' poRequest:URI:QueryString.
    MESSAGE '    poRequest:URI:Scheme:' poRequest:URI:Scheme.
    MESSAGE '    poRequest:URI:User:' poRequest:URI:User.
      
    oJsonObject = NEW JSonObject().        
    oJsonArray  = NEW JsonArray().
    
    IF cEksterntMedlemsNr <> '' AND 
      poRequest:URI:HasQueryName('id') THEN
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
/*    ELSE IF NOT poRequest:URI:HasQueryName('id') THEN                                                                */
/*      DO:                                                                                                            */
/*        iLoop = 0.                                                                                                   */
/*        MEDLEMLOOP:                                                                                                  */
/*        FOR EACH Medlem NO-LOCK WHERE                                                                                */
/*          Medlem.EksterntMedlemsNr > '':                                                                             */
/*          iLoop = iLoop + 1.                                                                                         */
/*          RUN cls\dintero\fyllDatasettMedlem.p (Medlem.MedlemsNr, FALSE, INPUT-OUTPUT DATASET dsMedlem BY-REFERENCE).*/
/*          rCustomerDintero:datasetMedlemToJSon( INPUT DATASET dsMedlem, OUTPUT oJsonObject) NO-ERROR.                */
/*          IF ERROR-STATUS:ERROR THEN                                                                                 */
/*            DO:                                                                                                      */
/*              DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                */
/*                cTekst = cTekst +                                                                                    */
/*                       (IF cTekst <> '' THEN CHR(10) ELSE '') +                                                      */
/*                        STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).                    */
/*                MESSAGE 'cTekst:' cTekst                                                                             */
/*                VIEW-AS ALERT-BOX.                                                                                   */
/*              END.                                                                                                   */
/*            END.                                                                                                     */
/*          oJsonArray:Add(oJsonObject).                                                                               */
/*          IF iLoop > 100 THEN                                                                                        */
/*            LEAVE MEDLEMLOOP.                                                                                        */
/*        END. /* MEDLEMLOOP */                                                                                        */
/*      END.                                                                                                           */
    ELSE DO:
        oJsonArray:Add(oJsonObject).
      END. 

    ASSIGN 
        oResponse:Entity        = oJsonArray
        /* HTTP messages require a content type */
        oResponse:ContentType   = 'application/json':u
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
            Purpose: PUT --> Oppdater.                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED INTEGER HandlePut( INPUT poRequest AS OpenEdge.Web.IWebRequest ):
      
      /* Behandles likt som POST. */
      HandlePost( INPUT poRequest ).
        
    END METHOD.
    
     
   /*------------------------------------------------------------------------------
            Purpose: POST --> Create.                                                               
            Notes:   Her er POST implementert slik at den gjør en update hvis
                     posten finnes fra før.                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED INTEGER HandlePost( INPUT poRequest AS OpenEdge.Web.IWebRequest ):

      DEFINE VARIABLE oResponse AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO.
      DEFINE VARIABLE oWriter   AS OpenEdge.Web.WebResponseWriter  NO-UNDO.
      DEFINE VARIABLE oData     AS JsonObject NO-UNDO.
      DEFINE VARIABLE oStatus   AS JsonObject NO-UNDO.
      
      DEFINE VARIABLE cStatus   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pcLogg    AS CHARACTER NO-UNDO.
  
      rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
  
      ASSIGN 
        pcLogg = cLogg + REPLACE(STRING(TODAY),'/','')
        . 
  
      IF poRequest:ContentType = 'application/json' THEN 
        DO:
          oData = CAST(poRequest:Entity, JsonObject).
          RUN cls\dintero\customerToDataset.p(oData, OUTPUT DATASET dsCustCustomer ).
          RUN cls\dintero\customerToMedlem.p(INPUT DATASET dsCustCustomer, OUTPUT cStatus ).
        END.
        
      FIND FIRST ttCustCustomer NO-ERROR.

      /* Logger medlemmer som opprettes. */
      IF AVAILABLE ttCustCustomer THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Medlem: ' + ttCustCustomer.customer_id + '.'
          ).    
      END.  
      oStatus = NEW JSonObject().
        
      CREATE ttRespons.
      ASSIGN 
        ttRespons.cStatus      = IF AVAILABLE ttCustCustomer THEN 'OK' ELSE 'FEIL'
        ttRespons.cTekst       = cStatus
        ttRespons.cCustomer_Id = IF AVAILABLE ttCustCustomer THEN ttCustCustomer.customer_id ELSE '' 
        .
      oStatus:ADD("Status", ttRespons.cStatus).
      oStatus:ADD("Tekst", ttRespons.cTekst).
      oStatus:ADD("customer_id", ttRespons.cCustomer_Id).

      ASSIGN 
        oResponse            = NEW OpenEdge.Web.WebResponse()
        oResponse:StatusCode = INTEGER(StatusCodeEnum:OK)
        oResponse:Entity        = oStatus
        oResponse:ContentType   = 'application/json':u
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
      RETURN 0.
    
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

  METHOD PUBLIC VOID prepMobilNr( INPUT piAction AS INTEGER, INPUT-OUTPUT pcMobilNr AS CHARACTER ):
    
    ASSIGN 
        pcMobilNr = REPLACE(pcMobilNr,'0047','+47')
        pcMobilNr = IF NOT pcMobilNr BEGINS '+47' THEN '+47' + pcMobilNr ELSE pcMobilNr
        .

    IF piAction = 1 THEN 
      ASSIGN 
          pcMobilNr = REPLACE(pcMobilNr,'+','%2B')
          .
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Konverterer oCustomoer objektet til et datasett.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID datasettCustomerToMedlem( INPUT poCustCustomer AS JSonObject, OUTPUT DATASET dsMedlem ):
    
    RETURN.

  END METHOD.
  
END CLASS.