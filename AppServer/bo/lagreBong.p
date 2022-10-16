
/*------------------------------------------------------------------------
    File        : lagreBong.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Tue Aug 02 13:35:12 CEST 2022
    Notes       :

                        None                          =  0
                        Unknown                       = -1
                            
                            /* 1xx: Informational - Request received, continuing process */
                        Continue                      = 100
                        SwitchingProtocols            = 101
                        Processing                    = 102
                        /* 103-199     Unassigned */
                            
                            /* 2xx: Success - The action was successfully received, understood, and accepted */
                        OK                            = 200
                        Created                       = 201
                        Accepted                      = 202
                        NonAuthoritativeInformation   = 203
                        NoContent                     = 204
                        ResetContent                  = 205
                        PartialContent                = 206
                        Multi-Status                  = 207
                        AlreadyReported               = 208
                        /* 209-225  Unassigned */     
                        IMUsed                        = 226
                            
                            /* 3xx: Redirection - Further action must be taken in order to complete the request */
                        MultipleChoices               = 300
                        MovedPermanently              = 301
                        Found                         = 302
                        SeeOther                      = 303
                        NotModified                   = 304
                        UseProxy                      = 305
                        /* 306 Unused */
                        TemporaryRedirect             = 307
                        PermanentRedirect             = 308
                        /* 309-399  Unassigned */
           
                /* 4xx: Client Error - The request contains bad syntax or cannot be fulfilled */
                        BadRequest                    = 400
                        Unauthorized                  = 401
                        PaymentRequired               = 402
                        Forbidden                     = 403
                        NotFound                      = 404
                        MethodNotAllowed              = 405
                        NotAcceptable                 = 406                                                                   
                        ProxyAuthenticationRequired   = 407
                        RequestTimeOut                = 408
                        Conflict                      = 409
                        Gone                          = 410
                        LengthRequired                = 411
                        PreconditionFailed            = 412
                        RequestEntityTooLarge         = 413
                        RequestURITooLarge            = 414
                        UnsupportedMediaType          = 415
                        RequestedRangeNotSatisfiable  = 416
                        ExpectationFailed             = 417
                        /* 418-420  Unassigned */
                        MisdirectedRequest            = 421
                        UnprocessableEntity           = 422
                        Locked                        = 423
                        FailedDependency              = 424
                        /* 425 Unassigned */
                        UpgradeRequired               = 426
                        /*427 Unassigned */
                        PreconditionRequired          = 428
                        TooManyRequests               = 429
                        /* 430 Unassigned */
                        RequestHeaderFieldsTooLarge   = 431
                        /* 432-499     Unassigned */
                     
                      /* 5xx: Server Error - The server failed to fulfill an apparently valid request */
                        InternalServerError           = 500
                        NotImplemented                = 501
                        BadGateway                    = 502
                        ServiceUnavailable            = 503
                        GatewayTimeout                = 504
                        HTTPVersionNotSupported       = 505
                        VariantAlsoNegotiates         = 506
                        InsufficientStorage           = 507
                        LoopDetected                  = 508
                        /* 509 Unassigned */
                        NotExtended                   = 510
                        NetworkAuthenticationRequired = 511
                        /* 512-599     Unassigned */
                        .      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{bo\bong.i}

DEFINE INPUT PARAMETER DATASET FOR dsBongHode.
DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cTekst  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER iStatusCode AS INTEGER NO-UNDO.

DEFINE VARIABLE cFilNavn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER NO-UNDO.
DEFINE VARIABLE lFilId      AS DECIMAL NO-UNDO.
DEFINE VARIABLE lDataSettId AS DECIMAL NO-UNDO.

/*BLOCK-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST ttBongHode NO-LOCK NO-ERROR.
IF AVAILABLE ttBongHode THEN 
BONGMOTTATT:
DO.
  ASSIGN 
  cKatalog = IF SEARCH('tnc.txt') <> ? 
               THEN 'C:\NSoft\Polygon\PRS\kom\in\Terki\'
             ELSE 
               'C:\Polygon\PRS\kom\in\Terki\'
  cFilNavn = cKatalog + 
              'bong_' + 
              STRING(ttBongHode.butikkNr) + '_' + 
              STRING(ttBongHode.KasseNr) + '-' +
              STRING(ttBongHode.BongNr) + '-' +
              STRING(ttBongHode.Dato,'99999999') + '-' + 
              REPLACE(STRING(ttBongHode.Tid,'HH:MM:SS'),':','') +
              '.json'
              .

  DATASET dsBongHode:WRITE-JSON('file',cFilNavn,TRUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    ASSIGN  
      cStatus     = 'FEIL'
      cTekst      = 'Feil ved skriving av bong til fil.'
      iStatusCode = 400
      .
  ELSE 
  LAGRE:
  DO:
    /* Finnes bong fra før med samme B_Id? */
    FIND FIRST BongHode NO-LOCK WHERE 
               BongHode.B_Id = ttBongHode.b_id NO-ERROR.
    IF AVAILABLE BongHode THEN  
      DO:
        ASSIGN  
          cStatus     = 'FEIL'
          cTekst      = 'Bong finnes fra før med samme B_Id (' + STRING(ttBongHode.B_Id) + ').'
          iStatusCode = 400
          .
        LEAVE LAGRE.
      END.
    FIND FIRST BongHode NO-LOCK WHERE 
               BongHode.ButikkNr = ttBongHode.ButikkNr AND 
               BongHode.GruppeNr = ttBongHode.GruppeNr AND 
               BongHode.KasseNr  = ttBongHode.KasseNr AND 
               BongHode.Dato     = ttBongHode.Dato AND 
               BongHode.BongNr   = ttBongHode.BongNr NO-ERROR.
    IF AVAILABLE BongHode THEN  
      DO:
        ASSIGN  
          cStatus     = 'FEIL'
          cTekst      = 'Bong finnes fra før med samme butikk, gruppe, kasse, dato og bongnr.'
          iStatusCode = 400
          .
        LEAVE LAGRE.
      END.
    ELSE DO:
      LAGREBONG: 
      DO TRANSACTION:
        FIND FIRST ttBongHode. 
        CREATE BongHode.
        BUFFER-COPY ttBongHode TO BongHode 
          ASSIGN 
            BongHode.BongStatus = 5 /* Oppdatert */
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
          cTekst = 'Feil ved lagring av Bong til databasen.'.
          DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
            cTekst = cTekst + CHR(10) + 
                     STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+ 
                     ERROR-STATUS:GET-MESSAGE(ix). 
          END.
          ASSIGN  
            cStatus     = 'FEIL'
            iStatusCode = 400
            .
          IF AVAILABLE BongHode THEN 
            DELETE BongHode.
          LEAVE LAGREBONG.
        END.
        
        FOR EACH ttBongLinje:
          CREATE BongLinje.
          BUFFER-COPY ttBongLinje TO BongLinje NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          DO:
            cTekst = 'Feil ved lagring av Bonglinjer til databasen.'.
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
              cTekst = cTekst + CHR(10) + 
                       STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+ 
                       ERROR-STATUS:GET-MESSAGE(ix). 
            END.
            ASSIGN  
              cStatus     = 'FEIL'
              iStatusCode = 400
              .
            IF AVAILABLE BongHode THEN 
              DELETE BongHode.
            IF AVAILABLE BongLinje THEN 
              DELETE BongLinje. 
            LEAVE LAGREBONG.
          END.
          
        END.  

        RUN opprettFil.
        RUN opprettDatasett.
        ASSIGN 
          BongHode.DataSettId = lDataSettId
          .
          
        IF AVAILABLE BongHode THEN 
          RELEASE BongHode.
        IF AVAILABLE BongLinje THEN 
          RELEASE BongLinje.
        
        ASSIGN  
          cStatus     = 'OK'
          cTekst      = 'Vellykket lagring av Bong.'
          iStatusCode = 200
          .
      END. /* LAGREBONG */
    END. /* LAGRE */
  END.
  
  IF iStatusCode = 200 THEN 
  DO:
    
  END.
  
END. /* BONGMOTTATT */

PROCEDURE OpprettFil:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Oppretter posten i filen. */
  IF NOT CAN-FIND(Filer WHERE
                  Filer.FilNavn   = "Salg fra kundeordre via REST API" AND
    Filer.Dato      = TODAY AND
                  Filer.Kl        = STRING(TIME,"HH:MM") AND
                  Filer.Storrelse = 0 AND
                  Filer.Katalog   = "Nettbutikk"
                 ) THEN
  DO TRANSACTION:
    /* Finner FilId */
    FIND LAST Filer NO-LOCK NO-ERROR.
    IF AVAILABLE Filer THEN
      lFilId = Filer.FilId + 1.
    ELSE
      lFilId = 1.
    CREATE Filer.
    ASSIGN
      Filer.FilId       = lFilId
      Filer.FilNavn     = "Salg fra kundeordre via REST API" 
      Filer.Dato        = TODAY 
      Filer.Kl          = STRING(TIME,"HH:MM:SS") 
      Filer.Storrelse   = 0 
      Filer.Katalog     = "Nettbutikk"
      Filer.AntLinjer   = 0
      Filer.FilType     = 1 
      Filer.Innlest     = TRUE
      Filer.InnlestDato = TODAY 
      Filer.InnlestKl   = TIME
      Filer.Oppdatert   = TRUE
      Filer.OppdatertDato = TODAY 
      Filer.OppdatertKl = TIME
      .
    RELEASE Filer.
  END.

END PROCEDURE.

PROCEDURE OpprettDatasett:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR piSettNr AS INT  NO-UNDO.

    OPPRETTDATASETT:
    DO TRANSACTION:
      /* Finner neste SettNr */
      FIND LAST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = ttBongHode.ButikkNr AND
        Datasett.GruppeNr = 1  AND
          Datasett.KasseNr  = ttBongHode.KasseNr  AND
        Datasett.Dato     = TODAY  AND
          DataSett.FilType  = 1 /* EL-Journal */
          USE-INDEX DataSett NO-ERROR.
      IF AVAILABLE DataSett THEN
          piSettNr = DataSett.SettNr + 1.
      ELSE DO:
          piSettNr = 1.
      END.

      /* Finner neste DataSettId */
      FIND LAST DataSett NO-LOCK
          USE-INDEX DataSettId NO-ERROR.
      IF AVAILABLE DataSett THEN
          lDataSettId = DataSett.DataSettId + 1.
      ELSE
          lDataSettId = 1.

      RELEASE DataSett. /* Ny post skal skapes. */

      IF NOT AVAILABLE DataSett THEN
      DO:
        CREATE DataSett.
        ASSIGN
            DataSett.DataSettId = lDataSettId
            DataSett.SettStatus = 2 /* Ankommet */
            DataSett.Behandlet  = 3 /* Behandlet */
            .
      END.

      ASSIGN
        DataSett.ButikkNr   = ttBongHode.ButikkNr 
        DataSett.GruppeNr   = 1 
        DataSett.KasseNr    = ttBongHode.KasseNr
        DataSett.Dato       = TODAY 
        DataSett.SettNr     = piSettNr
        DataSett.Tid        = 0
        DataSett.FilId      = lFilId
        DataSett.FilType    = 1 /* EL-Journal */
        .
      RELEASE Datasett.
    END. /* OPPRETTDATASETT */
END PROCEDURE.