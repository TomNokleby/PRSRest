
/*------------------------------------------------------------------------
    File        : lagreBong.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Tue Aug 02 13:35:12 CEST 2022
    Notes       :

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
DEFINE VARIABLE iButNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/*BLOCK-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.

ASSIGN 
  cLogg = 'Lagrebong' + STRING(TODAY,'99999999')
  .
OS-COMMAND SILENT mkdir VALUE('log') NO-ERROR. 

FIND FIRST ttBongHode NO-LOCK NO-ERROR.

IF AVAILABLE ttBongHode THEN 
BONGMOTTATT:
DO.
  ASSIGN 
  iButNr   = ttBongHode.ButikkNr
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
              REPLACE(STRING(ttBongHode.Tid,'HH:MM:SS'),':','') + '_' + 
              REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(STRING(NOW),'/',''),':',''),' ','_'),',','_'),'+','_') +
              '.json'
              .
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Fil: ' + cFilNavn   
      ).
  MESSAGE 'Loggfil: ' + cFilNavn.  
      
  /* Lagrer en kopi av bongen på disk. */
  DATASET dsBongHode:WRITE-JSON('file',cFilNavn,TRUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
    ASSIGN  
      cStatus     = 'FEIL'
      cTekst      = 'Feil ved skriving av bong til fil.'
      iStatusCode = 400
      .
    MESSAGE cstatus STRING(iStatusCode) cTekst.  
  END.
  ELSE 
  LAGRE:
  DO:
    /* Må gjøre dette på nytt da skriving til jason filen gjør at bufferen blir borte! */
    FIND FIRST ttBongHode NO-LOCK NO-ERROR.

    /* Sjekker butikk */
    FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = ttBongHode.butikkNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
      DO:
        ASSIGN
          cStatus     = 'FEIL'
          cTekst      = 'Ukjent butikk på bongen (' + STRING(iButNr) + ').'
          iStatusCode = 400
          .
        MESSAGE cstatus STRING(iStatusCode) cTekst.  
        LEAVE LAGRE.
      END.
    
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
        MESSAGE cstatus STRING(iStatusCode) cTekst.  
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
        MESSAGE cstatus STRING(iStatusCode) cTekst.  
        LEAVE LAGRE.
      END.
    ELSE DO:
      LAGREBONG: 
      DO TRANSACTION:
        /* Validerer og legger på informasjon i bonghode. */ 
        RUN addInfoBongHode.
        IF RETURN-VALUE <> 'OK' THEN 
        DO:
          ASSIGN  
            cStatus     = 'FEIL'
            cTekst      = RETURN-VALUE
            iStatusCode = 400
            .
          MESSAGE cstatus STRING(iStatusCode) cTekst. 
          UNDO LAGREBONG, LEAVE LAGRE. 
        END.
        
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
          MESSAGE cstatus STRING(iStatusCode) cTekst.  
          IF AVAILABLE BongHode THEN 
            DELETE BongHode.
          UNDO LAGREBONG, LEAVE LAGRE. 
        END.
        
        LESBONGLINJER:
        FOR EACH ttBongLinje:
          
          /* Validerer og adderer informasjon på bonglinje. */
          RUN addInfoBongLinje.
          IF RETURN-VALUE <> 'OK' THEN 
          DO:
            ASSIGN  
              cStatus     = 'FEIL'
              cTekst      = RETURN-VALUE
              iStatusCode = 400
              .
            MESSAGE cstatus STRING(iStatusCode) cTekst.  
            UNDO LAGREBONG, LEAVE LAGRE. 
          END.
        
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
            MESSAGE cstatus STRING(iStatusCode) cTekst.  
            IF AVAILABLE BongHode THEN 
              DELETE BongHode.
            IF AVAILABLE BongLinje THEN 
              DELETE BongLinje. 
            UNDO LAGREBONG, LEAVE LAGRE. 
          END.
        END. /* LESBONGLINJER */  

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
          cTekst      = 'Meget vellykket lagring av Bong.'
          iStatusCode = 200
          .
        MESSAGE cstatus STRING(iStatusCode) cTekst.  
      END. /* LAGREBONG */
    END. /* LAGRE */
  END.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'StatusCode: ' + STRING(iStatusCode) + ' Status: ' + cStatus + ' ' + cTekst   
      ).
  MESSAGE 'Ferdig lagrebong.p:' cstatus STRING(iStatusCode) cTekst.  
  
  IF iStatusCode = 200 THEN 
  DO:
    
  END.
  
END. /* BONGMOTTATT */



/* **********************  Internal Procedures  *********************** */


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

PROCEDURE addInfoBongHode:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   
      Settes av DB trigger:
      ---------------------
      "ODato": "2023-02-19",
      "OTid": 48230,
      "OAv": "tomn",
      "EDato": "2023-02-19",
      "ETid": 59480,
      "EAv": "pub",

      Settes av innlesningsprogrammet:
      --------------------------------      
      "BongStatus": 7,
      "DataSettId": 1.0,
      "GruppeNr": 1,
      "KundeNr": 121563.0,
      "MedlemsNr": 0.0,
      "KassererNavn": "Tom",
      "SelgerNavn": "",
      "MedlemNavn": "",
      "KOrdre_Id": 1220000440.0,
      "KundeNavn": "Geir Langseth",
      "MedlemsKort": "",
      "KundeKort": "",

      Ignoreres.:
      -----------
      "OverforingsNr": 0.0,
      "Gradering": 0,
      "flBetalingskort": false,
      "flBankkort": false,
      "flKreditkort": false,
      "flGavekort": false,
      "flSjekk": false,
      "flRekvisisasjon": false,
      "flKupong1": false,
      "flSlKort": 0,
      "flRabatt": false,
      "Systemkort": "",
      "flSystemkort": false,
      "EksportertDato": null,
      "pfFlagg": 1,
      "Kampanje": false,
      "SkiftNr": 0,
      "UtskriftsKopi": "Nada",
      "OpdKvit": true,
      "OpdUtskKopi": false,
      "Konvertert": true,
      "Logg": "",
      "KortType": 0,
      "Makulert": 0,
      "SkiftId": 0.0,
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rcTekst AS CHARACTER NO-UNDO.
  
  /* Sjekker felt som må være med. */
  /*      "b_id": 230500036217.0,  */
  /*      "BongNr": 5,             */
  /*      "ButikkNr": 999,         */
  /*      "Dato": "2022-05-09",    */
  /*      "Tid": 48230,            */
  /*      "Belop": 999.0,          */
  /*      "KasseNr": 99,           */
  /*      "KassererNr": 1001.0,    */
  /*      "SelgerNr": 0,           */
  /*      "TTId": 0,               */
  VALIDERBONGHODE:
  DO:
    IF ttBongHode.B_Id = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.B_Id er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.BongNr = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.BongNr er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.ButikkNr = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.ButikkNr er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.Dato = ? THEN 
      DO:
        rcTekst = 'Felt BongHode.Dato er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.Tid = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.Tid er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.Belop = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.Belop er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.KasseNr = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.KasseNr er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.KassererNr = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.KassererNr er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.SelgerNr = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.SelgerNr er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
    IF ttBongHode.TTId = 0 THEN 
      DO:
        rcTekst = 'Felt BongHode.TTId er ikke utfylt.'.
        LEAVE VALIDERBONGHODE.
      END.
  END. /* VALIDERBONGHODE */  

  LEGGPAINFO:
  DO:
    /*  Settes av innlesningsprogrammet:*/
    /*  --------------------------------*/
    /*  "BongStatus": Settes annet sted.*/
    /*  "DataSettId": Settes annet sted.*/
    /*  "GruppeNr": 1,                  */
    /*  "KundeNr": 121563.0,            */
    /*  "MedlemsNr": 0.0,               */
    /*  "KassererNavn": "Tom",          */
    /*  "SelgerNavn": "",               */
    /*  "MedlemNavn": "",               */
    /*  "KOrdre_Id": 1220000440.0,      */
    /*  "KundeNavn": "Geir Langseth",   */
    /*  "MedlemsKort": "",              */
    /*  "KundeKort": "",                */
    ASSIGN 
      ttBongHode.GruppeNr = 1
      .
    IF ttBongHode.MedlemsNr > 0 OR ttBongHode.MedlemsKort <> '' THEN 
    DO:
      /* Her må kode legges inn...*/
    END.
    IF ttBongHode.KundeNr > 0 OR ttBongHode.KundeKort <> '' THEN 
    DO:
      /* Her må kode legges inn...*/
    END.
    IF ttBongHode.SelgerNr > 0 THEN 
    DO:
      FIND Selger NO-LOCK WHERE 
        Selger.SElgerNr = ttBongHode.SelgerNr NO-ERROR.
      IF AVAILABLE Selger THEN 
        ASSIGN 
          ttBongHode.SelgerNavn = Selger.Navn.
    END.
    IF ttBongHode.KassererNr > 0 THEN 
    DO:
      FIND Forsalj NO-LOCK WHERE 
        Forsalj.ForsNr = INT(ttBongHode.KassererNr) NO-ERROR.
      IF AVAILABLE Forsalj THEN 
        ASSIGN 
          ttBongHode.SelgerNavn = Forsalj.FoNamn. 
    END.
  END. /* LEGGPAINFO */
  
  IF TRIM(ttBongHode.MedlemsKort) <> '' THEN 
   DO:
       FIND FIRST Medlemskort NO-LOCK WHERE MedlemsKort.KortNr = ttBongHode.MedlemsKort NO-ERROR.
       /* Deretter med ledende nuller */
       IF NOT AVAILABLE MedlemsKort AND LENGTH(ttBongHode.Medlemskort) <= 6 THEN
           FIND FIRST MedlemsKort NO-LOCK WHERE MedlemsKort.KortNr = STRING(INT(ttBongHode.Medlemskort),"999999") NO-ERROR.
       ELSE IF NOT AVAILABLE MedlemsKort AND LENGTH(ttBongHode.Medlemskort) > 6 THEN
           FIND FIRST MedlemsKort NO-LOCK WHERE MedlemsKort.KortNr = ttBongHode.Medlemskort NO-ERROR.
       /* Vi har funnet et medlemskort, altså er dette et medlem. */
       IF AVAILABLE MedlemsKort THEN MEDLEMSKORT: 
       DO:
           ttBongHode.KortType = 3.
           FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
           IF AVAILABLE Medlem THEN 
           DO:
               ASSIGN ttBongHode.MedlemsNr  = Medlem.MedlemsNr
                      ttBongHode.MedlemNavn = Medlem.Fornavn + " " + Medlem.EtterNavn.
               /* Er medlemmet koblet til en kunde, skal også kundenummer settes i transaksjonen. */
               IF ttBongHode.Kundekort = "" AND Medlem.KundeNr <> 0 THEN 
               DO:
                 FIND Kunde NO-LOCK WHERE
                     Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
                 ASSIGN
                   ttBongHode.KundeNr   = Medlem.KundeNr
                   ttBongHode.KundeNavn = IF AVAILABLE Kunde
                                  THEN Kunde.Navn
                                  ELSE "*Ukjent*".
               END.
           END.
       END.
       ELSE
           ttBongHode.MedlemsKort = "".
   END.  
    IF TRIM(ttBongHode.KundeKort) <> '' THEN 
    DO:
       FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = ttBongHode.Kundekort NO-ERROR.
       IF NOT AVAILABLE KundeKort AND LENGTH(ttBongHode.Kundekort) <= 6 THEN
           FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = STRING(INT(ttBongHode.Kundekort),"999999") NO-ERROR.
       ELSE IF NOT AVAILABLE KundeKort AND LENGTH(ttBongHode.Kundekort) > 6 THEN
           FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = ttBongHode.Kundekort NO-ERROR.
       IF AVAILABLE KundeKort THEN 
       DO:
         FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
             ASSIGN ttBongHode.KundeNr   = KundeKort.KundeNr
                    ttBongHode.KundeNavn = IF AVAIL kunde THEN Kunde.Navn ELSE "Ukjent".
       END.
    END.
  
  IF rcTekst = '' THEN 
    rcTekst = 'OK'.
  RETURN rcTekst.  

END PROCEDURE.

PROCEDURE addInfoBongLinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
    Felt som Ignoreres:
    ------------------
    "AlternativPrisRabatt": 0.00,
    "EAv": "",
    "EDato": null,
    "ETid": 0,
    "KampanjeId": 0,
    "KampEierId": 0,
    "KampId": 0.00,
    "KampTilbId": 0,
    "GenerellRabatt": 0.00,
    "IndividNr": 0.00,
    "Kunderabatt": 0.00,
    "LinjerabattPersonal": 0.00,
    "ManuelEndretPrisRabatt": 0.00,
    "Medlemsrabatt": 0.00,
    "MixMatchRabatt": 0.00,
    "NotatKode": 0,
    "NotatKodeTekst": "",
    "OAv": "",
    "ODato": null,
    "OrgVareGr": 0,
    "OriginalData": "",
    "OTid": 0,
    "Personalrabatt": 0.00,
    "PrisPrSalgsenhet": 0.00,
    "ProduktType": 1,
    "ReturButikk": 0,
    "ReturKasserer": 0.00,
    "ReturKassererNavn": "",
    "SalgsType": false,
    "SeqNr": 1,
    "SkiftNr": 0,
    "SubtotalrabattPersonal": 0.00,
    "Tilbudsrabatt": 0.00,
    "TransDato": "2022-05-09",
    "TransNr": 1426,
    "TransTid": 48230,
    "ForKonvertering": "",
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rcTekst AS CHARACTER NO-UNDO.

  VALIDERBONGLINJE:
  DO:
    /*Felt som må settes:                                          */
    /*-------------------                                          */
    /*"Antall": 1.000,                                             */
    /*"LinjeNr": 1,                                                */
    /*"TBId": 1,                                                   */
    /*"LinjeSum": 1200.00,                                         */
    /*"Strekkode": "7325706579944",                                */
    /*"MButikkNr": Må settes ved overføring                        */
    /*"MvaKr": 192.00,                            */
    /*"Mva%": 25.00,                              */
    /*"TTId": 1,                                  */
    IF ttBongLinje.TTId = 0 OR NOT CAN-FIND(FIRST TransType WHERE TransType.TTId = ttBongLinje.TTId) THEN
      DO:
        rcTekst = 'Felt BongLinje.TTId er ikke utfylt.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.Antall = 0 AND ttBongLinje.TTId < 11 THEN
      DO:
        rcTekst = 'Felt BongLinje.Antall er ikke utfylt.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.Linjesum = 0 AND ttBongLinje.TTId < 11 THEN
      DO:
        rcTekst = 'Felt BongLinje.Linjesum er ikke utfylt.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.LinjeNr = 0 THEN
      DO:
        rcTekst = 'Felt BongLinje.LinjeNr er ikke utfylt.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.TbId = 0 THEN
      DO:
        rcTekst = 'Felt BongLinje.TbId er ikke utfylt.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.Strekkode = '' AND ttBongLinje.TTId < 11 THEN
      DO:
        rcTekst = 'Felt BongLinje.Strekkode er ikke utfylt.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.MButikkNr = 0 AND ttBongLinje.TTId = 6 THEN
      DO:
        rcTekst = 'Felt BongLinje.MButikkNr er ikke utfylt på varelinje med overføring.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.Mva% = 0 AND CAN-DO('01,06',STRING(ttBongLinje.TTId)) THEN
      DO:
        rcTekst = 'Felt BongLinje.Mva% er ikke utfylt på varelinje med varesalg eller overføring.'.
        LEAVE VALIDERBONGLINJE.
      END.
    IF ttBongLinje.MvaKr = 0 AND CAN-DO('01,06',STRING(ttBongLinje.TTId)) THEN
      DO:
        rcTekst = 'Felt BongLinje.MvaKr er ikke utfylt på varelinje med varesalg eller overføring.'.
        LEAVE VALIDERBONGLINJE.
      END.
  END. /* VALIDERBONGLINJE */  

  LEGGPAKANSETTESINFO:
  DO:
    /*Felt som kan settes:                                         */
    /*-------------------                                          */
    /*"RefNr": 0,                                                  */
    /*"RefTekst": Refnr og reftekst feltene settes hvis man ønsker.*/
    /*"FeilKode": 0,                                               */
    /*"FeilKodeTekst": Her settes feilkode ved reklamasjon.        */
    /*"LinjeRab": 240.00,                                          */
    /*"SubtotalRab": 0.00,                                         */
    /*"Normalpris": 0.00,                                          */
  END. /* LEGGPAKANSETTESINFO */

  LEGGPAINFO:
  DO:
    /*Fyles ut ved mottak av bong                 */
    /*---------------------------                 */
    /*"b_id": 230500036217.00,                    */
    /*"BongNr": 5,                                */
    /*"GruppeNr": 1,                              */
    /*"KasseNr": 99,                              */
    /*"Dato": "2022-05-09",                       */
    /*"ButikkNr": 999,                            */
    /*"AaaaMmDd": "20220509",                     */
    /*"ArtikkelNr": "9865043",                    */
    /*"BongTekst": "D1. REG PASTEL OXF STRIPE BD",*/
    /*"DivInfo": "Felt for oppbevaring",          */
    /*"HovedGr": 0,                               */
    /*"HovedGrBeskrivelse": "",                   */
    /*"LevNavn": "",                              */
    /*"LevNr": 0,                                 */
    /*"LopeNr": 4287,                             */
    /*"Makulert": false,                          */
    /*"MvaGr": 1,                                 */
    /*"MvaGruppeNavn": "Mva = 25%",               */
    /*"MvaKr": 192.00,                            */
    /*"Storrelse": " M",                          */
    /*"Type": 0,                                  */
    /*"VareGr": 100160,                           */
    /*"VareGruppeNavn": "MEN",                    */
    /*"VVarekost": 378.00                         */
    /*"BongPris": 1200.00,                        */
    ASSIGN /* Arves fra BongHode. */
      ttBongLinje.b_id      = ttBongHode.B_Id
      ttBongLinje.BongNr    = ttBongHode.BongNr
      ttBongLinje.GruppeNr  = ttBongHode.GruppeNr
      ttBongLinje.KasseNr   = ttBongHode.KasseNr
      ttBongLinje.Dato      = ttBongHode.Dato
      ttBongLinje.TransDato = ttBongHode.Dato
      ttBongLinje.ButikkNr  = ttBongHode.ButikkNr
      ttBongLinje.AaaaMmDd  = STRING(YEAR(ttBongLinje.TransDato),"9999") + 
                              string(MONTH(ttBongLinje.TransDato),"99") + 
                              string(DAY(ttBongLinje.TransDato),"99")
      .
    IF ttBongLinje.TTId < 11 THEN 
    VARELINJE:
    DO:
      FIND Strekkode NO-LOCK WHERE strekkode.Kode = ttBongLinje.Strekkode NO-ERROR.
      IF NOT AVAILABLE Strekkode THEN 
      DO:
        rcTekst = 'Felt BongLinje.Strekkode inneholder en ukjent strekkode.'.
        LEAVE LEGGPAINFO.
      END.
      FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
      IF AVAILABLE StrKonv THEN 
        ttBongLinje.Storrelse = StrKonv.Storl.
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN 
      DO:
        rcTekst = 'Felt BongLinje.Strekkode er ikke koblet til en artikkel.'.
        LEAVE LEGGPAINFO.
      END.
      ELSE DO:
        FIND butiker NO-LOCK WHERE Butiker.Butik = iButNr NO-ERROR.
        FIND huvgr OF ArtBas NO-LOCK NO-ERROR.
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        FIND LevBas OF ArtBas NO-LOCK NO-ERROR.  
        FIND ArtPris OF ArtBas NO-LOCK WHERE 
          ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.    
        FIND FIRST Moms NO-LOCK WHERE 
          Moms.MomsProc = ttBongLinje.Mva% NO-ERROR.
  
        ASSIGN
          ttBongLinje.BongTekst = IF ttBongLinje.BongTekst = '' THEN ArtBas.Bongtekst ELSE ttBongLinje.BongTekst 
          ttBongLinje.LopeNr    = ArtBas.LopNr
          ttBongLinje.VareGr    = ArtBas.Vg
          ttBongLinje.DivInfo   = ArtBas.DivInfo[1]
          ttBongLinje.HovedGr   = ArtBas.Hg
          ttBongLinje.HovedGrBeskrivelse = IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE ttBongLinje.HovedGrBeskrivelse
          ttBongLinje.VareGruppeNavn = IF AVAILABLE VArGr THEN VarGr.VgBeskr ELSE ttBongLinje.VareGruppeNavn
          ttBongLinje.LevNr     = ArtBas.LevNr
          ttBongLinje.LevNavn   = IF AVAILABLE LevBas THEN LevBas.levnamn ELSE ttBongLinje.LevNavn
          ttBongLinje.MvaGr     = IF AVAILABLE Moms THEN Moms.MomsKod ELSE 1
          ttBongLinje.MvaGruppeNavn = IF AVAILABLE Moms THEN Moms.Beskrivelse ELSE ttBongLinje.MvaGruppeNavn
          ttBongLinje.BongPris  = IF AVAILABLE ArtPris THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE ttBongLinje.BongPris     
          .
      END.
    END. /* VARELINJE */
    
  END. /* LEGGPAINFO */
  
  IF rcTekst = '' THEN 
    rcTekst = 'OK'.
  RETURN rcTekst.  

END PROCEDURE.
