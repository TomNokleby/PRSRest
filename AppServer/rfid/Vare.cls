 
/*------------------------------------------------------------------------
   File        : Vare
    
   Syntax      : 
   Author(s)   : Tom Nøkleby
   Created     : Mon Jul 30 11:42:31 CEST 2018
   Notes       : REST Endpoint som leverer vare og prisinformasjon.
                 Prisinformasjonen omfatter pris på ulike pris profiler, 
                 samt eventuell tilbudspris med periode angivelse.
 ----------------------------------------------------------------------*/
  
@program FILE(name="Vare.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="Vare", URI="/Vare", schemaName="dsStrekKode", schemaFile="PRSRest/AppServer/rfid/vare.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS rfid.Vare INHERITS BusinessEntity:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
  {"rfid\ttVare.i"}
  {"rfid\dsVare.i"}
    
  DEFINE DATA-SOURCE srcVare FOR StrekKode.
        
  DEFINE VARIABLE iSeq       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iNetBut    AS INTEGER   NO-UNDO. 
  DEFINE VARIABLE iProfilNr  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dEDato     AS DATE      NO-UNDO.
  DEFINE VARIABLE cablFilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iOperator  AS INTEGER   NO-UNDO. 
       
    
  /*------------------------------------------------------------------------------
          Purpose:                                                                      
          Notes:                                                                        
  ------------------------------------------------------------------------------*/
  CONSTRUCTOR PUBLIC Vare():
        
    DEFINE VARIABLE hDataSourceArray AS HANDLE    NO-UNDO EXTENT 1.
    DEFINE VARIABLE cSkipListArray   AS CHARACTER NO-UNDO EXTENT 1.

    DEFINE BUFFER bufButiker FOR Butiker.

    SUPER (DATASET dsVare:HANDLE).
        
    {syspara.i 150 1 2 iNetBut INT}
    FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = iNetBut NO-ERROR.
    IF AVAILABLE Butiker THEN 
      iProfilNr = Butiker.ProfilNr.
    ELSE 
      iProfilNr = 16.
        
    DATASET dsVare:SET-CALLBACK ( "BEFORE-FILL" , "preDataSetFill" ).

    /* Data Source for each table in dataset. Should be in table order as defined 
        in DataSet */     
    hDataSourceArray[1] =  DATA-SOURCE srcVare:HANDLE.
         
                  
    /* Skip-list entry for each table in dataset. Should be in temp-table order 
      as defined in DataSet */
    /* Each skip-list entry is a comma-separated list of field names, to be
        ignored in create stmt */
             
    cSkipListArray[1] = "".
         
         
    THIS-OBJECT:ProDataSource = hDataSourceArray.
    THIS-OBJECT:SkipList = cSkipListArray.
                 
  END CONSTRUCTOR.
        
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID AddIdField( INPUT DATASET dsvare ):

    ASSIGN 
      ttvare.id  = STRING(ROWID(Strekkode))
      iSeq       = iSeq + 1
      ttvare.seq = iSeq
      .

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
  @progress.service.resourceMapping(type="REST", operation="invoke", URI="/count", alias="", mediaType="application/json").
  METHOD PUBLIC VOID count( INPUT filter AS CHARACTER, OUTPUT numRecs AS
    INTEGER):
    DEFINE VARIABLE jsonParser AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE jsonObject AS JsonObject        NO-UNDO.
    DEFINE VARIABLE ablFilter  AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cWhere     AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE qh         AS HANDLE            NO-UNDO.
        
    IF filter BEGINS "WHERE " THEN
      cWhere = filter.
    ELSE IF filter BEGINS "~{" THEN
      DO:
        jsonParser = NEW ObjectModelParser().
        jsonObject = CAST(jsonParser:Parse(filter), jsonObject).
        ablFilter = jsonObject:GetCharacter("ablFilter") NO-ERROR.
        cWhere = "WHERE " + ablFilter.
      END.
      ELSE IF filter NE "" THEN
        DO:
          /* Use filter as WHERE clause */
          cWhere = "WHERE " + filter.
        END.
    CREATE QUERY qh.
    qh:SET-BUFFERS(BUFFER Strekkode:HANDLE).
    qh:QUERY-PREPARE("PRESELECT EACH Prod " + cWhere).
    qh:QUERY-OPEN ().
    numRecs = qh:NUM-RESULTS.
    qh:QUERY-CLOSE ().
    DELETE OBJECT qh. 
  END METHOD.  

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID countFnName( INPUT filter AS CHARACTER,
    OUTPUT numRecs AS INTEGER ):

        
    FOR EACH strekkode NO-LOCK:
      numRecs = numRecs + 1.
    END. 

    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID JFPFillMethod( INPUT filter AS CHARACTER ):
    DEFINE VARIABLE jsonParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE jsonObject     AS JsonObject        NO-UNDO.
    DEFINE VARIABLE cWhere         AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE hQuery         AS HANDLE            NO-UNDO.
    DEFINE VARIABLE lUseReposition AS LOGICAL           NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER           NO-UNDO.
    DEFINE VARIABLE ablFilter      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE id             AS CHARACTER         INITIAL ? NO-UNDO.
    DEFINE VARIABLE iMaxRows       AS INTEGER           INITIAL ? NO-UNDO.
    DEFINE VARIABLE iSkipRows      AS INTEGER           INITIAL ? NO-UNDO.
    DEFINE VARIABLE cOrderBy       AS CHARACTER         INITIAL "" NO-UNDO.

    /* purge any existing data */
    EMPTY TEMP-TABLE ttVare.
    EMPTY TEMP-TABLE ttVArePris.

    jsonParser = NEW ObjectModelParser().
        
    jsonObject = CAST(jsonParser:Parse(filter), jsonObject).
        
    iMaxRows = jsonObject:GetInteger("top") NO-ERROR.
    iSkipRows = jsonObject:GetInteger("skip") NO-ERROR.
    ablFilter = jsonObject:GetCharacter("ablFilter") NO-ERROR.
    id = jsonObject:GetCharacter("id") NO-ERROR.
        
    cOrderBy = jsonObject:GetCharacter("orderBy") NO-ERROR.
        
    /* Tar vare på filter for bruk til utvidet søk. */
    cablFilter = ablFilter.
        
    cWhere = "WHERE " + ablFilter NO-ERROR.
    IF cOrderBy > "" THEN
    ORDERBYBLOKK: 
    DO:
      cOrderBy = REPLACE(cOrderBy, ",", " by ").
      cOrderBy = "by " + cOrderBy + " ".
        
      /* NOTE: id and seq fields should be removed from
      cWhere and cOrderBy */
      cOrderBy = REPLACE(cOrderBy, "by id desc", "").
      cOrderBy = REPLACE(cOrderBy, "by id ", "").
      cOrderBy = REPLACE(cOrderBy, "by seq desc", "").
      cOrderBy = REPLACE(cOrderBy, "by seq ", "").
    END. /* ORDERBYBLOKK */
    lUseReposition = iSkipRows <> ?.
    IF iMaxRows <> ? AND iMaxRows > 0 THEN 
    DO:
      BUFFER ttVare:HANDLE:BATCH-SIZE = iMaxRows.
    END.
    ELSE 
    DO:
      IF id > "" THEN
        BUFFER ttVare:HANDLE:BATCH-SIZE = 1.
      ELSE
        BUFFER ttVare:HANDLE:BATCH-SIZE = 0.
    END.
        
    BUFFER ttVare:ATTACH-DATA-SOURCE(DATA-SOURCE srcVare:HANDLE).
    IF cOrderBy = ? THEN cOrderBy = "".
    cWhere = IF cWhere > "" THEN (cWhere + " " + cOrderBy)
    ELSE ("WHERE " + cOrderBy).
        
    DATA-SOURCE srcVare:FILL-WHERE-STRING = cWhere.
    BUFFER ttVarePris:FILL-MODE = "NO-FILL". 
        
    IF lUseReposition THEN 
    DO:
      hQuery = DATA-SOURCE srcVare:QUERY.
      hQuery:QUERY-OPEN.    
      IF id > "" AND id <> "?" THEN 
      DO:
        hQuery:REPOSITION-TO-ROWID(TO-ROWID(id)).
      END.
      ELSE IF iSkipRows <> ? AND iSkipRows > 0 THEN 
        DO:
          hQuery:REPOSITION-TO-ROW(iSkipRows).
          IF NOT AVAILABLE Strekkode THEN
            hQuery:GET-NEXT() NO-ERROR.
        END.
      iCount = 0.
      REPEAT WHILE NOT hQuery:QUERY-OFF-END AND iCount < iMaxRows:
        hQuery:GET-NEXT () NO-ERROR.
        IF AVAILABLE Strekkode THEN 
        DO:
          CREATE ttVare.
          BUFFER-COPY Strekkode TO ttVare.
          ASSIGN 
            ttVare.id  = STRING(ROWID(Strekkode))
            iSeq       = iSeq + 1
            ttVare.seq = iSeq.
        END.
        iCount = iCount + 1.
      END.
      hQuery:QUERY-CLOSE().
      DELETE OBJECT hQuery.
    END.
    ELSE 
    DO:
      IF id > "" THEN DATA-SOURCE srcVare:RESTART-ROWID(1)
        = TO-ROWID ((id)).
      BUFFER ttVare:SET-CALLBACK ("AFTER-ROW-FILL", "AddIdField").
      DATASET dsVare:FILL().
    END.
    FINALLY:
      BUFFER ttVare:DETACH-DATA-SOURCE().
    END FINALLY.
  END METHOD.

  /*------------------------------------------------------------------------------
          Purpose:  Get one or more records, based on a filter string                                                                     
          Notes:                                                                        
  ------------------------------------------------------------------------------*/
  @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
  @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
  @openapi.openedge.method.property (name="mappingType", value="JFP").
  @openapi.openedge.method.property (name="capabilities", value="ablFilter,top,skip,id,orderBy").
  METHOD PUBLIC VOID ReadVare(
    INPUT filter AS CHARACTER, 
    OUTPUT DATASET dsVare):
        
    MESSAGE 'Filterverd ReadVare: ' + filter.
    /* Eksempler på bruk av filter
    Filterverd ReadVare: {"ablFilter":"Kode = '7325706169718'"}
    Filterverd ReadVare: {"ablFilter":"eDato >='30/05/2022'"}
    Filterverd ReadVare: {"ablFilter":"eDato >='02/06/2022'"}
    */
        
    EMPTY TEMP-TABLE ttVare.
    EMPTY TEMP-TABLE ttVarePris.
    
    IF filter BEGINS "~{" THEN
      THIS-OBJECT:JFPFillMethod (INPUT filter).
            
    ELSE 
    DO:
      BUFFER ttVare:HANDLE:BATCH-SIZE = 0.
      BUFFER ttVare:SET-CALLBACK ("AFTER-ROW-FILL", "AddIdField").
      SUPER:ReadData(filter).
    END.    

    MESSAGE 'Filterverdi - renset ReadVare:' cablFilter.

    /* Er eDato angitt i filteret, skal det gjøres utvidet søk mot ArtPris. */
    dEDato = ?.
    IF cablFilter MATCHES '*eDato*' THEN 
    DO:
      /* eDato >='02/06/2022' */
      cablFilter = REPLACE(cablFilter,"'","").
      /* eDato >=02/06/2022 */
      iOperator = (IF cablFilter MATCHES '* =*' THEN 1
      ELSE IF cablFilter MATCHES '*>=*' THEN 2
      ELSE 3 /* <= */
        ).
      dEDato = DATE(ENTRY(2,cablFilter,'=')).
      MESSAGE '  cablFilter:' cablFilter ' dEDato:' dEDato ' iOperator:' iOperator.
    END.
                
    /* Legger på ekstra informasjon på vare recordden */
    FOR EACH ttVare:
      MESSAGE '  Vare:' ttVare.Kode ttVare.Beskr ttVare.EDato.
      supplerInfoVare( INPUT ttVare.Kode ).
    END.

    /* Er eDato angitt i filteret, skal det gjøres utvidet søk mot ArtPris. */
    IF cablFilter MATCHES '*eDato*' THEN 
    DO:
      supplerPris (dEDato, iOperator). 
      supplerVare (dEDato, iOperator).
    END.
        
              
  END METHOD. 

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID supplerInfoVare( INPUT pcKode AS CHARACTER  ):
    DEFINE VARIABLE pdDato AS DATE NO-UNDO.             
        
    DEFINE BUFFER bufButiker FOR butiker.    

    MESSAGE '    supplerInfoVare HENTER:' ttVare.Kode ttVare.Beskr.  
        
    /* Legg inn kode her for å berike temp tabellen. */
    FIND Strekkode NO-LOCK WHERE 
      Strekkode.Kode = ttVare.Kode NO-ERROR.
    IF AVAILABLE StrekKode THEN 
      FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas THEN
    ARTBAS_FUNNET: 
    DO:
      ASSIGN 
        ttVare.Beskr      = ArtBas.Besk
        ttVare.LevKod     = ArtBas.LevKod
        ttVare.LevFargKod = ArtBas.LevFargKod
        ttVare.Vg         = ArtBas.Vg
        ttVare.VmId       = ArtBas.VMId
        ttVare.Farg       = ArtBas.Farg
        ttVare.Sasong     = ArtBas.Sasong
        ttVare.LevNr      = ArtBas.LevNr
        ttVare.NOS        = (IF ArtBas.Lagerkoder MATCHES '*NOS*' THEN 'NOS' ELSE '')
        ttVare.eDato      = ArtBas.eDato  
        .

      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ttVare.ArtikkelNr AND  
        ArtPris.ProfilNr = iProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ttVare.ArtikkelNr AND  
          ArtPris.ProfilNr = 1 NO-ERROR.
      IF AVAILABLE ArtPris THEN
      DO:
        ASSIGN
          ttVare.Pris           = ArtPris.Pris[1]
          ttVare.InnkjopsPris   = ArtPris.InnkjopsPris[1]
          ttVare.Varekost       = ArtPris.VareKost[1]
          ttVare.Pris           = IF ttVare.Pris = ? THEN 0 ELSE ttVare.Pris
          ttVare.InnkjopsPris   = IF ttVare.InnkjopsPris = ? THEN 0 ELSE ttVare.InnkjopsPris
          ttVare.VareKost       = IF ttVare.VareKost = ? THEN 0 ELSE ttVare.VareKost
          ttVare.ProfilNr       = ArtPris.ProfilNr
          ttVare.TilbPris       = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE 0
          ttVare.Mva%           = ArtPris.Mva%[1]
          ttVare.TilbFraDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudFraDato, ArtPris.TilbudFraTid * 1000) ELSE ?
          ttVare.TilbTilDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudTilDato, ArtPris.TilbudTilTid * 1000) ELSE ?
          .
        /* TN 1/10-22 Hvis det avgrenses på dato, skal ikke pris pr. butikk legges ut. */
        IF dEDato = ? THEN 
        DO:
          FOR EACH bufButiker NO-LOCK WHERE
            bufButiker.ApningsDato <= TODAY AND
            bufButiker.harButikkSystem = TRUE AND
            bufButiker.NedlagtDato = ? AND
            bufButiker.Butik <= 9999:
            IF CAN-DO('848,849',STRING(bufButiker.Butik)) THEN
              NEXT.
            FIND ttVarePris WHERE
              ttVarePris.Kode     = ttVare.Kode AND
              ttVarePris.ButikkNr = bufButiker.Butik NO-ERROR.
            IF NOT AVAILABLE ttVarePris THEN
            DO:
              CREATE ttVarePris.
              ASSIGN
                ttVarePris.seq      = ttVare.Seq
                ttVarePris.Kode     = ttVare.Kode
                ttVarePris.ProfilNr = ArtPris.ProfilNr
                ttVarePris.ButikkNr = bufButiker.Butik
                .
            END.
            ASSIGN
              ttVarePris.Pris           = ArtPris.Pris[1]
              ttVarePris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
              ttVarePris.Varekost       = ArtPris.VareKost[1]
              ttVarePris.Pris           = IF ttVarePris.Pris = ? THEN 0 ELSE ttVarePris.Pris
              ttVarePris.InnkjopsPris   = IF ttVarePris.InnkjopsPris = ? THEN 0 ELSE ttVarePris.InnkjopsPris
              ttVarePris.VareKost       = IF ttVarePris.VareKost = ? THEN 0 ELSE ttVarePris.VareKost
              ttVarePris.Mva%           = ArtPris.Mva%[1]
              ttVarePris.TilbPris       = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE 0
              ttVarePris.TilbPris       = IF ttVarePris.TilbPris = ? THEN 0 ELSE ttVarePris.TilbPris
              ttVarePris.TilbFraDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudFraDato, ArtPris.TilbudFraTid * 1000) ELSE ?
              ttVarePris.TilbTilDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudTilDato, ArtPris.TilbudTilTid * 1000) ELSE ?
              ttVarePris.EDato          = ArtPris.EDato
              .
          END.
        END.
      END.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE VarGr THEN 
      DO:
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        ASSIGN 
          ttVare.VgBeskr = VarGr.VgBeskr
          ttVare.Hg      = VarGr.Hg
          ttVare.HgBeskr = (IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE '')
          .
      END.
      FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE Varemerke THEN 
        ttVare.VmBeskr = Varemerke.Beskrivelse.
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE Farg THEN 
        ttVare.FarBeskr = Farg.FarBeskr.
      FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE SaSong THEN 
        ttVare.SasBeskr = SaSong.SasBeskr.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE LevBas THEN 
        ttVare.LevBeskr = LevBas.levnamn.
      FIND StrekKode NO-LOCK WHERE 
        StrekKode.Kode = ttVare.Kode NO-ERROR.
      IF AVAILABLE StrekKode THEN 
        FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
      IF AVAILABLE StrKonv THEN 
        ttVare.Storl = StrKonv.Storl.
      MESSAGE '    supplerInfoVare OPPDATERER:' ttVare.Kode ttVare.Beskr.  
    END. /* ARTBAS_FUNNET */  

    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID preDataSetFill( INPUT DATASET dsvare ):
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL supplerPris( INPUT pdEDato AS DATE,
                                     INPUT piOperator AS INTEGER ):   
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bufButiker FOR Butiker.
    
    PRISLOOP:
    FOR EACH ArtPris NO-LOCK WHERE
      ArtPris.ProfilNr = iProfilNr AND  
      (IF piOperator = 1 THEN ArtPris.EDato = pdEDato
       ELSE IF piOperator = 2 THEN ArtPris.EDato >= pdEDato
       ELSE IF piOperator = 3 THEN ArtPris.EDato <= pdEDato
       ELSE TRUE),
       FIRST ArtBas OF ArtPris NO-LOCK,
       EACH Strekkode NO-LOCK WHERE 
         Strekkode.ArtikkelNr = ArtPris.ArtikkelNr:
       
      FIND FIRST ttVare WHERE
        ttVare.Kode     = Strekkode.Kode NO-ERROR.
      IF NOT AVAILABLE ttVare THEN 
      PRISOPPSTANDELSEN:
      DO:
        CREATE ttVare.
        ASSIGN
          ttVare.Kode       = Strekkode.Kode 
          ttVare.ArtikkelNr = Strekkode.ArtikkelNr
          ttVare.eDato      = ArtBas.eDato  
          .
        supplerInfoVare( INPUT ttVare.Kode ).
      END. /* PRISOPPSTANDELSEN */

      /* Her skal pris bare legges ut for de butikker som har fått endret pris på den aktuelle profilen. */
      FOR EACH bufButiker NO-LOCK WHERE 
        bufButiker.ApningsDato <= TODAY AND 
        bufButiker.harButikkSystem = TRUE AND 
        bufButiker.NedlagtDato = ? AND 
        bufButiker.Butik <= 9999 AND 
        bufButiker.ProfilNr = ArtPris.ProfilNr:
        IF CAN-DO('848,849',STRING(bufButiker.Butik)) THEN 
          NEXT.
        FIND ttVarePris WHERE
          ttVarePris.Kode     = ttVare.Kode AND 
          ttVarePris.ButikkNr = bufButiker.Butik NO-ERROR.
        IF NOT AVAILABLE ttVarePris THEN 
        DO: 
          CREATE ttVarePris.
          ASSIGN 
            ttVarePris.seq      = ttVare.Seq
            ttVarePris.Kode     = ttVare.Kode
            ttVarePris.ProfilNr = ArtPris.ProfilNr
            ttVarePris.ButikkNr = bufButiker.Butik
            .
        END.
        ASSIGN
          ttVarePris.Pris           = ArtPris.Pris[1]
          ttVarePris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
          ttVarePris.Varekost       = ArtPris.VareKost[1]
          ttVarePris.Pris           = IF ttVarePris.Pris = ? THEN 0 ELSE ttVarePris.Pris                 
          ttVarePris.InnkjopsPris   = IF ttVarePris.InnkjopsPris = ? THEN 0 ELSE ttVarePris.InnkjopsPris                 
          ttVarePris.VareKost       = IF ttVarePris.VareKost = ? THEN 0 ELSE ttVarePris.VareKost   
          ttVarePris.Mva%           = ArtPris.Mva%[1]
          ttVarePris.TilbPris       = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE 0
          ttVarePris.TilbPris       = IF ttVarePris.TilbPris = ? THEN 0 ELSE ttVarePris.TilbPris                 
          ttVarePris.TilbFraDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudFraDato, ArtPris.TilbudFraTid * 1000) ELSE ?
          ttVarePris.TilbTilDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudTilDato, ArtPris.TilbudTilTid * 1000) ELSE ?
          ttVarePris.EDato          = ArtPris.EDato 
          .
      END.  
      MESSAGE '    supplerPris:' ttVare.Kode ttVare.Beskr.       
    END. /* PRISLOOP */  

    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL supplerVare( INPUT pdEDato AS DATE,
                                     INPUT piOperator AS INTEGER  ):    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bufButiker FOR Butiker.
    
    PRISLOOP:
    FOR EACH ArtBas NO-LOCK WHERE
      (IF piOperator = 1 THEN ArtBas.EDato = pdEDato
       ELSE IF piOperator = 2 THEN ArtBas.EDato >= pdEDato
       ELSE IF piOperator = 3 THEN ArtBas.EDato <= pdEDato
       ELSE TRUE),
       EACH ArtPris OF ArtBas NO-LOCK WHERE 
         ArtPris.ProfilNr = iProfilNr,
       EACH Strekkode NO-LOCK WHERE 
         Strekkode.ArtikkelNr = ArtPris.ArtikkelNr:
       
      FIND FIRST ttVare WHERE
        ttVare.Kode = Strekkode.Kode NO-ERROR.
      IF NOT AVAILABLE ttVare THEN 
      PRISOPPSTANDELSEN:
      DO:
        CREATE ttVare.
        ASSIGN
          ttVare.Kode       = Strekkode.Kode 
          ttVare.ArtikkelNr = Strekkode.ArtikkelNr
          ttVare.eDato      = ArtBas.eDato  
          .
        supplerInfoVare( INPUT ttVare.Kode ).
      END. /* PRISOPPSTANDELSEN */
      
      /* Skal bare tas med hvis dato er angitt og dato er nyere på varepris. */
      IF dEDato <> ? AND ArtPris.EDato >= dEDato THEN 
      DO:    
        FOR EACH bufButiker NO-LOCK WHERE 
          bufButiker.ApningsDato <= TODAY AND 
          bufButiker.harButikkSystem = TRUE AND 
          bufButiker.NedlagtDato = ? AND 
          bufButiker.Butik <= 9999:
          IF CAN-DO('848,849',STRING(bufButiker.Butik)) THEN 
            NEXT.
          FIND ttVarePris WHERE
            ttVarePris.Kode     = ttVare.Kode AND 
            ttVarePris.ButikkNr = bufButiker.Butik NO-ERROR.
          IF NOT AVAILABLE ttVarePris THEN 
          DO: 
            CREATE ttVarePris.
            ASSIGN 
              ttVarePris.seq      = ttVare.Seq
              ttVarePris.Kode     = ttVare.Kode
              ttVarePris.ProfilNr = ArtPris.ProfilNr
              ttVarePris.ButikkNr = bufButiker.Butik
              .
          END.
          ASSIGN
            ttVarePris.Kode           = ttVare.Kode
            ttVarePris.Pris           = ArtPris.Pris[1]
            ttVarePris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
            ttVarePris.Varekost       = ArtPris.VareKost[1]
            ttVarePris.Pris           = IF ttVarePris.Pris = ? THEN 0 ELSE ttVarePris.Pris                 
            ttVarePris.InnkjopsPris   = IF ttVarePris.InnkjopsPris = ? THEN 0 ELSE ttVarePris.InnkjopsPris                 
            ttVarePris.VareKost       = IF ttVarePris.VareKost = ? THEN 0 ELSE ttVarePris.VareKost   
            ttVarePris.Mva%           = ArtPris.Mva%[1]
            ttVarePris.TilbPris       = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE 0
            ttVarePris.TilbPris       = IF ttVarePris.TilbPris = ? THEN 0 ELSE ttVarePris.TilbPris                 
            ttVarePris.TilbFraDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudFraDato, ArtPris.TilbudFraTid * 1000) ELSE ?
            ttVarePris.TilbTilDatoTid = IF ArtPris.Tilbud THEN DATETIME(ArtPris.TilbudTilDato, ArtPris.TilbudTilTid * 1000) ELSE ?
            ttVarePris.EDato          = ArtPris.EDato 
            .
        END.
      END.  
      MESSAGE '    supplerVare:' ttVare.Kode ttVare.Beskr.       
    END. /* PRISLOOP */  

    RETURN bResult.
  END METHOD.
    
END CLASS.
