 
 /*------------------------------------------------------------------------
    File        : LokasjonsListe
    Syntax      : 
    Author(s)   : tny
    Created     : Thu Aug 09 09:38:37 CEST 2018
    Notes       : Hej Tom
  ----------------------------------------------------------------------*/
  
@program FILE(name="LokasjonsListe.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="LokasjonsListe", URI="/LokasjonsListe", schemaName="dsTelleLinje", schemaFile="PRSRest/AppServer/rfid/lokasjonsliste.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS rfid.LokasjonsListe INHERITS BusinessEntity:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"rfid\ttLokasjonsliste.i"}
    {"rfid\dslokasjonsliste.i"}
    
    DEFINE DATA-SOURCE srcTelleLinje FOR SkoTex.TelleLinje.

    DEFINE VARIABLE iSeq     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lcJSon   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iTelleNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
    
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LokasjonsListe():
        
        DEFINE VARIABLE hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VARIABLE cSkipListArray AS CHARACTER NO-UNDO EXTENT 1.
        
        SUPER (DATASET dsTelleLinje:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
            in DataSet */     
         hDataSourceArray[1] =  DATA-SOURCE srcTelleLinje:HANDLE.
         
                  
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

    METHOD PUBLIC VOID AddIdField( INPUT DATASET dsTellelinje ):

        ASSIGN 
            ttTellelinje.id  = STRING(ROWID(Tellelinje))
            iSeq             = iSeq + 1
            ttTellelinje.seq = iSeq
            .

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID countFnName( INPUT filter AS CHARACTER,
                                    OUTPUT numRecs AS INTEGER ):

        
        FOR EACH ttTellelinje NO-LOCK:
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
        EMPTY TEMP-TABLE ttTellelinje.

        jsonParser = NEW ObjectModelParser().
        jsonObject = CAST(jsonParser:Parse(filter), jsonObject).
        iMaxRows = jsonObject:GetInteger("top") NO-ERROR.
        iSkipRows = jsonObject:GetInteger("skip") NO-ERROR.
        ablFilter = jsonObject:GetCharacter("ablFilter") NO-ERROR.
        id = jsonObject:GetCharacter("id") NO-ERROR.
        cOrderBy = jsonObject:GetCharacter("orderBy") NO-ERROR.
        cWhere = "WHERE " + ablFilter NO-ERROR.
        IF cOrderBy > "" THEN 
        DO:
            cOrderBy = REPLACE(cOrderBy, ",", " by ").
            cOrderBy = "by " + cOrderBy + " ".
            /* NOTE: id and seq fields should be removed from
            cWhere and cOrderBy */
            cOrderBy = REPLACE(cOrderBy, "by id desc", "").
            cOrderBy = REPLACE(cOrderBy, "by id ", "").
            cOrderBy = REPLACE(cOrderBy, "by seq desc", "").
            cOrderBy = REPLACE(cOrderBy, "by seq ", "").
        END.
        lUseReposition = iSkipRows <> ?.
        IF iMaxRows <> ? AND iMaxRows > 0 THEN 
        DO:
            BUFFER ttTellelinje:HANDLE:BATCH-SIZE = iMaxRows.
        END.
        ELSE 
        DO:
            IF id > "" THEN
                BUFFER ttTellelinje:HANDLE:BATCH-SIZE = 1.
            ELSE
                BUFFER ttTellelinje:HANDLE:BATCH-SIZE = 0.
        END.
        BUFFER ttTellelinje:ATTACH-DATA-SOURCE(DATA-SOURCE srcTellelinje:HANDLE).
        IF cOrderBy = ? THEN cOrderBy = "".
        cWhere = IF cWhere > "" THEN (cWhere + " " + cOrderBy)
        ELSE ("WHERE " + cOrderBy).
        DATA-SOURCE srcTellelinje:FILL-WHERE-STRING = cWhere.
        IF lUseReposition THEN 
        DO:
            hQuery = DATA-SOURCE srcTellelinje:QUERY.
            hQuery:QUERY-OPEN.    
            IF id > "" AND id <> "?" THEN 
            DO:
                hQuery:REPOSITION-TO-ROWID(TO-ROWID(id)).
            END.
            ELSE IF iSkipRows <> ? AND iSkipRows > 0 THEN 
                DO:
                    hQuery:REPOSITION-TO-ROW(iSkipRows).
                    IF NOT AVAILABLE Tellelinje THEN
                        hQuery:GET-NEXT() NO-ERROR.
                END.
            iCount = 0.
            REPEAT WHILE NOT hQuery:QUERY-OFF-END AND iCount < iMaxRows:
                hQuery:GET-NEXT () NO-ERROR.
                IF AVAILABLE Tellelinje THEN 
                DO:
                    CREATE ttTellelinje.
                    BUFFER-COPY Tellelinje TO ttTellelinje.
                    ASSIGN 
                        ttTellelinje.id  = STRING(ROWID(Tellelinje))
                        iSeq           = iSeq + 1
                        ttTellelinje.seq = iSeq.
                END.
                iCount = iCount + 1.
            END.
        END.
        ELSE 
        DO:
            IF id > "" THEN DATA-SOURCE srcTellelinje:RESTART-ROWID(1)
                = TO-ROWID ((id)).
            BUFFER ttTellelinje:SET-CALLBACK ("AFTER-ROW-FILL", "AddIdField").
            DATASET dsTellelinje:FILL().
        END.
        FINALLY:
            BUFFER ttTellelinje:DETACH-DATA-SOURCE().
        END FINALLY.
    END METHOD.
        
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadLokasjonsListe(
            INPUT filter AS CHARACTER, 
            OUTPUT DATASET dsTelleLinje):
        
        MESSAGE 'Lokasjonsliste.cls - ReadLokasjonsListe:' + filter.
        
        
        IF filter BEGINS "~{" THEN
            THIS-OBJECT:JFPFillMethod (INPUT filter).
            
        ELSE 
        DO:
            BUFFER ttTellelinje:HANDLE:BATCH-SIZE = 0.
            BUFFER ttTellelinje:SET-CALLBACK ("AFTER-ROW-FILL", "AddIdField").
            SUPER:ReadData(filter).
        END.            
        
        FOR EACH ttTelleLinje:
            IF AVAILABLE StrekKode THEN 
                RELEASE StrekKode.
            IF ttTelleLinje.Kode = '' THEN DO:
                FIND FIRST StrKonv NO-LOCK WHERE 
                    StrKonv.Storl = ttTelleLinje.Storl NO-ERROR.
                IF AVAILABLE StrKonv THEN  
                    FIND LAST StrekKode NO-LOCK WHERE
                        StrekKode.ArtikkelNr = ttTelleLinje.ArtikkelNr AND 
                        StrekKode.StrKode    = StrKonv.StrKode NO-ERROR.
                IF AVAILABLE StrekKode THEN 
                    ttTelleLinje.Kode = StrekKode.Kode. 
            END.
        END.
              
    END METHOD.
      
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreateLokasjonsListe(INPUT-OUTPUT DATASET dsTelleLinje):            
               
/*        SUPER:CreateData(DATASET dsTelleLinje BY-REFERENCE).*/
        
        MESSAGE 'Lokasjonsliste.cls - CreateLokasjonsliste.'.
        DATASET dsTelleLinje:WRITE-JSON ("file", 'CreateLokasjonsliste' + REPLACE(STRING(TODAY),'/','') + '.JSon',TRUE).        
        DATASET dsTelleLinje:WRITE-JSON ("longchar", lcJSon,TRUE).
        
        FIND FIRST ttTellelinje NO-ERROR.
        IF AVAILABLE ttTelleLinje THEN 
        DO:
            ASSIGN 
                iTelleNr = ttTelleLinje.TelleNr
                .
            IF CAN-FIND(FIRST TelleLinje WHERE 
                        TelleLinje.TelleNr = ttTelleLinje.TelleNr) THEN 
            DO:
                lagreLokasjonsliste('Oppdater', iTelleNr).
/*                SUPER:UpdateData(DATASET dsTelleLinje BY-REFERENCE).*/
            END.
            ELSE DO:
                lagreLokasjonsliste('Ny', iTelleNr).
/*                SUPER:CreateData(DATASET dsTelleLinje BY-REFERENCE).*/
            END.        
        END.

              
    END METHOD.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID lagreLokasjonsliste( INPUT pcFlagg AS CHARACTER,
                                              INPUT piTelleNr AS INTEGER ):

        DEFINE VARIABLE pLokTelleNr AS INTEGER NO-UNDO.

        DO TRANSACTION:
            FOR EACH ttTelleLinje
                BREAK BY ttTelleLinje.TelleNr:

                /* Oppretter tellehode. */
                IF FIRST-OF(ttTelleLinje.TelleNr) THEN 
                DO:
                     FIND LAST TelleHode NO-LOCK USE-INDEX TelleHode NO-ERROR.
                        IF AVAILABLE TelleHode THEN 
                            pLokTelleNr = TelleHode.TelleNr + 1.
                        ELSE
                            pLokTelleNr = 1.
                                                    
                    CREATE TelleHode.
                    ASSIGN 
                        TelleHode.TelleNr = pLokTelleNr
                        TelleHode.EDato = TODAY 
                        TelleHode.ETid = TIME 
                        TelleHode.BrukerID = ttTelleLinje.BrukerID
                        TelleHode.RegistrertDato = TODAY
                        TelleHode.RegistrertTid = TIME
                        TelleHode.RegistrertAv = USERID('dictdb')
                        TelleHode.Beskrivelse = 'REST PDA TelleNr: ' + STRING(ttTelleLinje.TelleNr) + ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
                        TelleHode.TTId = 9
                        TelleHode.TBId = 1
                        TelleHode.Notat = 'Opprettet via REST LokasjonsListeService fra PDA '
                        TelleHode.StartDato = TODAY
                        TelleHode.Oppdatert = ?
                        TelleHode.ButikkListe = STRING(ttTelleLinje.Butik)
/*                        TelleHode.AntallPar = plAntTalt  */
/*                        TelleHode.AntallTalt = plAntTalt */
/*                        TelleHode.OpptVerdi = plVerdi    */
/*                        TelleHode.AntLinjer = piAntLinjer*/ 
                        TelleHode.TilButikk = 0
                        TelleHode.OrdreNr = 0
                        TelleHode.PkSdlNr = 0
                        TelleHode.BatchNr = 0
                        TelleHode.TelleType = 2 /* Lokasjonsliste */
                        TelleHode.KobletTilTelleNr = 0
                        iLinjeNr = 0
                        .
                    RELEASE TelleHode.    
                END.
                
                /* Oppdaterer summer i tellehode. */
                IF LAST-OF(ttTelleLinje.TelleNr) THEN 
                DO:
                    summerTelling( INPUT pLokTelleNr ). 
                END.    
                    
                /* Legger på linjeinformasjon */
                FIND StrekKode NO-LOCK WHERE 
                    StrekKode.Kode = ttTelleLinje.Kode NO-ERROR.
                IF NOT AVAILABLE StrekKode THEN 
                    NEXT.
                FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
                IF NOT AVAILABLE StrKonv THEN 
                    NEXT.
                FIND Lager NO-LOCK WHERE 
                    Lager.ArtikkelNr = StrekKode.ArtikkelNr AND 
                    Lager.Butik      = ttTelleLinje.Butik NO-ERROR.
                FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ArtBas THEN 
                    NEXT.
                FIND Farg OF ArtBas NO-LOCK NO-ERROR.

                FIND ArtLag NO-LOCK WHERE
                    ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
                    ArtLag.Storl      = StrKonv.Storl AND 
                    ArtLag.Butik      = ttTelleLinje.Butik NO-ERROR.           
                    
                ASSIGN
                    ttTelleLinje.ArtikkelNr = StrekKode.ArtikkelNr 
                    ttTelleLinje.Storl      = StrKonv.Storl
                    ttTelleLinje.VVareKost  = (IF AVAILABLE Lager THEN Lager.VVarekost ELSE 0)
                    .    
                IF ttTelleLinje.VVareKost = ? THEN 
                    ttTelleLinje.VVareKost = 0.

                IF (ttTelleLinje.VVareKost = 0 OR ttTelleLinje.VVareKost = ? OR ArtBas.Lager = FALSE) THEN 
                DO:
                    FIND Butiker NO-LOCK WHERE 
                        Butiker.Butik = ttTelleLinje.Butik NO-ERROR.
                    FIND ArtPris NO-LOCK WHERE
                        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                    IF NOT AVAILABLE ArtPris THEN 
                        FIND FIRST ArtPris NO-LOCK WHERE 
                        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR. 
                    IF AVAILABLE ArtPris THEN
                        ttTelleLinje.VVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
                END.
                /* Oppretter tellelinje */
                FIND FIRST TelleLinje EXCLUSIVE-LOCK WHERE 
                    TelleLinje.TelleNr    = pLokTelleNr AND        
                    TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
                    TelleLinje.Butik      = ttTelleLinje.butik AND
                    TelleLinje.Storl      = ttTelleLinje.Storl NO-ERROR.

                IF NOT AVAILABLE TelleLinje THEN
                OPPRETT_TELLELINJE: 
                DO:
                    CREATE TelleLinje.
                    /* Setter index. */
                    ASSIGN
                        iLinjeNr = iLinjeNr + 1
                        TelleLinje.TelleNr    = pLokTelleNr
                        TelleLinje.LinjeNr    = iLinjeNr 
                        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
                        TelleLinje.Butik      = ttTelleLinje.Butik
                        TelleLinje.Storl      = ttTelleLinje.Storl
                        Tellelinje.Beskr      = ArtBas.Beskr
                        TelleLinje.LevKod     = ArtBas.LevKod
                        TelleLinje.Vg         = ArtBas.Vg
                        TelleLinje.LopNr      = ArtBas.LopNr
                        TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> "" THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.Farbeskr ELSE ""
                        TelleLinje.AntallPar  = IF AVAILABLE ArtLag THEN ArtLag.Lagant ELSE 0
                        TelleLinje.OpprVerdi  = TelleLinje.AntallPar * ttTelleLinje.VVareKost
                        TelleLinje.VVareKost  = ttTelleLinje.VVAreKost
                        TelleLinje.LevNr      = ArtBas.LevNr
                        TelleLinje.Sasong     = ArtBas.SaSong
                        TelleLinje.Farg       = ArtBas.Farg
                        TelleLinje.MatKod     = ArtBas.MatKod
                        TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg,">>>>>>9")) + "/" + TRIM(STRING(ArtBas.LopNr,">>>>>>9"))
                        TelleLinje.Kode       = ttTelleLinje.Kode
                        TelleLinje.NedSkrevet = ttTelleLinje.VVAreKost
                        .
                END. /* OPPRETT_TELLELINJE */
                
                ASSIGN
                    TelleLinje.OpprAntalTalt = TelleLinje.OpprAntalTalt + ttTelleLinje.AntallTalt
                    TelleLinje.AntallTalt    = TelleLinje.AntallTalt    + ttTelleLinje.AntallTalt 
                    TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * ttTelleLinje.VVarekost
                    TelleLinje.AntallDiff    = TelleLinje.AntallPar - TelleLinje.AntallTalt
                    TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
                    .                          
                RELEASE TelleLinje. /* Denne MÅ få stå */                                
            END.

        END. /* TRANSACTION */

        RETURN.

    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdateLokasjonsListe(INPUT-OUTPUT DATASET dsTelleLinje):            
    
/*        SUPER:UpdateData(DATASET dsTelleLinje BY-REFERENCE).*/
        
        MESSAGE 'Lokasjonsliste.cls - CreateLokasjonsliste.'.
        DATASET dsTelleLinje:WRITE-JSON ("file", 'UpdateLokasjonsliste' + REPLACE(STRING(TODAY),'/','') + '.JSon',TRUE).        
        DATASET dsTelleLinje:WRITE-JSON ("longchar", lcJSon,TRUE).
        
        FIND FIRST ttTellelinje NO-ERROR.
        IF AVAILABLE ttTelleLinje THEN 
        DO:
            ASSIGN 
                iTelleNr = ttTelleLinje.TelleNr
                .
            IF CAN-FIND(TelleLinje WHERE 
                        TelleLinje.TelleNr = ttTelleLinje.TelleNr) THEN 
            DO:
                lagreLokasjonsliste('Oppdater', iTelleNr).
/*                SUPER:UpdateData(DATASET dsTelleLinje BY-REFERENCE).*/
            END.
            ELSE DO:
                lagreLokasjonsliste('Ny', iTelleNr).
/*                SUPER:CreateData(DATASET dsTelleLinje BY-REFERENCE).*/
            END.        
        END.
           
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeleteLokasjonsListe(INPUT-OUTPUT DATASET dsTelleLinje):                    
    
        SUPER:DeleteData(DATASET dsTelleLinje BY-REFERENCE).
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitLokasjonsListe", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitLokasjonsListe(INPUT-OUTPUT DATASET dsTelleLinje):                    
    
        MESSAGE 'LokasjonsListe.cls - SubmitLokasjonsliste.'.
        DATASET dsTelleLinje:WRITE-JSON ("file", 'SUBMITUpdateLokasjonsListe' + REPLACE(STRING(TODAY),'/','') + '.JSon',TRUE).        
        DATASET dsTelleLinje:WRITE-JSON ("longchar", lcJSon,TRUE).
    
          SUPER:Submit(DATASET dsTelleLinje BY-REFERENCE).           
    END METHOD.
     
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="invoke", URI="/count", alias="", mediaType="application/json").
    METHOD PUBLIC VOID count( INPUT filter AS CHARACTER, OUTPUT numRecs AS
        INTEGER):
        DEFINE VARIABLE jsonParser AS ObjectModelParser NO-UNDO.
        DEFINE VARIABLE jsonObject AS JsonObject        NO-UNDO.
        DEFINE VARIABLE ablFilter  AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE cWhere     AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE qh         AS HANDLE            NO-UNDO.
        
        MESSAGE 'LokasjonsListe.cls - count:' + filter.
                
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
        qh:SET-BUFFERS(BUFFER TelleLinje:HANDLE).
        qh:QUERY-PREPARE("PRESELECT EACH Tellelinje " + cWhere).
        qh:QUERY-OPEN ().
        numRecs = qh:NUM-RESULTS.
    END METHOD.  
     
    METHOD PUBLIC LOGICAL summerTelling( INPUT piTelleNr AS INTEGER  ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        DEF VAR piAntallPar     AS DEC  NO-UNDO.
        DEF VAR piAntallTalt    AS DEC  NO-UNDO.
        DEF VAR piOpptVerdi     AS DEC  NO-UNDO.
        DEF VAR piVerdiDiff     AS DEC  NO-UNDO.
        DEF VAR piAntallDiff    AS DEC  NO-UNDO.
        DEF VAR piOpprVerdi     AS DEC  NO-UNDO.
        DEF VAR piAntLinjer     AS DEC  NO-UNDO.
        DEF VAR plOppdatert     AS DATE NO-UNDO.
        DEF VAR piAntallNegDiff AS DEC  NO-UNDO.
        DEF VAR piAntallPosDiff AS DEC  NO-UNDO.
        DEF VAR piVerdiNegDiff  AS DEC  NO-UNDO.
        DEF VAR piVerdiPosDiff  AS DEC  NO-UNDO.

        FIND TelleHode NO-LOCK WHERE TelleHode.TelleNr = piTelleNr NO-ERROR.

        IF AVAIL TelleHode THEN
        DO:
            ASSIGN
                piAntallPar     = 0
                piAntallTalt    = 0
                piOpptVerdi     = 0
                piVerdiDiff     = 0
                piAntallDiff    = 0
                piOpprVerdi     = 0
                piAntLinjer     = 0
                piAntallNegDiff = 0
                piAntallPosDiff = 0
                piVerdiNegDiff  = 0
                piVerdiPosDiff  = 0
                .

            /* Sumerer opp linjene. */
            FOR EACH TelleLinje OF TelleHode NO-LOCK:
                ASSIGN
                    piAntallPar  = piAntallPar  + TelleLinje.AntallPar
                    piAntallTalt = piAntallTalt + TelleLinje.AntallTalt
                    piOpprVerdi  = piOpprVerdi  + TelleLinje.OpprVerdi      
                    piOpptVerdi  = piOpptVerdi  + TelleLinje.OpptVerdi
                    piAntLinjer  = piAntLinjer  + 1      
                    .
                /* Sumerer positive differanser. */
                IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) < 0 THEN
                    ASSIGN
                        piAntallNegDiff = piAntallNegDiff + (TelleLinje.AntallPar - TelleLinje.AntallTalt)
                        piVerdiNegDiff  = piVerdiNegDiff  + (TelleLinje.OpprVerdi - TelleLinje.OpptVerdi).
                /* Sumerer negative differanser. */
                IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) > 0 THEN
                    ASSIGN
                        piAntallPosDiff = piAntallPosDiff + (TelleLinje.AntallPar - TelleLinje.AntallTalt)
                        piVerdiPosDiff  = piVerdiPosDiff  + (TelleLinje.OpprVerdi - TelleLinje.OpptVerdi).
            END.
            /* Differanser */
            ASSIGN
                piVerdiDiff  = piOpprVerdi  - piOpptVerdi
                piAntallDiff = piAntallPar  - piAntallTalt
                .      

            DO TRANSACTION:
                FIND CURRENT TelleHode EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL TelleHode THEN
                DO:
                    ASSIGN
                        TelleHode.AntallPar  = piAntallPar  
                        TelleHode.AntallTalt = piAntallTalt 
                        TelleHode.OpptVerdi  = piOpptVerdi  
                        TelleHode.VerdiDiff  = piVerdiDiff  
                        TelleHode.AntallDiff = piAntallDiff 
                        TelleHode.OpprVerdi  = piOpprVerdi  
                        TelleHode.AntLinjer  = piAntLinjer.
                    /* Skal ikke sumeres for lokasjonslister. */
                    IF TelleHode.TelleType = 1 THEN
                        ASSIGN
                            TelleHode.AntallNegDiff = piAntallNegDiff
                            TelleHode.AntallPosDiff = piAntallPosDiff
                            TelleHode.VerdiNegDiff  = piVerdiNegDiff
                            TelleHode.VerdiPosDiff  = piVerdiPosdiff
                            .

                END.
            END. /*Transaction*/
            RELEASE TelleHode.
        END.

        RETURN bResult.

    END METHOD.
    
END CLASS.
