 
 /*------------------------------------------------------------------------
    File        : Lager
    Syntax      : 
    Author(s)   : tny
    Created     : Mon Jul 30 11:50:22 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="Lager.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="VareLager", URI="/VareLager", schemaName="dsVareLager", schemaFile="").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS rfid.VareLager INHERITS BusinessEntity:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"rfid\ttVareLager.i"}
    {"bo\ttLagerInfo.i"}
    {"rfid\dsVareLager.i"}
    
    DEFINE DATA-SOURCE srcVareLager FOR StrekKode.
    DEFINE DATA-SOURCE srcLager FOR Lager.
    
    DEFINE VARIABLE iSeq AS INTEGER NO-UNDO.
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC VareLager():
        
        DEFINE VARIABLE hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VARIABLE cSkipListArray AS CHARACTER NO-UNDO EXTENT 1.
        
        SUPER (DATASET dsVareLager:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
            in DataSet */     
         hDataSourceArray[1] =  DATA-SOURCE srcVareLager:HANDLE.
         
                  
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

    METHOD PUBLIC VOID AddIdField( INPUT DATASET dsVareLager ):

        ASSIGN 
            ttVareLager.id  = STRING(ROWID(Strekkode))
            iSeq        = iSeq + 1
            ttVareLager.seq = iSeq
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
        DEFINE VARIABLE cWhere2        AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE hQuery         AS HANDLE            NO-UNDO.
        DEFINE VARIABLE lUseReposition AS LOGICAL           NO-UNDO.
        DEFINE VARIABLE iCount         AS INTEGER           NO-UNDO.
        DEFINE VARIABLE ablFilter      AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE id             AS CHARACTER         INITIAL ? NO-UNDO.
        DEFINE VARIABLE iMaxRows       AS INTEGER           INITIAL ? NO-UNDO.
        DEFINE VARIABLE iSkipRows      AS INTEGER           INITIAL ? NO-UNDO.
        DEFINE VARIABLE cOrderBy       AS CHARACTER         INITIAL "" NO-UNDO.

        /* purge any existing data */
        EMPTY TEMP-TABLE ttVareLager.
        EMPTY TEMP-TABLE ttLager.

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
            BUFFER ttVareLager:HANDLE:BATCH-SIZE = iMaxRows.
        END.
        ELSE 
        DO:
            IF id > "" THEN
                BUFFER ttVareLager:HANDLE:BATCH-SIZE = 1.
            ELSE
                BUFFER ttVareLager:HANDLE:BATCH-SIZE = 0.
        END.
        
        BUFFER ttVareLager:ATTACH-DATA-SOURCE(DATA-SOURCE srcVareLager:HANDLE).
        BUFFER ttLager:ATTACH-DATA-SOURCE(DATA-SOURCE srcLager:HANDLE).
        
        IF cOrderBy = ? THEN cOrderBy = "".
        cWhere = IF cWhere > "" THEN (cWhere + " " + cOrderBy)
        ELSE ("WHERE " + cOrderBy).
        
        DATA-SOURCE srcVareLager:FILL-WHERE-STRING = cWhere.
        IF lUseReposition THEN 
        DO:
            hQuery = DATA-SOURCE srcVareLager:QUERY.
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
                    CREATE ttVareLager.
                    BUFFER-COPY Strekkode TO ttVareLager.
                    ASSIGN 
                        ttVareLager.id  = STRING(ROWID(Strekkode))
                        iSeq           = iSeq + 1
                        ttVareLager.seq = iSeq.
                END.
                iCount = iCount + 1.
            END.
        END.
        ELSE 
        DO:
            IF id > "" THEN DATA-SOURCE srcVareLager:RESTART-ROWID(1)
                = TO-ROWID ((id)).
            BUFFER ttVareLager:SET-CALLBACK ("AFTER-ROW-FILL", "AddIdField").
            DATASET dsVareLager:FILL(). 
        END.
        FINALLY:
            BUFFER ttVareLager:DETACH-DATA-SOURCE().
        END FINALLY.
    END METHOD.
        
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadLager(
            INPUT filter AS CHARACTER, 
            OUTPUT DATASET dsVareLager):
        
        MESSAGE 'Filterverd ReadVare: ' + filter.
        
        IF filter BEGINS "~{" THEN
            THIS-OBJECT:JFPFillMethod (INPUT filter).
            
        ELSE 
        DO:
            BUFFER ttVareLager:HANDLE:BATCH-SIZE = 0.
            BUFFER ttVareLager:SET-CALLBACK ("AFTER-ROW-FILL", "AddIdField").
            SUPER:ReadData(filter).
        END.    
        
        FOR EACH ttVareLager:
            supplerInfoLager( INPUT ttVareLager.Kode ).
        END.
              
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID supplerInfoLager( INPUT pcKode AS CHARACTER  ):
        DEFINE VARIABLE pdDato AS DATE NO-UNDO.                                            

        /* Legg inn kode her for å berike temp tabellen. */
        FIND ArtBas OF ttVareLager NO-LOCK NO-ERROR.
        IF AVAILABLE ArtBas THEN
        ARTBAS_FUNNET: 
        DO:
            ASSIGN 
            ttVareLager.Beskr   = ArtBas.Besk
            ttVareLager.LevKod  = ArtBas.LevKod
            ttVareLager.LevFargKod = ArtBas.LevFargKod
            ttVareLager.VmId    = ArtBas.VMId
            ttVareLager.Sasong  = ArtBas.Sasong
            ttVareLager.LevNr   = ArtBas.LevNr
            .

            FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ttVareLager.ArtikkelNr AND  
                ArtPris.ProfilNr = 1 NO-ERROR.
            IF AVAILABLE ArtPris THEN 
            DO:
                ASSIGN
                ttVareLager.Pris = ArtPris.Pris[1]
                ttVareLager.InnkjopsPris = ArtPris.InnkjopsPris[1]
                ttVareLager.Varekost = ArtPris.VareKost[1]
                ttVareLager.Pris = IF ttVareLager.Pris = ? THEN 0 ELSE ttVareLager.Pris                 
                ttVareLager.InnkjopsPris = IF ttVareLager.InnkjopsPris = ? THEN 0 ELSE ttVareLager.InnkjopsPris                 
                ttVareLager.VareKost = IF ttVareLager.VareKost = ? THEN 0 ELSE ttVareLager.VareKost                 
                .
            END.  
            FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE Varemerke THEN 
                ttVareLager.VmBeskr = Varemerke.Beskrivelse.
            FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE SaSong THEN 
                ttVareLager.SasBeskr = SaSong.SasBeskr.
            FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE LevBas THEN 
                ttVareLager.LevBeskr = LevBas.levnamn.
                      
        END. /* ARTBAS_FUNNET */  

        RETURN.

    END METHOD.
    
END CLASS.
