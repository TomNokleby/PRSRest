 
 /*------------------------------------------------------------------------
    File        : VareOgLager
    Syntax      : 
    Author(s)   : tny
    Created     : Wed Aug 01 12:06:33 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="VareOgLager.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="VareOgLager", URI="/VareOgLager", schemaName="ttVareLager", schemaFile="PRSRest/AppServer/rfid/ttVareLager.i").

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS rfid.VareOgLager:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"rfid\ttVareOgLager.i"}
        
    DEFINE VARIABLE ihBuffer1 AS HANDLE NO-UNDO.
    DEFINE VARIABLE ihBuffer2 AS HANDLE NO-UNDO.
    DEFINE VARIABLE ihBuffer3 AS HANDLE NO-UNDO.
    DEFINE VARIABLE ihBuffer4 AS HANDLE NO-UNDO.
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
        
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadVareOgLager(
            INPUT filter AS CHARACTER, 
            OUTPUT TABLE ttVareLager):

        DEFINE VARIABLE pcWhere AS CHARACTER.
        DEFINE VARIABLE pcKode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcButNr AS CHARACTER NO-UNDO.

        EMPTY TEMP-TABLE ttVareLager.
        
        ihBuffer1 = BUFFER Strekkode:HANDLE.    
        ihBuffer2 = BUFFER ArtLag:HANDLE.
        ihBuffer3 = BUFFER Lager:HANDLE.    
        ihBuffer4 = BUFFER StrKonv:HANDLE.
        
        filter = TRIM(filter,'"').
        
        MESSAGE 'Filter fra VareOgfilter: ' filter.
        
        pcWhere = "FOR EACH Strekkode WHERE Kode = '&Ean',
                   first StrKonv OF StrekKode no-lock,
                   FIRST ArtLag WHERE ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND ArtLag.butik = '&ButNr' AND ArtLag.storl = StrKonv.Storl,
                   FIRST Lager WHERE Lager.ArtikkelNr = StrekKode.ArtikkelNr AND Lager.Butik = ArtLag.butik".     

        IF NUM-ENTRIES(filter) = 2 THEN 
        DO:
            ASSIGN
                pcKode  = REPLACE(ENTRY(1,Filter),'Kode=','')
                pcButNr = REPLACE(ENTRY(2,filter),'ButNr=','') 
                pcWhere = REPLACE(pcWhere,'&Ean',pcKode)
                pcWhere = REPLACE(pcWhere,'&ButNr',pcButNr)
                .
        END.
        ELSE RETURN.
             
        MESSAGE 'Where sats: ' + pcWhere.
             
        /* TODO: Add code to get a set of records and return the
           resulting records to the client. */   
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(ihBuffer1,ihBuffer4,ihBuffer2,ihBuffer3).
        hQuery:QUERY-PREPARE(pcWhere).
        hQuery:QUERY-OPEN().
        hQuery:GET-FIRST().
           
        LOOPEN:
        REPEAT WHILE NOT hQuery:QUERY-OFF-END: 
            FIND StrekKode NO-LOCK WHERE 
                StrekKode.Kode = ihBuffer1:BUFFER-FIELD("Kode"):BUFFER-VALUE NO-ERROR.
            FIND Butiker NO-LOCK WHERE 
                Butiker.Butik = Lager.Butik NO-ERROR.
            FIND ArtBas NO-LOCK WHERE 
                ArtBas.ArtikkelNr = ihBuffer1:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NO-ERROR.
            FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE 
                ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN  
                FIND FIRST artPris OF ArtBas NO-LOCK NO-ERROR.
            FIND StrKonv OF StrekKode NO-LOCK NO-ERROR. 
            FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
            
            IF NOT CAN-FIND(FIRST ttVareLager WHERE 
                            ttVareLager.Kode = ihBuffer1:BUFFER-FIELD("Kode"):BUFFER-VALUE) THEN 
            DO:
                CREATE ttVareLager.
                ASSIGN 
                    ttVareLager.Kode = ihBuffer1:BUFFER-FIELD("Kode"):BUFFER-VALUE
                    ttVareLager.ArtikkelNr = ihBuffer2:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                    ttVareLager.ButikkNr = ihBuffer2:BUFFER-FIELD("Butik"):BUFFER-VALUE
                    ttVareLager.Beskr = IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE ''
                    ttVareLager.LevKod = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
                    ttVareLager.LevFargKod = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
                    ttVareLager.VmId = IF AVAILABLE ArtBas THEN ArtBas.VmId ELSE 0
                    ttVareLager.VmBeskr = IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ''
                    ttVareLager.StrKode = ihBuffer1:BUFFER-FIELD("StrKode"):BUFFER-VALUE
                    ttVareLager.Sasong = IF AVAILABLE ArtBas THEN ArtBas.Sasong ELSE 0
                    ttVareLager.LevNr = IF AVAILABLE ArtBas THEN ArtBas.LevNr ELSE 0
                    ttVareLager.LagAnt = ihBuffer2:BUFFER-FIELD("LagAnt"):BUFFER-VALUE
                    ttVareLager.vVarekost = ihBuffer3:BUFFER-FIELD("vVareKost"):BUFFER-VALUE
                    ttVareLager.EDato = ihBuffer3:BUFFER-FIELD("EDato"):BUFFER-VALUE
                    ttVareLager.Storl = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
                    ttVareLager.SasBeskr = IF AVAILABLE SaSong THEN SaSong.SasBeskr ELSE ''
                    ttVareLager.LevBeskr = IF AVAILABLE LevBas THEN LevBas.levnamn ELSE '' 
                    ttVareLager.Pris = IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0
                    ttVareLager.InnkjopsPris = IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0
                    ttVareLager.Varekost = IF AVAILABLE ArtPris THEN ArtPris.VareKost[1] ELSE 0
                    ttVareLager.EndretDatoTid = IF AVAILABLE ArtLag THEN ArtLag.EndretDatoTid ELSE ttVareLager.EndretDatoTid 
                .
            END.
            
            hQuery:GET-NEXT(). 
        END. /* LOOPEN */      
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.   
    END METHOD.
END CLASS.
