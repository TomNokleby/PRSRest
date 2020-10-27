 
 /*------------------------------------------------------------------------
    File        : LagerEndret
    Syntax      : 
    Author(s)   : tny
    Created     : Wed Aug 08 16:01:47 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="LagerEndret.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="LagerEndret", URI="/LagerEndret", schemaName="ttLagerEndret", schemaFile="PRSRest/AppServer/rfid/ttLagerEndret.i").

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS rfid.LagerEndret:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"rfid\ttLagerEndret.i"}

    DEFINE VARIABLE ihBuffer1 AS HANDLE NO-UNDO.
    DEFINE VARIABLE ihBuffer2 AS HANDLE NO-UNDO.
    DEFINE VARIABLE ihBuffer3 AS HANDLE NO-UNDO.
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
        
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadLagerEndret(
            INPUT filter AS CHARACTER, 
            OUTPUT TABLE ttLagerEndret):
        
        DEFINE VARIABLE pcWhere AS CHARACTER.
        DEFINE VARIABLE pcEndretDatoTid AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcButNr AS CHARACTER NO-UNDO.
        
        EMPTY TEMP-TABLE ttLagerEndret.

        ihBuffer1 = BUFFER ArtLag:HANDLE.    
        ihBuffer2 = BUFFER Lager:HANDLE.
        ihBuffer3 = BUFFER Strekkode:HANDLE.    

        filter = TRIM(filter,'"').
        
        MESSAGE 'Filter fra Gurre2.: ' filter.
        
/*        pcWhere = "FOR EACH ArtLag WHERE EndretDatoTid >= '&EndretDatoTid' AND ArtLag.butik = '&ButNr',                          */
/*                   FIRST Lager no-lock where Lager.ArtikkelNr = ArtLag.ArtikkelNr and Lager.Butik = ArtLag.butik,                */
/*                   EACH Strekkode no-lock where Strekkode.ArtikkelNr = ArtLag.ArtikkelNr and Strekkode.StrKode = ArtLag.StrKode".*/
        pcWhere = "FOR EACH ArtLag WHERE EndretDatoTid >= '&EndretDatoTid' AND ArtLag.butik = '&ButNr',
                   FIRST Lager no-lock where Lager.ArtikkelNr = ArtLag.ArtikkelNr and Lager.Butik = ArtLag.butik,
                   LAST Strekkode no-lock where Strekkode.ArtikkelNr = ArtLag.ArtikkelNr and Strekkode.StrKode = ArtLag.StrKode USE-INDEX Str_Lib".     
        
        IF NUM-ENTRIES(filter) = 2 THEN 
        DO:
            ASSIGN
                pcEndretDatoTid  = REPLACE(ENTRY(1,Filter),'EndretDato=','')
                pcButNr = REPLACE(ENTRY(2,filter),'ButNr=','') 
                pcWhere = REPLACE(pcWhere,'&EndretDatoTid',pcEndretDatoTid)
                pcWhere = REPLACE(pcWhere,'&ButNr',pcButNr)
                .
        END.
        ELSE RETURN.
             
        MESSAGE 'Where sats: ' + pcWhere.
             
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(ihBuffer1,ihBuffer2,ihBuffer3).
        hQuery:QUERY-PREPARE(pcWhere).
        hQuery:QUERY-OPEN().
        hQuery:GET-FIRST().
           
        LOOPEN:
        REPEAT WHILE NOT hQuery:QUERY-OFF-END: 
/*            FIND StrekKode NO-LOCK WHERE                                              */
/*                StrekKode.Kode = ihBuffer3:BUFFER-FIELD("Kode"):BUFFER-VALUE NO-ERROR.*/
            
            IF NOT CAN-FIND(FIRST ttLagerEndret WHERE 
                            ttLagerEndret.Kode = ihBuffer3:BUFFER-FIELD("Kode"):BUFFER-VALUE) THEN 
            DO:
                CREATE ttLagerEndret.
                ASSIGN 
                    ttLagerEndret.Kode = ihBuffer3:BUFFER-FIELD("Kode"):BUFFER-VALUE
                    ttLagerEndret.ArtikkelNr = ihBuffer1:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                    ttLagerEndret.ButikkNr = ihBuffer1:BUFFER-FIELD("Butik"):BUFFER-VALUE
                    ttLagerEndret.StrKode = ihBuffer1:BUFFER-FIELD("StrKode"):BUFFER-VALUE
                    ttLagerEndret.Storl = ihBuffer1:BUFFER-FIELD("Storl"):BUFFER-VALUE
                    ttLagerEndret.LagAnt = ihBuffer1:BUFFER-FIELD("LagAnt"):BUFFER-VALUE
                    ttLagerEndret.vVarekost = ihBuffer2:BUFFER-FIELD("vVareKost"):BUFFER-VALUE
                    ttLagerEndret.EndretDato = ihBuffer1:BUFFER-FIELD("EndretDatoTid"):BUFFER-VALUE
                .
            END.
            
            hQuery:GET-NEXT(). 
        END. /* LOOPEN */      
    END METHOD.
    
    
    
     
    
END CLASS.
