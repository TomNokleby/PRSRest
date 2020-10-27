 
 /*------------------------------------------------------------------------
    File        : TelleHode
    Syntax      : 
    Author(s)   : tny
    Created     : Thu Aug 09 10:37:07 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="TelleHode.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="TelleHode", URI="/TelleHode", schemaName="dsTelleHode", schemaFile="PRSRest/AppServer/bo/tellehode.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bo.TelleHode INHERITS BusinessEntity:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"bo\tellehode.i"}
    
    DEFINE DATA-SOURCE srcTelleHode FOR SkoTex.TelleHode.
    
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TelleHode():
        
        DEFINE VARIABLE hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VARIABLE cSkipListArray AS CHARACTER NO-UNDO EXTENT 1.
        
        SUPER (DATASET dsTelleHode:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
            in DataSet */     
         hDataSourceArray[1] =  DATA-SOURCE srcTelleHode:HANDLE.
         
                  
         /* Skip-list entry for each table in dataset. Should be in temp-table order 
           as defined in DataSet */
        /* Each skip-list entry is a comma-separated list of field names, to be
            ignored in create stmt */
             
         cSkipListArray[1] = "".
         
         
         THIS-OBJECT:ProDataSource = hDataSourceArray.
        THIS-OBJECT:SkipList = cSkipListArray.
                 
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadTelleHode(
            INPUT filter AS CHARACTER, 
            OUTPUT DATASET dsTelleHode):
        
        SUPER:ReadData(filter).
              
    END METHOD.
      
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreateTelleHode(INPUT-OUTPUT DATASET dsTelleHode):            
               
        SUPER:CreateData(DATASET dsTelleHode BY-REFERENCE).        
    END METHOD.    
    
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdateTelleHode(INPUT-OUTPUT DATASET dsTelleHode):            
    
        SUPER:UpdateData(DATASET dsTelleHode BY-REFERENCE).          
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeleteTelleHode(INPUT-OUTPUT DATASET dsTelleHode):                    
    
        SUPER:DeleteData(DATASET dsTelleHode BY-REFERENCE).
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitTelleHode", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitTelleHode(INPUT-OUTPUT DATASET dsTelleHode):                    
    
          SUPER:Submit(DATASET dsTelleHode BY-REFERENCE).           
    END METHOD.
     
    
END CLASS.
