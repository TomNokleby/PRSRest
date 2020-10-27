 
 /*------------------------------------------------------------------------
    File        : Lager
    Syntax      : 
    Author(s)   : tny
    Created     : Wed Aug 01 11:15:23 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="Lager.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="Lager", URI="/Lager", schemaName="dsLager", schemaFile="PRSRest/AppServer/bo/lager.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bo.Lager INHERITS BusinessEntity:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"bo\ttlager.i"}
    {"bo\dslager.i"}
    
    DEFINE DATA-SOURCE srcLager FOR Lager.
    
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Lager():
        
        DEFINE VARIABLE hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VARIABLE cSkipListArray AS CHARACTER NO-UNDO EXTENT 1.
        
        SUPER (DATASET dsLager:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
            in DataSet */     
         hDataSourceArray[1] =  DATA-SOURCE srcLager:HANDLE.
         
                  
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
    METHOD PUBLIC VOID ReadLager(
            INPUT filter AS CHARACTER, 
            OUTPUT DATASET dsLager):
        
        SUPER:ReadData(filter).
              
    END METHOD.
      
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreateLager(INPUT-OUTPUT DATASET dsLager):            
               
        SUPER:CreateData(DATASET dsLager BY-REFERENCE).        
    END METHOD.    
    
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdateLager(INPUT-OUTPUT DATASET dsLager):            
    
        SUPER:UpdateData(DATASET dsLager BY-REFERENCE).          
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeleteLager(INPUT-OUTPUT DATASET dsLager):                    
    
        SUPER:DeleteData(DATASET dsLager BY-REFERENCE).
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitLager", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitLager(INPUT-OUTPUT DATASET dsLager):                    
    
          SUPER:Submit(DATASET dsLager BY-REFERENCE).           
    END METHOD.
     
    
END CLASS.
