 
 /*------------------------------------------------------------------------
    File        : Tellelinje
    Syntax      : 
    Author(s)   : tny
    Created     : Thu Aug 09 10:42:43 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="Tellelinje.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="Tellelinje", URI="/Tellelinje", schemaName="dsTelleLinje", schemaFile="PRSRest/AppServer/bo/tellelinje.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bo.Tellelinje INHERITS BusinessEntity:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
    {"bo\tellelinje.i"}
    
    DEFINE DATA-SOURCE srcTelleLinje FOR SkoTex.TelleLinje.
    
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Tellelinje():
        
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
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadTellelinje(
            INPUT filter AS CHARACTER, 
            OUTPUT DATASET dsTelleLinje):
        
        SUPER:ReadData(filter).
              
    END METHOD.
      
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreateTellelinje(INPUT-OUTPUT DATASET dsTelleLinje):            
               
        SUPER:CreateData(DATASET dsTelleLinje BY-REFERENCE).        
    END METHOD.    
    
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdateTellelinje(INPUT-OUTPUT DATASET dsTelleLinje):            
    
        SUPER:UpdateData(DATASET dsTelleLinje BY-REFERENCE).          
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeleteTellelinje(INPUT-OUTPUT DATASET dsTelleLinje):                    
    
        SUPER:DeleteData(DATASET dsTelleLinje BY-REFERENCE).
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitTellelinje", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitTellelinje(INPUT-OUTPUT DATASET dsTelleLinje):                    
    
          SUPER:Submit(DATASET dsTelleLinje BY-REFERENCE).           
    END METHOD.
     
    
END CLASS.
