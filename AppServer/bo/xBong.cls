 
 /*------------------------------------------------------------------------
    File        : Bong
    Syntax      : 
    Author(s)   : tomn
    Created     : Mon Aug 01 12:10:11 CEST 2022
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="Bong.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="Bong", URI="/BongService", schemaName="dsBongHode", schemaFile="PRSRest/AppServer/bo/bong.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bo.Bong INHERITS BusinessEntity:
  /*------------------------------------------------------------------------------
      Purpose:                                                                      
      Notes:                                                                        
  ------------------------------------------------------------------------------*/
  
  {"bo\bong.i"}
  
  DEFINE DATA-SOURCE srcBongHode FOR Data.BongHode.
  DEFINE DATA-SOURCE srcBongLinje FOR Data.BongLinje.
  
  /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Bong():
      
      DEFINE VARIABLE hDataSourceArray AS HANDLE NO-UNDO EXTENT 2.
        DEFINE VARIABLE cSkipListArray AS CHARACTER NO-UNDO EXTENT 2.
        
        SUPER (DATASET dsBongHode:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
          in DataSet */     
       hDataSourceArray[1] =  DATA-SOURCE srcBongHode:HANDLE.
       hDataSourceArray[2] =  DATA-SOURCE srcBongLinje:HANDLE.
       
              
       /* Skip-list entry for each table in dataset. Should be in temp-table order 
           as defined in DataSet */
        /* Each skip-list entry is a comma-separated list of field names, to be
          ignored in create stmt */
           
       cSkipListArray[1] = "".
       cSkipListArray[2] = "".
       
       
       THIS-OBJECT:ProDataSource = hDataSourceArray.
        THIS-OBJECT:SkipList = cSkipListArray.
               
    END CONSTRUCTOR.
      
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadBong(
        INPUT filter AS CHARACTER, 
        OUTPUT DATASET dsBongHode):
      
      SUPER:ReadData(filter).
            
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreateBong(INPUT-OUTPUT DATASET dsBongHode):        
             
        SUPER:CreateData(DATASET dsBongHode BY-REFERENCE).        
    END METHOD.    
  
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdateBong(INPUT-OUTPUT DATASET dsBongHode):        
  
        SUPER:UpdateData(DATASET dsBongHode BY-REFERENCE).          
    END METHOD.
  
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeleteBong(INPUT-OUTPUT DATASET dsBongHode):                
  
        SUPER:DeleteData(DATASET dsBongHode BY-REFERENCE).
    END METHOD.
   
    
  DESTRUCTOR PUBLIC Bong ( ):

  END DESTRUCTOR.

END CLASS.
