DEF VAR cFilter AS CHAR NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rLagerEndret AS rfid.LagerEndret NO-UNDO.
    
rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner(  ).
rLagerEndret  = NEW rfid.LagerEndret().

{rfid\ttLagerEndret.i}

ASSIGN 
    cFilter = 'EndretDato=01/01/2019,ButNr=16'
    .

rLagerEndret:ReadLagerEndret(INPUT cfilter, OUTPUT TABLE ttLagerEndret).

TEMP-TABLE ttLagerEndret:WRITE-JSON ("file", 'konv\tmpttLagerEndret' + 
                                     REPLACE(STRING(TODAY),'/','') +  
                                     '_ ' + 
                                     REPLACE(STRING(TIME,"HH:MM:SS"),':','') + 
                                     '.JSon',TRUE).
