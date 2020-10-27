DEFINE VARIABLE rPricatReg AS CLASS Vpi.PricatReg NO-UNDO.

rPricatReg = NEW Vpi.PricatReg().


FOR EACH VPIDatasett WHERE 
    EkstVPILEvNr >= 10000 AND 
    EkstVPILEvNr <= 11999:

MESSAGE 'EkstVPILEvNr:' EkstVPILEvNr
VIEW-AS ALERT-BOX.
    rPricatReg:eksporterVpiLev( EkstVPILEvNr ).
END.




