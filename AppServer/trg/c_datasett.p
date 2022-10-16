TRIGGER PROCEDURE FOR CREATE OF Datasett.

DEF VAR trgDataSettId AS DEC NO-UNDO.

DEF BUFFER trgDataSett FOR Data.DataSett.

LOOPEN:
DO WHILE TRUE:
  trgDataSettId = dec(SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
                    STRING(MONTH(TODAY),"99") + 
                    string(NEXT-VALUE(DataSettId,Data),"99999999")).
  IF NOT CAN-FIND(FIRST trgDataSett WHERE 
                  trgDataSett.DataSettId = trgDataSettId) THEN
    DO:
      ASSIGN
        Data.DataSett.DataSettId  = trgDataSettId
        no-error.

      IF ERROR-STATUS:ERROR = TRUE
        THEN NEXT LOOPEN.
      ELSE 
        LEAVE LOOPEN.
    END. 
END. /* LOOPEN */


