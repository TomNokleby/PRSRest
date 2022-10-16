TRIGGER PROCEDURE FOR CREATE OF BongHode.

ASSIGN 
  BongHode.ODato = TODAY
  BongHode.OTid  = TIME
  BongHode.OAv   = USERID("REST") NO-ERROR 
  .
  



