
 /*------------------------------------------------------------------------
    File        : Lager
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tny
    Created     : Wed Aug 01 11:15:23 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="ArtikkelNr,Butik").
	
DEFINE TEMP-TABLE ttLager SERIALIZE-NAME 'ttLager' BEFORE-TABLE bttLager
FIELD EDato AS DATE LABEL "Endret"
FIELD ETid AS INTEGER INITIAL "0" LABEL "Endret tid"
FIELD BrukerID AS CHARACTER LABEL "Bruker"
FIELD RegistrertDato AS DATE LABEL "Registrert dato"
FIELD RegistrertTid AS INTEGER INITIAL "0" LABEL "Registreringstidspunkt"
FIELD RegistrertAv AS CHARACTER LABEL "Registrert av"
FIELD ArtikkelNr AS DECIMAL INITIAL "0" LABEL "Artikkelnummer"
FIELD VVarekost AS DECIMAL INITIAL "0" LABEL "Vektet varekost"
FIELD LagAnt AS DECIMAL INITIAL "0" LABEL "Antall på lager"
FIELD SistInnlevert AS DATE LABEL "Sist innlevert"
FIELD Butik AS INTEGER INITIAL "0" LABEL "Butikknummer"
FIELD AntSolgt AS DECIMAL INITIAL "0" LABEL "Antall  solgt"
FIELD BrekkAnt AS DECIMAL INITIAL "0" LABEL "Brekkasje"
FIELD IntAnt AS DECIMAL INITIAL "0" LABEL "Internt forbruk"
FIELD ReklAnt AS DECIMAL INITIAL "0" LABEL "Kundereklamasjoner"
FIELD ReklLAnt AS DECIMAL INITIAL "0" LABEL "Rekl.lev.antall"
FIELD GjenkjopAnt AS DECIMAL INITIAL "0" LABEL "Gjenkjøp fra kunde"
FIELD RetLAnt AS DECIMAL INITIAL "0" LABEL "Retur leverandør"
FIELD KjopAnt AS DECIMAL INITIAL "0" LABEL "Innkjopt antall"
FIELD OvAnt AS DECIMAL INITIAL "0" LABEL "Overført antall"
FIELD JustAnt AS DECIMAL INITIAL "0" LABEL "Justert antall"
FIELD JustVerdi AS DECIMAL INITIAL "0" LABEL "Justert verdi"
FIELD SvinnAnt AS DECIMAL INITIAL "0" LABEL "Antall svinn"
FIELD SvinnVerdi AS DECIMAL INITIAL "0" LABEL "Svinn verdi"
FIELD NedAnt AS DECIMAL INITIAL "0" LABEL "Nedskrevet antall"
FIELD NedVerdi AS DECIMAL INITIAL "0" LABEL "Verdi nedskrevet"
FIELD VerdiSolgt AS DECIMAL INITIAL "0" LABEL "Verdi solgt"
FIELD KjopVerdi AS DECIMAL INITIAL "0" LABEL "Verdi kjøpt"
FIELD BrekkVerdi AS DECIMAL INITIAL "0" LABEL "Verdi av brekasje"
FIELD IntVerdi AS DECIMAL INITIAL "0" LABEL "Verdi av internt forbruk"
FIELD ReklVerdi AS DECIMAL INITIAL "0" LABEL "Verdi kundereklamasjoner"
FIELD ReklLVerdi AS DECIMAL INITIAL "0" LABEL "Verdi av leveerandørreklamasjoner"
FIELD GjenkjopVerdi AS DECIMAL INITIAL "0" LABEL "Verdi av gjenkjøpte varer"
FIELD OvVerdi AS DECIMAL INITIAL "0" LABEL "Verdi av overførte varer"
FIELD VerdiRabatt AS DECIMAL INITIAL "0" LABEL "Verdi rabatt"
FIELD AntRab AS DECIMAL INITIAL "0" LABEL "Antall  rabatt"
FIELD SVK AS DECIMAL INITIAL "0" LABEL "SVK"
INDEX Butikk  Butik  ASCENDING  ArtikkelNr  ASCENDING 
INDEX Lager IS  PRIMARY  UNIQUE  ArtikkelNr  ASCENDING  Butik  ASCENDING . 
