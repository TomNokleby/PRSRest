
 /*------------------------------------------------------------------------
    File        : Bong
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : tomn
    Created     : Mon Aug 01 12:10:11 CEST 2022
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="b_id").
	@openapi.openedge.entity.required (fields="BongNr,ButikkNr,GruppeNr,KasseNr,KassererNr,SelgerNr,TTId").
	
DEFINE TEMP-TABLE ttBongHode BEFORE-TABLE bttBongHode
FIELD BongNr AS INTEGER INITIAL "0" LABEL "Bongnummer"
FIELD ButikkNr AS INTEGER INITIAL "0" LABEL "Butikknummer"
FIELD GruppeNr AS INTEGER INITIAL "0" LABEL "Gruppenummer"
FIELD ODato AS DATE LABEL "OpprettetDato"
FIELD OTid AS INTEGER INITIAL "0" LABEL "Opprettet tid"
FIELD OAv AS CHARACTER LABEL "Opprettet av"
FIELD EDato AS DATE LABEL "Endret dato"
FIELD ETid AS INTEGER INITIAL "0" LABEL "Endret tid"
FIELD EAv AS CHARACTER LABEL "Endret av"
FIELD KasseNr AS INTEGER INITIAL "0" LABEL "Kassenummer"
FIELD KassererNr AS DECIMAL INITIAL "0" LABEL "Kasserernummer"
FIELD SelgerNr AS INTEGER INITIAL "0" LABEL "Selgernummer"
FIELD KundeNr AS DECIMAL INITIAL "0" LABEL "Kundenummer"
FIELD MedlemsNr AS DECIMAL INITIAL "0" LABEL "Medlemsnummer"
FIELD BongStatus AS INTEGER INITIAL "0" LABEL "Status"
FIELD KassererNavn AS CHARACTER LABEL "Kasserernavn"
FIELD SelgerNavn AS CHARACTER LABEL "Selgernavn"
FIELD MedlemNavn AS CHARACTER LABEL "MedlemNavn"
FIELD OverforingsNr AS DECIMAL INITIAL "0" LABEL "Overføringsnummer"
FIELD MedlemsKort AS CHARACTER LABEL "Medlemskort"
FIELD KundeKort AS CHARACTER LABEL "Kundekort"
FIELD DataSettId AS DECIMAL INITIAL "0" LABEL "DatasettId"
FIELD UtskriftsKopi AS CHARACTER LABEL "Utskriftskopi"
FIELD OpdKvit AS LOGICAL INITIAL "no"
FIELD OpdUtskKopi AS LOGICAL INITIAL "no" LABEL "Utskriftskopi"
FIELD Konvertert AS LOGICAL INITIAL "no" LABEL "Konvertert"
FIELD Dato AS DATE LABEL "Dato"
FIELD Tid AS INTEGER INITIAL "0" LABEL "Tid"
FIELD Belop AS DECIMAL INITIAL "0" LABEL "Beløp"
FIELD KundeNavn AS CHARACTER LABEL "Kundenavn"
FIELD Logg AS CHARACTER LABEL "Logg"
FIELD KortType AS INTEGER INITIAL "0" LABEL "Korttype"
FIELD Gradering AS INTEGER INITIAL "0" LABEL "Gradering"
FIELD b_id AS DECIMAL INITIAL "0" LABEL "BongId"
FIELD flBetalingskort AS LOGICAL INITIAL "no" LABEL "Betalingskort"
FIELD flBankkort AS LOGICAL INITIAL "no" LABEL "Bankkort"
FIELD flKreditkort AS LOGICAL INITIAL "no" LABEL "Kreditkort"
FIELD flGavekort AS LOGICAL INITIAL "no" LABEL "Gavekort"
FIELD flSjekk AS LOGICAL INITIAL "no" LABEL "Sjekk"
FIELD flRekvisisasjon AS LOGICAL INITIAL "no" LABEL "Rekvisisasjon"
FIELD flKupong1 AS LOGICAL INITIAL "no" LABEL "Kupong1"
FIELD flSlKort AS INTEGER INITIAL "0"
FIELD flRabatt AS LOGICAL INITIAL "no" LABEL "Rabatt"
FIELD Systemkort AS CHARACTER LABEL "Systemkort"
FIELD flSystemkort AS LOGICAL INITIAL "no" LABEL "Systemkort"
FIELD EksportertDato AS DATE LABEL "Eksportert"
FIELD pfFlagg AS INTEGER INITIAL "1" LABEL "Overført ProfitBase"
FIELD Kampanje AS LOGICAL INITIAL "no" LABEL "Kampanje"
FIELD SkiftNr AS INTEGER INITIAL "0" LABEL "Skiftnr"
FIELD Makulert AS INTEGER INITIAL "0"
FIELD SkiftId AS DECIMAL INITIAL "0"
FIELD KOrdre_Id AS DECIMAL INITIAL "0" LABEL "KOrdre id"
FIELD TTId AS INTEGER INITIAL "0" LABEL "Transaksjonstype"
INDEX Bong IS  UNIQUE  ButikkNr  ASCENDING  GruppeNr  ASCENDING  KasseNr  ASCENDING  Dato  ASCENDING  BongNr  ASCENDING 
INDEX b_id IS  PRIMARY  UNIQUE  b_id  ASCENDING 
. 

DEFINE TEMP-TABLE ttBongLinje BEFORE-TABLE bttBongLinje
FIELD AaaaMmDd               AS CHARACTER   FORMAT "X(8)" LABEL "ÅrMndDag"
FIELD AlternativPrisRabatt   AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
FIELD Antall                 AS DECIMAL     DECIMALS 3 FORMAT "->>>,>>9.999" LABEL "Antall" COLUMN-LABEL "Antall"
FIELD ArtikkelNr             AS CHARACTER   FORMAT "X(20)" LABEL "Artikkelnummer" COLUMN-LABEL "ArtikkelNr"
FIELD BongNr                 AS INTEGER     FORMAT ">>>>>>>>>>>>9" LABEL "Bongnummer" COLUMN-LABEL "BongNr"
FIELD BongPris               AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "BongPris" COLUMN-LABEL "BongPris"
FIELD BongTekst              AS CHARACTER   FORMAT "X(30)" LABEL "Bongtekst" COLUMN-LABEL "Bongtekst"
FIELD ButikkNr               AS INTEGER     FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
FIELD b_id                   AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>9" LABEL "BongId"
FIELD Dato                   AS DATE        LABEL "Dato"
FIELD DivInfo                AS CHARACTER   FORMAT "X(20)" INITIAL "Felt for oppbevaring av infomrasjon som ikke skal inn i bongtekst."
FIELD EAv                    AS CHARACTER   FORMAT "X(15)" LABEL "Endret av" COLUMN-LABEL "EAv"
FIELD EDato                  AS DATE        LABEL "Endret dato" COLUMN-LABEL "EDato"
FIELD ETid                   AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
FIELD FeilKode               AS INTEGER     FORMAT ">9" LABEL "Feilkode" COLUMN-LABEL "FK"
FIELD FeilKodeTekst          AS CHARACTER   FORMAT "X(30)" LABEL "Feilkodetekst"
FIELD ForKonvertering        AS CHARACTER   FORMAT "X(40)"
FIELD GenerellRabatt         AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Generell rabatt"
FIELD GruppeNr               AS INTEGER     FORMAT ">9" LABEL "Gruppenummer" COLUMN-LABEL "GrNr"
FIELD HovedGr                AS INTEGER     FORMAT ">>>9" LABEL "Hovedgruppe" COLUMN-LABEL "Hg"
FIELD HovedGrBeskrivelse     AS CHARACTER   FORMAT "X(30)" LABEL "Hovedgruppe"
FIELD IndividNr              AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Individnr"
FIELD KampanjeId             AS INTEGER     FORMAT ">>>>>>>9" LABEL "Kampanjeid"
FIELD KampEierId             AS INTEGER     FORMAT ">>>>>9" LABEL "Kampanjeeier"
FIELD KampId                 AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>9" LABEL "Kampanjeid"
FIELD KampTilbId             AS INTEGER     FORMAT ">>>>>>>9" LABEL "Kampanjetilbud"
FIELD KasseNr                AS INTEGER     FORMAT ">>9" LABEL "Kassenummer" COLUMN-LABEL "KasseNr"
FIELD Kunderabatt            AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Kunderabatt"
FIELD LevNavn                AS CHARACTER   FORMAT "X(30)" LABEL "Leverandørnavn"
FIELD LevNr                  AS INTEGER     FORMAT ">>>>>9" LABEL "Leverandørnummer"
FIELD LinjeNr                AS INTEGER     FORMAT ">>>>9" LABEL "Linjenummer" COLUMN-LABEL "LinjeNr"
FIELD LinjeRab               AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Linjerabatt"
FIELD LinjerabattPersonal    AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
FIELD LinjeSum               AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>>,>>9.99" LABEL "Linjesum" COLUMN-LABEL "LinSum"
FIELD LopeNr                 AS INTEGER     FORMAT ">>>9" LABEL "Løpenummer" COLUMN-LABEL "LøpeNr"
FIELD Makulert               AS LOGICAL     LABEL "Makulert" COLUMN-LABEL "Mak"
FIELD ManuelEndretPrisRabatt AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
FIELD MButikkNr              AS INTEGER     FORMAT ">>>>>9" LABEL "Mottagende butikk" COLUMN-LABEL "MButNr"
FIELD Medlemsrabatt          AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Medlemsrabatt"
FIELD MixMatchRabatt         AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
FIELD Mva%                   AS DECIMAL     DECIMALS 2 LABEL "MVA%"
FIELD MvaGr                  AS INTEGER     FORMAT ">9" LABEL "Mva gruppe" COLUMN-LABEL "Mva"
FIELD MvaGruppeNavn          AS CHARACTER   FORMAT "X(30)" LABEL "Navn" COLUMN-LABEL "Navn"
FIELD MvaKr                  AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "MvaKr" COLUMN-LABEL "MvaKr"
FIELD Normalpris             AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>>,>>9.99" LABEL "Normalpris" COLUMN-LABEL "Normalpris"
FIELD NotatKode              AS INTEGER     FORMAT ">9" LABEL "Notatkode" COLUMN-LABEL "NK"
FIELD NotatKodeTekst         AS CHARACTER   FORMAT "X(30)" LABEL "Notat"
FIELD OAv                    AS CHARACTER   FORMAT "X(15)" LABEL "Opprettet av" COLUMN-LABEL "OAv"
FIELD ODato                  AS DATE        LABEL "OpprettetDato" COLUMN-LABEL "ODato"
FIELD OrgVareGr              AS INTEGER     FORMAT ">>>>>9" LABEL "Varegruppe" COLUMN-LABEL "Vg"
FIELD OriginalData           AS CHARACTER   FORMAT "X(60)" LABEL "OriginalData"
FIELD OTid                   AS INTEGER     LABEL "Opprettet tid" COLUMN-LABEL "OTid"
FIELD Personalrabatt         AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Personalrabatt"
FIELD PrisPrSalgsenhet       AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>>,>>9.99" LABEL "Pris pr. salgsenhet" COLUMN-LABEL "Pris pr. s.e."
FIELD ProduktType            AS INTEGER     FORMAT "9" INITIAL 1 LABEL "Produkttype"
FIELD RefNr                  AS INTEGER     LABEL "ReferanseNr" COLUMN-LABEL "RefNr"
FIELD RefTekst               AS CHARACTER   FORMAT "X(40)" LABEL "Referansetekst" COLUMN-LABEL "Ref.tekst"
FIELD ReturButikk            AS INTEGER     FORMAT ">>>>>9" LABEL "ReturButikk"
FIELD ReturKasserer          AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "ReturKasserer" COLUMN-LABEL "ReturKasserer"
FIELD ReturKassererNavn      AS CHARACTER   FORMAT "X(30)" LABEL "ReturKasserernavn"
FIELD SalgsType              AS LOGICAL     LABEL "Salgstype"
FIELD SeqNr                  AS INTEGER     FORMAT ">9" LABEL "SeqNr"
FIELD SkiftNr                AS INTEGER     FORMAT ">>>>>9" LABEL "Skiftnr"
FIELD Storrelse              AS CHARACTER   FORMAT "X(4)" LABEL "Størrelse" COLUMN-LABEL "Str"
FIELD Strekkode              AS CHARACTER   FORMAT "X(20)" LABEL "Strekkode" COLUMN-LABEL "Kode"
FIELD SubtotalRab            AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Subtotalrabatt" COLUMN-LABEL "SubRab"
FIELD SubtotalrabattPersonal AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
FIELD TBId                   AS INTEGER     FORMAT ">>9" LABEL "Transaksjonstype beskrivelse"
FIELD Tilbudsrabatt          AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
FIELD TransDato              AS DATE        LABEL "Transaksjonsdato" COLUMN-LABEL "TDato"
FIELD TransNr                AS INTEGER     FORMAT "->>,>>>,>>9" LABEL "TransNr"
FIELD TransTid               AS INTEGER     LABEL "TransaksjonsTid" COLUMN-LABEL "TTid"
FIELD TTId                   AS INTEGER     FORMAT ">>>9" LABEL "Transaksjonstype" COLUMN-LABEL "TTId"
FIELD Type                   AS INTEGER     FORMAT "9"
FIELD VareGr                 AS INTEGER     FORMAT ">>>>>9" LABEL "Varegruppe" COLUMN-LABEL "Vg"
FIELD VareGruppeNavn         AS CHARACTER   FORMAT "X(30)" LABEL "Varegruppenavn" COLUMN-LABEL "VgNavn"
FIELD VVarekost              AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "VVarekost"
INDEX b_id IS  PRIMARY  UNIQUE  b_id  ASCENDING LinjeNr ASCENDING 
.

DEFINE DATASET dsBongHode FOR 
  ttBongHode,
  ttBongLinje
  DATA-RELATION drBong FOR ttBongHode, ttBongLinje RELATION-FIELDS (b_id, b_id) NESTED
  .