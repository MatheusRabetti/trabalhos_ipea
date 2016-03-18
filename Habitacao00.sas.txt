LIBNAME OUT 'C:\Projetos\Projeto a ser autorizado\Programas\Habitacao\2000';
LIBNAME IN 'C:\Projetos\Projeto a ser autorizado\Programas\Base dom e pes 00';

 OPTIONS PS=10000 NODATE LS=250 OBS=MAX;

DATA IBGE(DROP=KEEP=V0201 V0402 V1006 V0204 V0208 V0209 V0211 V0212 V0213 V0203 V7100 P001 SETORC);
SET IN.pesdom00;


 ********************;
 * VARIAVEIS DE     *;
 * SETOR CENSITARIO *;
 ********************;

UF=PUT(V0102,$2.);
DIST=PUT(V0104,$2.);
MUNIC=PUT(V0103,$4.);
SUBDIST=PUT(V0105,$2.);
SETOR=PUT(V0106,$4.);
QUEST=PUT(V0101,$4.);

 SETORC=UF!!MUNIC!!DIST!!SUBDIST!!SETOR;

 N_DOM=UF!!MUNIC!!DIST!!SUBDIST!!SETOR!!QUEST;


DATA HABIT;
 SET IBGE;



****************;
* PESOP - PESO *;
****************;
PESOPES=P001;

*** CONTADOR ***;
CONTADOR=1;

*** EXCLUINDO PENSIONISTA, EMPREGADO DOMESTICO E PARENTES DO EMPREGADO ***;

IF V0402<=8;


*** EXCLUINDO OS DOMICILIOS COLETIVOS ***;
IF V0201=1;


****************************************;
* SITUACAO DO DOMICILIO=URBANO E RURAL *;
****************************************;
  SELECT;
    WHEN (V1006=1)     URBANO=1;
    WHEN (V1006=2)     URBANO=0;
  OTHERWISE            URBANO=.;
  END;


**************************;
* COLETA REGULAR DE LIXO *;
**************************;

  SELECT;
   WHEN (URBANO=1) DO;
    SELECT;
     WHEN (1<=V0212<=2) LIXO=1;
     WHEN (3<=V0212<=7) LIXO=0;
    OTHERWISE           LIXO=.;
    END;
    END;
   WHEN (URBANO=0) DO;
    SELECT;
     WHEN (1<=V0212<=2) LIXO=.;
     WHEN (3<=V0212<=7) LIXO=.;
    OTHERWISE           LIXO=.;
    END;
    END;
   OTHERWISE            LIXO=.;
  END;


*****************************;
* ESGOTO SANITARIO ADEQUADO *;
*****************************;
  SELECT;
    WHEN  (1<=V0211<=2)  ESGOTO=1;
    WHEN  (V0211>=3)     ESGOTO=0;
  OTHERWISE              ESGOTO=.;
  END;


*****************;
* AGUA ENCANADA *;
*****************;
  SELECT;
    WHEN  (V0208=1)         AGUA=1;
    WHEN  (2<=V0208<=3)     AGUA=0;
  OTHERWISE                 AGUA=.;
  END;


  SELECT;
    WHEN  (AGUA=0 AND ESGOTO=0)     AGUA_ESGOTO=1;
  OTHERWISE                         AGUA_ESGOTO=0;
  END;


**********************************************;
* DOMICILIO C/BANHEIRO E AGUA CANAL. INTERNA *;
* VARIAVEL                                   *;
* ABASTEC=ABAST. DE AGUA                     *;
* QTDBANH=BANHEIRO                           *;
**********************************************;
  SELECT;
    WHEN (AGUA=1) DO;
     SELECT;
      WHEN (1<=V0209<=9)   BANAGUA=1;
      WHEN (V0209=0)       BANAGUA=0;
     OTHERWISE             BANAGUA=.;
     END;END;
    WHEN (AGUA=0) DO;
     SELECT;
      WHEN (1<=V0209<=9)   BANAGUA=.;
      WHEN (V0209=0)       BANAGUA=.;
      OTHERWISE            BANAGUA=.;
      END;END;
  OTHERWISE                BANAGUA=.;
  END;



********************;
* ENERGIA ELETRICA *;
********************;
SELECT;
  WHEN (V0213=1)    LUZ=1;
  WHEN (V0213=2)    LUZ=0;
  OTHERWISE         LUZ=.;
END;


*************;
* DENSIDADE *;
*************;
SELECT;
   WHEN (V7100>(2*V0204))     DENS=1;
OTHERWISE                     DENS=0;
END;



PROC SORT DATA=HABIT;
BY SETORC;



PROC MEANS DATA=HABIT SUMWGT NOPRINT;
WHERE URBANO=1;
VAR CONTADOR;
OUTPUT OUT=SAIDA1(KEEP=SETORC POPURB)
            SUMWGT=POPURB;
WEIGHT PESOPES;
BY SETORC;


PROC MEANS DATA=HABIT SUM NOPRINT;
VAR CONTADOR AGUA_ESGOTO AGUA BANAGUA LIXO LUZ DENS;
OUTPUT OUT=SAIDA2(KEEP=SETORC POP AGUA_ESGOTO AGUA BANAGUA LIXO LUZ DENS)
       SUM=POP AGUA_ESGOTO AGUA BANAGUA LIXO LUZ DENS;
WEIGHT PESOPES;
BY SETORC;



DATA FINALHAB;
MERGE SAIDA1 SAIDA2;
BY SETORC;



DATA OUT.HABIT00;
SET FINALHAB;



RUN;
