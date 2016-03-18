LIBNAME OUT 'C:\Projetos\RMs Proc. 03605.001403-2013-88 - FJP Maria Luiza Marques\Maceio\Habitacao\2010';
LIBNAME IN 'C:\Projetos\RMs Proc. 03605.001403-2013-88 - FJP Maria Luiza Marques\Base dom e pes 2010';

 OPTIONS PS=10000 NODATE LS=250 OBS=MAX;

DATA IBGE(DROP=KEEP=V4001 V0502 V1006 V0204 V0209 V0205 V0207 V0210 V0211 V0202 V0401 V0010 SETORC);
SET IN.pesdom10;


IF V0001='27';*AL;


 SETORC=V0000;



DATA HABIT;
 SET IBGE;



****************;
* PESOP - PESO *;
****************;
PESOPES=V0010;


*** CONTADOR ***;
CONTADOR=1;


*** EXCLUINDO PENSIONISTA, EMPREGADO DOMESTICO E PARENTES DO EMPREGADO ***;

IF 1<=V0502<=16;


*** EXCLUINDO OS DOMICILIOS COLETIVOS ***;
IF V4001=1 OR V4001=2;


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
     WHEN (1<=V0210<=2) LIXO=1;
     WHEN (3<=V0210<=7) LIXO=0;
    OTHERWISE           LIXO=.;
    END;
    END;
   WHEN (URBANO=0) DO;
    SELECT;
     WHEN (1<=V0210<=2) LIXO=.;
     WHEN (3<=V0210<=7) LIXO=.;
    OTHERWISE           LIXO=.;
    END;
    END;
   OTHERWISE            LIXO=.;
  END;


*****************************;
* ESGOTO SANITARIO ADEQUADO *;
*****************************;
  SELECT;
    WHEN  (1<=V0207<=2)  ESGOTO=1;
    WHEN  (V0207>=3)     ESGOTO=0;
  OTHERWISE              ESGOTO=.;
  END;


*****************;
* AGUA ENCANADA *;
*****************;
  SELECT;
    WHEN  (V0209=1)         AGUA=1;
    WHEN  (2<=V0209<=3)     AGUA=0;
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
      WHEN (1<=V0205<=9)   BANAGUA=1;
      WHEN (V0205=0)       BANAGUA=0;
     OTHERWISE             BANAGUA=.;
     END;END;
    WHEN (AGUA=0) DO;
     SELECT;
      WHEN (1<=V0205<=9)   BANAGUA=.;
      WHEN (V0205=0)       BANAGUA=.;
      OTHERWISE            BANAGUA=.;
      END;END;
  OTHERWISE                BANAGUA=.;
  END;



********************;
* ENERGIA ELETRICA *;
********************;
SELECT;
  WHEN (V0211=1 OR V0211=2)    LUZ=1;
  WHEN (V0211=3)               LUZ=0;
  OTHERWISE                    LUZ=.;
END;



***********************;
* SEM PAREDES OU      *;
* PAREDES INADEQUADAS *;
***********************;
SELECT;
  WHEN (4<=V0202<=9)    PAREDE=1;
  OTHERWISE             PAREDE=0;
END;


*************;
* DENSIDADE *;
*************;
SELECT;
   WHEN (V0401>(2*V0204))     DENS=1;
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
VAR CONTADOR AGUA_ESGOTO AGUA BANAGUA LIXO LUZ PAREDE DENS;
OUTPUT OUT=SAIDA2(KEEP=SETORC POP AGUA_ESGOTO AGUA BANAGUA LIXO LUZ PAREDE DENS)
       SUM=POP AGUA_ESGOTO AGUA BANAGUA LIXO LUZ PAREDE DENS;
WEIGHT PESOPES;
BY SETORC;



DATA FINALHAB;
MERGE SAIDA1 SAIDA2;
BY SETORC;



DATA OUT.HABIT10;
SET FINALHAB;



RUN;
