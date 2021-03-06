LIBNAME OUT 'C:\Projetos\RMs Proc. 03605.001403-2013-88 - FJP Maria Luiza Marques\Maceio\Trabalho\2010';
LIBNAME IN 'C:\Projetos\RMs Proc. 03605.001403-2013-88 - FJP Maria Luiza Marques\Base dom e pes 2010';


DATA IBGE;
SET IN.pesdom10;


IF V0001='27';*AL;



 SETORC=V0000;



DATA PES;
SET IBGE;


PESOPES=V0010;


********* tirando empregada doméstica e pensionista etc **********;
IF 1<=V0502<=16;

****** selecionando os dom particulares permanentes ****;
IF V4001=1 OR V4001=2;



*******;
* PIA *;
*******;

PIA=1;


********;
* OCUP *;
********;

SELECT;
 WHEN (V0641=1 OR V0642=1 OR V0643=1 OR V0644=1) OCUP=1;
 OTHERWISE                                       OCUP=0;
END;


*******;
* DES *;
*******;

SELECT;
 WHEN (V0641=2 AND V0642=2 AND V0643=2 AND V0644=2 AND V0654=1) DES=1;
 OTHERWISE                                                      DES=0;
END;


*******;
* PEA *;
*******;

SELECT;
 WHEN (OCUP=1 OR DES=1) PEA=1;
 OTHERWISE              PEA=0;
END;


**********CALCULANDO OS OCUPADOS E DESOCUPADOS - 10 A 14 ANOS ************;

 SELECT;
   WHEN (10<=V6036<=14) DO;
     SELECT;
       WHEN (PIA=1)  PIA1014=1;
       WHEN (PIA=0)  PIA1014=0;
       OTHERWISE     PIA1014=.;
     END;END;
   OTHERWISE         PIA1014=.;
 END;

 SELECT;
   WHEN (10<=V6036<=14) DO;
     SELECT;
       WHEN (OCUP=1)  OCUP1014=1;
       WHEN (OCUP=0)  OCUP1014=0;
       OTHERWISE      OCUP1014=.;
     END;END;
   OTHERWISE          OCUP1014=.;
 END;

  SELECT;
   WHEN (10<=V6036<=14) DO;
     SELECT;
       WHEN (DES=1)  DES1014=1;
       WHEN (DES=0)  DES1014=0;
       OTHERWISE     DES1014=.;
     END;END;
   OTHERWISE         DES1014=.;
 END;

  SELECT;
   WHEN (10<=V6036<=14) DO;
     SELECT;
       WHEN (PEA=1)  PEA1014=1;
       WHEN (PEA=0)  PEA1014=0;
       OTHERWISE     PEA1014=.;
     END;END;
   OTHERWISE         PEA1014=.;
 END;


**********CALCULANDO OS OCUPADOS E DESOCUPADOS - 15 A 17 ANOS ************;

 SELECT;
   WHEN (15<=V6036<=17) DO;
     SELECT;
       WHEN (PIA=1)  PIA1517=1;
       WHEN (PIA=0)  PIA1517=0;
       OTHERWISE     PIA1517=.;
     END;END;
   OTHERWISE         PIA1517=.;
 END;

 SELECT;
   WHEN (15<=V6036<=17) DO;
     SELECT;
       WHEN (OCUP=1)  OCUP1517=1;
       WHEN (OCUP=0)  OCUP1517=0;
       OTHERWISE      OCUP1517=.;
     END;END;
   OTHERWISE          OCUP1517=.;
 END;

  SELECT;
   WHEN (15<=V6036<=17) DO;
     SELECT;
       WHEN (DES=1)  DES1517=1;
       WHEN (DES=0)  DES1517=0;
       OTHERWISE     DES1517=.;
     END;END;
   OTHERWISE         DES1517=.;
 END;

  SELECT;
   WHEN (15<=V6036<=17) DO;
     SELECT;
       WHEN (PEA=1)  PEA1517=1;
       WHEN (PEA=0)  PEA1517=0;
       OTHERWISE     PEA1517=.;
     END;END;
   OTHERWISE         PEA1517=.;
 END;


 **********CALCULANDO OS OCUPADOS E DESOCUPADOS - 18 A 24 ANOS ************;

 SELECT;
   WHEN (18<=V6036<=24) DO;
     SELECT;
       WHEN (PIA=1)  PIA1824=1;
       WHEN (PIA=0)  PIA1824=0;
       OTHERWISE     PIA1824=.;
     END;END;
   OTHERWISE         PIA1824=.;
 END;

 SELECT;
   WHEN (18<=V6036<=24) DO;
     SELECT;
       WHEN (OCUP=1)  OCUP1824=1;
       WHEN (OCUP=0)  OCUP1824=0;
       OTHERWISE      OCUP1824=.;
     END;END;
   OTHERWISE          OCUP1824=.;
 END;

  SELECT;
   WHEN (18<=V6036<=24) DO;
     SELECT;
       WHEN (DES=1)  DES1824=1;
       WHEN (DES=0)  DES1824=0;
       OTHERWISE     DES1824=.;
     END;END;
   OTHERWISE         DES1824=.;
 END;

  SELECT;
   WHEN (18<=V6036<=24) DO;
     SELECT;
       WHEN (PEA=1)  PEA1824=1;
       WHEN (PEA=0)  PEA1824=0;
       OTHERWISE     PEA1824=.;
     END;END;
   OTHERWISE         PEA1824=.;
 END;


 **********CALCULANDO OS OCUPADOS E DESOCUPADOS - 25 A 29 ANOS ************;

 SELECT;
   WHEN (25<=V6036<=29) DO;
     SELECT;
       WHEN (PIA=1)  PIA2529=1;
       WHEN (PIA=0)  PIA2529=0;
       OTHERWISE     PIA2529=.;
     END;END;
   OTHERWISE         PIA2529=.;
 END;

 SELECT;
   WHEN (25<=V6036<=29) DO;
     SELECT;
       WHEN (OCUP=1)  OCUP2529=1;
       WHEN (OCUP=0)  OCUP2529=0;
       OTHERWISE      OCUP2529=.;
     END;END;
   OTHERWISE          OCUP2529=.;
 END;

  SELECT;
   WHEN (25<=V6036<=29) DO;
     SELECT;
       WHEN (DES=1)  DES2529=1;
       WHEN (DES=0)  DES2529=0;
       OTHERWISE     DES2529=.;
     END;END;
   OTHERWISE         DES2529=.;
 END;

  SELECT;
   WHEN (25<=V6036<=29) DO;
     SELECT;
       WHEN (PEA=1)  PEA2529=1;
       WHEN (PEA=0)  PEA2529=0;
       OTHERWISE     PEA2529=.;
     END;END;
   OTHERWISE         PEA2529=.;
 END;


 **********CALCULANDO OS OCUPADOS E DESOCUPADOS - 18 ANOS OU MAIS ************;

 SELECT;
   WHEN (V6036>=18) DO;
     SELECT;
       WHEN (PIA=1)  PIA18M=1;
       WHEN (PIA=0)  PIA18M=0;
       OTHERWISE     PIA18M=.;
     END;END;
   OTHERWISE         PIA18M=.;
 END;

 SELECT;
   WHEN (V6036>=18) DO;
     SELECT;
       WHEN (OCUP=1)  OCUP18M=1;
       WHEN (OCUP=0)  OCUP18M=0;
       OTHERWISE      OCUP18M=.;
     END;END;
   OTHERWISE          OCUP18M=.;
 END;

  SELECT;
   WHEN (V6036>=18) DO;
     SELECT;
       WHEN (DES=1)  DES18M=1;
       WHEN (DES=0)  DES18M=0;
       OTHERWISE     DES18M=.;
     END;END;
   OTHERWISE         DES18M=.;
 END;

  SELECT;
   WHEN (V6036>=18) DO;
     SELECT;
       WHEN (PEA=1)  PEA18M=1;
       WHEN (PEA=0)  PEA18M=0;
       OTHERWISE     PEA18M=.;
     END;END;
   OTHERWISE         PEA18M=.;
 END;



*************CALCULANDO OS OCUPADOS POR POSIÇÃO NA OCUPAÇÃO ***************;

IF (V0648=1) THEN TRABCC=1;
ELSE              TRABCC=0;

IF (V0648=4) THEN TRABSC=1;
ELSE              TRABSC=0;

IF (V0648=2 OR V0648=3) THEN TRABPUB=1;
ELSE                         TRABPUB=0;

IF (V0648=5) THEN CPR=1;
ELSE              CPR=0;

IF (V0648=6) THEN EMP=1;
ELSE              EMP=0;

***********CALCULANDO OS OCUPADOS FORMAIS NO TRABALHO PRINCIPAL********************;

IF (TRABCC=1 OR TRABPUB=1 OR ((CPR=1 OR EMP=1) AND V0650=1))  THEN FORMAL=1;
ELSE                                                               FORMAL=0;


***********CALCULANDO OS OCUPADOS POR GRAU DE ESCOLARIDADE********************;

IF ((1<=V0629<=6) OR (1<=V0633<=3) OR (5<=V0633<=6) OR (V0633=4 AND V0634=2) OR
    (V0633=7 AND V0634=2) OR (V0633=8 AND V0634=2) OR V0628=4)                   THEN  FUNDIN=1;
ELSE                                                                                   FUNDIN=0;


IF ((1<=V0629<=6) OR (V0629=7 AND 1<=V0631<=3) OR V0629=8 OR (1<=V0633<=8) OR
    (V0633=9 AND V0634=2) OR (V0633=10 AND V0634=2) OR V0628=4 OR (V0629=7 AND V0631=5)) THEN  MEDIN=1;
ELSE                                                                                           MEDIN=0;


IF ((10<=V0629<=12) OR (V0629=9 AND V0632=1) OR (V0633=11 AND V0634=1) OR (12<=V0633<=14))  THEN  SUPER=1;
ELSE                                                                                              SUPER=0;


***********CALCULANDO OS OCUPADOS POR SETOR********************;

IF (01101<=V6471<=03002) THEN AGRO=1;
ELSE                          AGRO=0;

IF (05000<=V6471<=09000) THEN EXTR=1;
ELSE                          EXTR=0;

IF (10010<=V6471<=33002) THEN TRANSF=1;
ELSE                          TRANSF=0;

IF (35010<=V6471<=39000) THEN SIUP=1;
ELSE                          SIUP=0;

IF (41000<=V6471<=43999) THEN CONSTR=1;
ELSE                          CONSTR=0;

IF ((48010<=V6471<=48999) OR V6471=45010 OR V6471=45030 OR V6471=45040)  THEN  COM=1;
ELSE                                                                           COM=0;

IF ((49010<=V6471<=97000) OR V6471=45020)  THEN  SERV=1;
ELSE                                             SERV=0;


*************CALCULANDO O RENDIMENTO MÉDIO DOS OCUPADOS***********;

RENOCUP=V6525;


*************CALCULANDO O % DOS OCUPADOS POR FAIXA DE RENDIMENTO EM SAL MÍNIMOS***********;

IF (V6526=0) THEN REN0=1;
IF (V6526<1) THEN REN1=1;
IF (V6526<2) THEN REN2=1;
IF (V6526<3) THEN REN3=1;
IF (V6526<5) THEN REN5=1;


IF (V6525>=0) THEN POPREN=1;


*************CALCULANDO O THEIL-L DOS RENDIMENTOS DOS OCUPADOS***********;

IF (V6525>0) THEN RTHEIL=RENOCUP;
IF (V6525>0) THEN POPT=1;


LRTHEIL=LOG(RTHEIL);


PROC SORT DATA=PES;
BY SETORC;


PROC MEANS DATA=PES SUM NOPRINT;
VAR PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
    DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M;
OUTPUT OUT=SAI1(KEEP=SETORC PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
                     DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M)
           SUM=PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
               DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M;
WEIGHT PESOPES;
BY SETORC;


PROC MEANS DATA=PES SUM NOPRINT;
WHERE OCUP18M=1;
VAR TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
    SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
    REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN;
OUTPUT OUT=SAI2(KEEP=SETORC TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
                     SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
                     REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN)
           SUM=TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
               SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
               REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN;
WEIGHT PESOPES;
BY SETORC;


PROC MEANS DATA=PES MEAN NOPRINT;
WHERE OCUP18M=1;
VAR RENOCUP RTHEIL;
OUTPUT OUT=SAI3(KEEP=SETORC RENOCUP RTHEIL)
       MEAN=RENOCUP RTHEIL;
WEIGHT PESOPES;
BY SETORC;


DATA OUT.TRAB10;
MERGE SAI1 SAI2 SAI3;
BY SETORC;


RUN;
