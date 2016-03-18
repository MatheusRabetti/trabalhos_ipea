LIBNAME OUT 'C:\Projetos\RMs Proc. 03605.001726-2014-52 - FJP Maria Luiza Marques\Maceio\Trabalho\2000\Agregacao';
LIBNAME IN 'C:\Projetos\Projeto Proc. 03605.001349-2012-90 - FJP Maria Luiza Marques\Programas\Trabalho';

 OPTIONS PS=10000 NODATE LS=250 OBS=MAX;


DATA SETOR;
SET IN.trab00;


UF=SUBSTR(SETORC,1,2);
IF UF='27';*AL;


IF PIA=. THEN PIA=0;
IF PIA1014=. THEN PIA1014=0;
IF PIA1517=. THEN PIA1517=0;
IF PIA1824=. THEN PIA1824=0;
IF PIA2529=. THEN PIA2529=0;
IF PIA18M=. THEN PIA18M=0;
IF OCUP=. THEN OCUP=0;
IF OCUP1014=. THEN OCUP1014=0;
IF OCUP1517=. THEN OCUP1517=0;
IF OCUP1824=. THEN OCUP1824=0;
IF OCUP2529=. THEN OCUP2529=0;
IF OCUP18M=. THEN OCUP18M=0;
IF DES=. THEN DES=0;
IF DES1014=. THEN DES1014=0;
IF DES1517=. THEN DES1517=0;
IF DES1824=. THEN DES1824=0;
IF DES2529=. THEN DES2529=0;
IF DES18M=. THEN DES18M=0;
IF PEA=. THEN PEA=0;
IF PEA1014=. THEN PEA1014=0;
IF PEA1517=. THEN PEA1517=0;
IF PEA1824=. THEN PEA1824=0;
IF PEA2529=. THEN PEA2529=0;
IF PEA18M=. THEN PEA18M=0;
IF TRABCC=. THEN TRABCC=0;
IF TRABSC=. THEN TRABSC=0;
IF TRABPUB=. THEN TRABPUB=0;
IF CPR=. THEN CPR=0;
IF EMP=. THEN EMP=0;
IF FORMAL=. THEN FORMAL=0;
IF FUNDIN=. THEN FUNDIN=0;
IF MEDIN=. THEN MEDIN=0;
IF SUPER=. THEN SUPER=0;
IF AGRO=. THEN AGRO=0;
IF EXTR=. THEN EXTR=0;
IF TRANSF=. THEN TRANSF=0;
IF SIUP=. THEN SIUP=0;
IF CONSTR=. THEN CONSTR=0;
IF COM=. THEN COM=0;
IF SERV=. THEN SERV=0;
IF REN0=. THEN REN0=0;
IF REN1=. THEN REN1=0;
IF REN2=. THEN REN2=0;
IF REN3=. THEN REN3=0;
IF REN5=. THEN REN5=0;
IF LRTHEIL=. THEN LRTHEIL=0;
IF POPT=. THEN POPT=0;
IF POPREN=. THEN POPREN=0;





/* IMPORTACAO DO ARQUIVO CONTENDO O DIVISOR  */
PROC IMPORT OUT= WORK.DIV_A
            DATAFILE= "C:\Projetos\RMs Proc. 03605.001726-2014-52 - FJP Maria Luiza Marques\Base_mae\Maceio\DIV_mun00.txt"
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2;
RUN;


/* IMPORTACAO DO ARQUIVO CONTENDO BASE MAE */
PROC IMPORT OUT= WORK.MAE_A
            DATAFILE= "C:\Projetos\RMs Proc. 03605.001726-2014-52 - FJP Maria Luiza Marques\Base_mae\Maceio\MAE_mun00.txt"
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2;
RUN;




DATA MAE_A(KEEP=SETORC MUN10);
SET MAE_A;


setor=PUT(Setor_2000,15.);
set1=substr(Setor,1,6);
set11=put(set1,6.);
set2=substr(Setor,8,8);
set22=put(set2,8.);
SETORC=set11!!set22;



MUN10=Cod_Mun10_2000;



PROC SORT DATA=MAE_A;
BY SETORC;



DATA DIV_A (KEEP=SETORC DIVPESO);
SET DIV_A;


setor=PUT(Setor_2000,15.);
set1=substr(Setor,1,6);
set11=put(set1,6.);
set2=substr(Setor,8,8);
set22=put(set2,8.);
SETORC=set11!!set22;



DIVPESO=Div_Mun10_00;


PROC SORT DATA=DIV_A;
BY SETORC;


DATA DIVMAE;
MERGE MAE_A(IN=A) DIV_A(IN=B);
IF A AND B;
BY SETORC;run;


DATA DIVMAE(DROP= DIVPESO);
SET DIVMAE;

RETAIN D(0);
 IF FIRST.SETORC THEN DO;
  D=0;
 BY SETORC;
END;

D=D+1;


DATA UMO;
MERGE SETOR(IN=A) DIV_A(IN=B);
IF A;
BY SETORC;

IF DIVPESO=1;

D=1;

DATA DEDOS;
MERGE SETOR(IN=A) DIV_A(IN=B);
IF A;
BY SETORC;

IF DIVPESO=2;

DATA DEDOS;
SET DEDOS DEDOS;

PROC SORT DATA=DEDOS;
BY SETORC;

DATA DEDOS;
SET DEDOS;

RETAIN D(0);
  IF FIRST.SETORC THEN DO;
  D=0;
  BY SETORC;
END;

D=D+1;


DATA DETRE;
MERGE SETOR(IN=A) DIV_A(IN=B);
IF A;
BY SETORC;

IF DIVPESO=3;

DATA DETRE;
SET DETRE DETRE DETRE;

PROC SORT DATA=DETRE;
BY SETORC ;

DATA DETRE;
SET DETRE;

RETAIN D(0);
  IF FIRST.SETORC THEN DO;
  D=0;
  BY SETORC ;
END;

D=D+1;

DATA DECUA;
MERGE SETOR(IN=A) DIV_A(IN=B);
IF A;
BY SETORC;

IF DIVPESO=4;

DATA DECUA;
SET DECUA DECUA DECUA DECUA;

PROC SORT DATA=DECUA;
BY SETORC ;

DATA DECUA;
SET DECUA;

RETAIN D(0);
  IF FIRST.SETORC THEN DO;
  D=0;
  BY SETORC ;
END;

D=D+1;


DATA DECIN;
MERGE SETOR(IN=A) DIV_A(IN=B);
IF A;
BY SETORC;

IF DIVPESO=5;

DATA DECIN;
SET DECIN DECIN DECIN DECIN DECIN;

PROC SORT DATA=DECIN;
BY SETORC ;

DATA DECIN;
SET DECIN;

RETAIN D(0);
  IF FIRST.SETORC THEN DO;
  D=0;
  BY SETORC ;
END;

D=D+1;


DATA DESEI;
MERGE SETOR(IN=A) DIV_A(IN=B);
IF A;
BY SETORC;

IF DIVPESO=6;

DATA DESEI;
SET DESEI DESEI DESEI DESEI DESEI DESEI;

PROC SORT DATA=DESEI;
BY SETORC ;

DATA DESEI;
SET DESEI;

RETAIN D(0);
  IF FIRST.SETORC THEN DO;
  D=0;
  BY SETORC ;
END;

D=D+1;




DATA SETOR;
SET UMO DEDOS DETRE DECUA DECIN DESEI;


PROC SORT DATA=SETOR;
BY SETORC D;

PROC SORT DATA=DIVMAE;
BY SETORC D;


DATA FINAL;
MERGE SETOR(IN=A) DIVMAE(IN=B);
IF A AND B;
BY SETORC D;



PROC MEANS DATA=FINAL SUM;
VAR PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
    DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M
    TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
    SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
    REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN;



DATA FINAL;
SET FINAL;


PIA=PIA/DIVPESO;
PIA1014=PIA1014/DIVPESO;
PIA1517=PIA1517/DIVPESO;
PIA1824=PIA1824/DIVPESO;
PIA2529=PIA2529/DIVPESO;
PIA18M=PIA18M/DIVPESO;
OCUP=OCUP/DIVPESO;
OCUP1014=OCUP1014/DIVPESO;
OCUP1517=OCUP1517/DIVPESO;
OCUP1824=OCUP1824/DIVPESO;
OCUP2529=OCUP2529/DIVPESO;
OCUP18M=OCUP18M/DIVPESO;
DES=DES/DIVPESO;
DES1014=DES1014/DIVPESO;
DES1517=DES1517/DIVPESO;
DES1824=DES1824/DIVPESO;
DES2529=DES2529/DIVPESO;
DES18M=DES18M/DIVPESO;
PEA=PEA/DIVPESO;
PEA1014=PEA1014/DIVPESO;
PEA1517=PEA1517/DIVPESO;
PEA1824=PEA1824/DIVPESO;
PEA2529=PEA2529/DIVPESO;
PEA18M=PEA18M/DIVPESO;
TRABCC=TRABCC/DIVPESO;
TRABSC=TRABSC/DIVPESO;
TRABPUB=TRABPUB/DIVPESO;
CPR=CPR/DIVPESO;
EMP=EMP/DIVPESO;
FORMAL=FORMAL/DIVPESO;
FUNDIN=FUNDIN/DIVPESO;
MEDIN=MEDIN/DIVPESO;
SUPER=SUPER/DIVPESO;
AGRO=AGRO/DIVPESO;
EXTR=EXTR/DIVPESO;
TRANSF=TRANSF/DIVPESO;
SIUP=SIUP/DIVPESO;
CONSTR=CONSTR/DIVPESO;
COM=COM/DIVPESO;
SERV=SERV/DIVPESO;
REN0=REN0/DIVPESO;
REN1=REN1/DIVPESO;
REN2=REN2/DIVPESO;
REN3=REN3/DIVPESO;
REN5=REN5/DIVPESO;
LRTHEIL=LRTHEIL/DIVPESO;
POPT=POPT/DIVPESO;
POPREN=POPREN/DIVPESO;


IF PIA=. THEN PIA=0;
IF PIA1014=. THEN PIA1014=0;
IF PIA1517=. THEN PIA1517=0;
IF PIA1824=. THEN PIA1824=0;
IF PIA2529=. THEN PIA2529=0;
IF PIA18M=. THEN PIA18M=0;
IF OCUP=. THEN OCUP=0;
IF OCUP1014=. THEN OCUP1014=0;
IF OCUP1517=. THEN OCUP1517=0;
IF OCUP1824=. THEN OCUP1824=0;
IF OCUP2529=. THEN OCUP2529=0;
IF OCUP18M=. THEN OCUP18M=0;
IF DES=. THEN DES=0;
IF DES1014=. THEN DES1014=0;
IF DES1517=. THEN DES1517=0;
IF DES1824=. THEN DES1824=0;
IF DES2529=. THEN DES2529=0;
IF DES18M=. THEN DES18M=0;
IF PEA=. THEN PEA=0;
IF PEA1014=. THEN PEA1014=0;
IF PEA1517=. THEN PEA1517=0;
IF PEA1824=. THEN PEA1824=0;
IF PEA2529=. THEN PEA2529=0;
IF PEA18M=. THEN PEA18M=0;
IF TRABCC=. THEN TRABCC=0;
IF TRABSC=. THEN TRABSC=0;
IF TRABPUB=. THEN TRABPUB=0;
IF CPR=. THEN CPR=0;
IF EMP=. THEN EMP=0;
IF FORMAL=. THEN FORMAL=0;
IF FUNDIN=. THEN FUNDIN=0;
IF MEDIN=. THEN MEDIN=0;
IF SUPER=. THEN SUPER=0;
IF AGRO=. THEN AGRO=0;
IF EXTR=. THEN EXTR=0;
IF TRANSF=. THEN TRANSF=0;
IF SIUP=. THEN SIUP=0;
IF CONSTR=. THEN CONSTR=0;
IF COM=. THEN COM=0;
IF SERV=. THEN SERV=0;
IF REN0=. THEN REN0=0;
IF REN1=. THEN REN1=0;
IF REN2=. THEN REN2=0;
IF REN3=. THEN REN3=0;
IF REN5=. THEN REN5=0;
IF LRTHEIL=. THEN LRTHEIL=0;
IF POPT=. THEN POPT=0;
IF POPREN=. THEN POPREN=0;


PROC MEANS DATA=FINAL SUM;
VAR PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
    DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M
    TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
    SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
    REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN;



************************ AGREGAÇÃO PARA MUN10 ************************;

 PROC SORT DATA=FINAL;
 BY MUN10;



PROC MEANS DATA=FINAL SUM NOPRINT;
VAR PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
    DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M
    TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
    SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
    REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN;
OUTPUT OUT=SAI1(KEEP=MUN10
                     PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
                     DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M
                     TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
                     SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
                     REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN)
           SUM=PIA PIA1014 PIA1517 PIA1824 PIA2529 PIA18M OCUP OCUP1014 OCUP1517 OCUP1824 OCUP2529 OCUP18M DES DES1014 DES1517
           DES1824 DES2529 DES18M PEA PEA1014 PEA1517 PEA1824 PEA2529 PEA18M
           TRABCC TRABSC TRABPUB CPR EMP FORMAL FUNDIN MEDIN
           SUPER AGRO EXTR TRANSF SIUP CONSTR COM SERV
           REN0 REN1 REN2 REN3 REN5 LRTHEIL POPT POPREN;
BY MUN10;


PROC MEANS DATA=FINAL MEAN NOPRINT;
VAR RENOCUP RTHEIL;
OUTPUT OUT=SAI2(KEEP=MUN10 RENOCUP RTHEIL)
       MEAN=RENOCUP RTHEIL;
WEIGHT OCUP18M;
BY MUN10;


DATA JUNTO;
MERGE SAI1 SAI2;
BY MUN10;



P_REN0=REN0/POPREN*100;
P_REN1=REN1/POPREN*100;
P_REN2=REN2/POPREN*100;
P_REN3=REN3/POPREN*100;
P_REN5=REN5/POPREN*100;
THEIL=(LOG(RTHEIL)-((1/POPT)*LRTHEIL));



********************;
* GRAVACAO DA BASE *;
********************;


DATA OUT.TRAB00_MUN10;
SET JUNTO;


T_ATIV=PEA/PIA*100;
T_DES=DES/PEA*100;
T_ATIV1014=PEA1014/PIA1014*100;
T_DES1014=DES1014/PEA1014*100;
T_ATIV1517=PEA1517/PIA1517*100;
T_DES1517=DES1517/PEA1517*100;
T_ATIV1824=PEA1824/PIA1824*100;
T_DES1824=DES1824/PEA1824*100;
T_ATIV2529=PEA2529/PIA2529*100;
T_DES2529=DES2529/PEA2529*100;
T_ATIV18M=PEA18M/PIA18M*100;
T_DES18M=DES18M/PEA18M*100;
P_TRABCC=TRABCC/OCUP18M*100;
P_TRABSC=(TRABSC-TRABPUB)/OCUP18M*100;
P_TRABPUB=TRABPUB/OCUP18M*100;
P_CPR=CPR/OCUP18M*100;
P_EMP=EMP/OCUP18M*100;
P_FORMAL=FORMAL/OCUP18M*100;
P_FUND=(1-FUNDIN/OCUP18M)*100;
P_MED=(1-MEDIN/OCUP18M)*100;
P_SUPER=SUPER/OCUP18M*100;
P_AGRO=AGRO/OCUP18M*100;
P_EXTR=EXTR/OCUP18M*100;
P_TRANSF=TRANSF/OCUP18M*100;
P_SIUP=SIUP/OCUP18M*100;
P_CONSTR=CONSTR/OCUP18M*100;
P_COM=COM/OCUP18M*100;
P_SERV=SERV/OCUP18M*100;
P_REN0=REN0/POPREN*100;
P_REN1=REN1/POPREN*100;
P_REN2=REN2/POPREN*100;
P_REN3=REN3/POPREN*100;
P_REN5=REN5/POPREN*100;
THEIL=(LOG(RTHEIL)-((1/POPT)*LRTHEIL));



PROC DBLOAD DBMS=EXCEL DATA=OUT.TRAB00_MUN10;
PATH='C:\Projetos\RMs Proc. 03605.001726-2014-52 - FJP Maria Luiza Marques\Maceio\Trabalho\2000\Agregacao\TRAB00_MUN10.XLS';
PUTNAMES=YES;
LIMIT=0;
LOAD;
RUN;