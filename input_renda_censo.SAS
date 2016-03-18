
/*---- Leitura dos dados ----*/

/*libname dados 'C:\Bases de dados\Censo 2010\SAS\'; /*Endereço dos arquivos sas utilizados para teste*/

/*Leitura dos dados*/

libname dados 'D:\renda 2010'; 


proc delete data=_all_;
run;

	

/*---- Banco de domicílios ----*/

data dom_a (compress=yes);
	set dados.domicilios 
		(keep= v0001 v0401 v0300 v4001 V4002 v6531 v0011 v1006 
			v0201 v0202 v0203 v0205 v0207 v0208 v0210 v0211 
			v0213 v0214 v0215 v0216 v0217 v0218 v0219 v0220 v0222 v6204); /*Guarda as variáveis de interesse*/

	/*Seleciona apenas domicílios particulares permanentes*/
	if v4001 in ("01","02");

	/*Renomeia as variáveis de controle do domicílio, rendimento per capita e área de ponderação*/
	iddom = v0300;
	rdpc = v6531;
	area = v0011;
	npess = v0401; /*Renomeia a variável V0401*/

	/*Cria variável dummy para indicar área urbana ou rural*/
	if (v1006="1") then urbano=1;
	else urbano=0;

	/*Cria variável dummy para indicar existencia de banheiro de uso exclusivo*/
	if v0205 in ("1","2","3","4","5","6","7","8","9") then banhexc=1;
	else banhexc=0;

	/*Cria variável dummy para indicar existência de esgotamento sanitário adequado - rede ou fossa séptica*/
	if v0207 in ("1","2") then esgadeq=1;
	else esgadeq=0;

	/*Cria variável dummy para indicar existência de água no domicílio (ou aldeia)*/
	if v0208 in ("01","02","09") then temagua=1;
	else temagua=0;

	/*Cria variável dummy para indicar existência de água de rede geral no domicílio*/
	if v0208 = "01" then aguarede=1;
	else aguarede=0;

	/*Cria variável dummy para indicar coleta de lixo*/
	if v0210 in ("1","2") then lixo=1;
	else lixo=0;

	/*Cria variável dummy para indicar a existência de eletricidade*/
	if v0211 in ("1","2") then eletrica=1;
	else eletrica=0;

	/*Cria variável dummy para indicar a existência de radio*/
	if v0213 = "1" then radio=1;
	else radio=0;

	/*Cria variável dummy para indicar a existência de TV qualquer tipo*/
	if v0214 = "1" then tv=1;
	else tv=0;

	/*Cria variável dummy para indicar a existência de máquina de lavar*/
	if v0215 = "1" then maqlav=1;
	else maqlav=0;

	/*Cria variável dummy para indicar a existência de geladeira ou freezer*/
	if v0216 = "1" then geladeira=1;
	else geladeira=0;

	/*Cria variável dummy para indicar a existência de telefone - fixo ou celular*/
	if (v0217 = "1") or (v0218 = "1") then fone=1;
	else fone=0;

	/*Cria variável dummy para indicar a existência de computador*/
	if v0219 = "1" then computador=1;
	else computador=0;

	/*Cria variável dummy para indicar a existência de automóvel particular*/
	if v0222 = "1" then auto=1;
	else auto=0;

	/*Cria variável dummy para indicar densidade de moradores por dormitório <=2*/
	if v6204 <= 2 then dens=1;
	else dens=0;

	/*Cria nova variável para tipo de espécie*/
	if v4002 in ("11","12","13","14") then especie=v4002;
	else especie="15";

	/*Cria nova variável para condição de ocupação*/
	if v0201 in ("1","2","3") then condicao=v0201;
	else condicao="4";

	/*Cria nova variável para material predominante paredes externas*/
	if v0202 in ("1","2","3","4") then parede=v0202;
	else parede="6";

	/*Cria nova variável para número de cômodos*/
	if v0203<= 2 then comodo="1";
	else if v0203 in (3,4) then comodo="2";
	else if v0203 in (5,6) then comodo="3";
	else comodo="4";

	/*Cria nova variável para número de banheiros em categorias*/
	if v0205 in ("0","1") then banheiro=v0205;
	else banheiro="2";

	/*Cria variável dummy para indicar a existência de computador com acesso à internet*/
	if v0220 = "1" then micro=1;
	else micro=0;

	/*Cria variável dummy para indicar a existência de telefone fixo*/
	if v0218 = "1" then fonefixo=1;
	else fonefixo=0;

	/*Cria variável dummy para indicar a existência de automóvel particular*/
	if v0222 = "1" then carro=1;
	else carro=0;

keep iddom--carro;
run;



/*---- Banco de pessoas ----*/

data pes_a (compress=yes);
	set dados.pessoas
		(keep = v0001 v0010 v0300 v0502 v0601 v0606 v6036 v6400 v0010 v0011); /*Guarda as variáveis de interesse*/

	/* Elimina pensionistas, empregados domésticos e parente de empregados domésticos que moram no domicilio */
	if v0502 in ("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16");

	area = v0011; /*Renomeia a variável área de ponderação*/
	uf = v0001;   /* Unidade da federação */
	iddom= v0300; /*Renomeia a variável de controle do domicílio*/
	peso = v0010; /* Fator de expansão ou peso amostral */

	/*Cria variável dummy para indicar as pessoas menores de 14 anos (responsável (10 a 14 anos) ou pessoa com parentesco com responsável */
	if (v6036<14) then indicam14 = 1;
	else indicam14 = 0;

	/*Dummy para a pessoa de referencia*/
	if v0502 in ("01") then chefe=1;
	else chefe=0;

	/*Cria nova variável para cor ou raça em categorias*/
	if v0606 in ("1","2") then cor = v0606;
	else if v0606="4" then cor="3";
	else cor="4";

	/*Cria variável dummy para indicar sexo - 1 masculino e 0 feminino*/
	if v0601 = "1" then sexo = 1;
	else sexo = 0; 

	inst = v6400*1; /*Nível de instrução em formato númerico para as contas seguintes*/
	if inst=5 then inst=2; /*Junta a categoria 5 da variável nível de instrução na categoria 2 dessa variável*/

	concluido = (inst-1)/(4-1); /*Cria a variável concluido a partir do nível de instrução*/

keep area--concluido;
run;



proc sql;
 
/*Procedimento para somar as pessoas menores de 14 anos nos domicílios*/
create table pes_b (compress=yes) as
	select *, sum(indicam14) as pesm14
	from pes_a
	group by iddom;

/*Indexação da base de pessoas ao nível da área de ponderação*/
create index area on pes_b;

/*Indexação da base de domicílios ao nível da área de ponderação*/
create index area on dom_a;

/*Junta as bases de pessoas e domicílios e seleciona apenas o responsável pelo domicílio*/
create table dados_a (compress=yes drop=chefe) as
	select *
	from pes_b as a, dom_a as b
	where a.area = b.area and
	      a.iddom = b.iddom and
	      a.chefe=1;

quit;



/*---- Cria dois clusters para definir a variável "pobre" e "falso pobre" ----*/

proc fastclus data=dados_a noprint
	maxclusters=2
	mean=media 
	out=bancoclus (keep=iddom cluster compress=yes);
		where rdpc=0;
		var concluido urbano banhexc esgadeq temagua aguarede lixo eletrica radio tv maqlav geladeira fone computador auto dens;
run;



/*---- Criação da variável "falso pobre" ----*/

proc means data=media noprint;
	output out=minimo (drop=_type_ _freq_)
		min(concluido)=min_concluido;
run;


data _null_;
	set minimo;
	call symput('min',min_concluido);
run;


data falsopobre;
	set media (keep = cluster concluido);
	/* falsopobre = 1 corresponde às famílias que possuem rdpc=0 mas que não tem perfil para isso */
	if round(concluido,0.001) = round(&min.,0.001) then falsopobre = 0;
	else falsopobre = 1;
run;



/*---- Incorporção da variável 'falsopobre' à base com os clusters ----*/

proc sql;
create table bancoclus_b (compress=yes) as
	select a.*, b.falsopobre
	from bancoclus as a, falsopobre as b
	where a.cluster = b.cluster;
quit;


/*--------------------------------------------------------------------------------
* Se você quer verificar quantos falsos pobres existem,
  basta rodar a frequencia abaixo.;

	proc freq data=bancoclus_b;
	table falsopobre;
	run;
---------------------------------------------------------------------------------*/


proc sql;

create index iddom on dados_a;
create index iddom on bancoclus_b;

create table dados_b (compress=yes) as
	select *
	from dados_a as a 
		full join bancoclus_b as b
	on a.iddom = b.iddom;

quit;


data final (compress=yes);
set dados_b;

	/*A variável y assume valor 0 quando a rdpc é missing, e 1 caso contrário, 
	  e é utilizada para a obtenção dos scores*/
	if falsopobre = 1 then do;
		rdpc = .;
		y_probit = 0;
	end;
	else y_probit = 1;

run;


proc sql;
/*Indexação ao nível da Unidade da Federação*/
create index uf on final;
quit;



/*---- Modelo probit para a variável y, criada no passo anterior (por uf) ----*/

ods _all_ close;
proc logistic data=final /*noprint*/;
	class y_probit inst condicao parede comodo banheiro cor especie;
	model y_probit = inst condicao parede comodo banheiro micro carro fonefixo cor especie npess pesm14 / link=probit;
	weight peso / normalize; /*Ponderação pelo peso corrigido*/
	by uf;
	output out=scores_a (compress=yes) p=score;
	ods output ParameterEstimates=ParameterEstimates /* Guarda os resultados dos modelos para verificação */
		FitStatistics = FitStatistics
		NObs = NObs
		Association=Association
		ClassLevelInfo=ClassLevelInfo
		ConvergenceStatus=ConvergenceStatus
		GlobalTests=GlobalTests
		ModelInfo=ModelInfo
		ResponseProfile=ResponseProfile;
run;
ods listing;



/*---- Ordenando a base pela área de ponderação, e definindo novo id, que pode ser lincado com o anterior ----*/

data scores_b (compress=yes);
set scores_a (drop=_level_);
id = _n_;
run;


proc sort data=scores_b;
by area;
run;

 

/*---- Seleção dos 15 vizinhos mais próximos nas áreas de ponderação ----*/

ods _all_ close;
proc modeclus data=scores_b dk=16 neighbor;
	var score;
	id id;
	by area;
	ods output neighbor=vizinhos_a (compress=yes);
run;
ods listing;


/* A base de dados já vem ordenada por 'distance', então o trabalho sujo 
   já foi feito pelo SAS */
data vizinhos_b (compress=yes);

	/* o 'retain' serve para repassar o valor de uma observação para a outra */
	retain id count 1;

	set vizinhos_a (rename=(id=id_char Nbor=Nbor_char));

	/* Converte para numérico */
	/*vizinhos = input(strip(Nbor_char), best32.);*/
     vizinhos = Nbor_char*1;
	/* Se passa para uma nova observação, reinicia o contador de vizinhos e converte os códigos */
	if not missing(id_char) then do;
		/*id = input(strip(id_char), best32.);*/
		id = id_char*1;
		count = 1;
	end;
	else do; /* Caso contrário, conta o número de vizinhos */
		count + 1;
	end;

	/* Com os dados arrumados, é possível selecionar os vizinhos mais próximos aqui mesmo! */
	if count>5 then delete;
   
	drop ControlVar id_char Nbor_char;
run;

/*---- Captura a renda dos vizinhos ----*/

proc sql;

/* para economizar recursos, seleciona apenas os falso pobres */
create table vizinhos_c (compress=yes) as
	select a.* from vizinhos_b as a, scores_b as b
	where a.id = b.id
		and b.rdpc = .;

/* pega a informação de rdpc apenas de quem nao tem rdpc missing */
create table scores_c (compress=yes) as
	select rdpc, id 
	from scores_b 
	where rdpc ne .;

/* coloca a informação de renda nos vizinhos dos falsos pobres */
create table vizinhos_d (compress=yes) as
	select a.*, b.rdpc as rdpc_vizinho
	from vizinhos_c as a, scores_c as b
	where a.vizinhos = b.id
	order by a.id;

quit;

proc means data=vizinhos_d noprint min;
var count;
output out=vizinhos_d2 (drop= _freq_ _type_)
min=minimo;
by id;
run;

proc sql;
/*Junta as bases de vizinhos_d e d2 e seleciona apenas o dom minimo, o vizinho mais proximo*/
create table vizinhos_pri (compress=yes ) as
	select *
	from vizinhos_d as a, vizinhos_d2 as b
	where a.id = b.id and
	      a.count = b.minimo;

quit;


/* vai sair - não vai ter mais mediana */
/*---- Obtenção da mediana de rendimentos dos 5 vizinhos mais próximos ----*/

/* O número de observações desse arquivo deve ser igual ao número de falso pobres visto anteriormente.
   Se for menor, é porque em algumas observações nenhum dos vizinhos tinha renda válida. 
proc means data=vizinhos_d noprint nway;
class id;
output out=mediana (compress=yes drop=_type_ _freq_) 
	median(rdpc_vizinho)=mediana
	n(id)=num_vizinhos; /* ok, podem não ser 5 vizihos, pois selecionamos apenas aqueles com rdpc valida */
/*run;*/


/*--------------------------------------------------------------------------------
* Se você quer verificar quantos vizinhos foram utilizados,
  basta rodar a frequencia abaixo;

	proc freq data=mediana;
	table num_vizinhos;
	run;
---------------------------------------------------------------------------------*/



/*---- Merge das bases e criação da variável renda imputada ----*/


/* A base imputada contém apenas poucas variáveis relevantes. Supostamente
   essa base vai ser pareada com os dados do Censo originais */
proc sql;
create table almostfinal as
	select *
	from scores_b (keep=iddom id rdpc falsopobre) as a full join vizinhos_pri as b
	on a.id=b.id;
quit;


/* Base final */
data imputado;
set almostfinal;

rdpc_imput = rdpc;
if rdpc = . then do;
	rdpc_imput = sum(0,rdpc_vizinho);
	flag_imput = 1; /* indicador de imputação */
end;

run;


