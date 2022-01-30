libname Projet "/home/u59520851/Projet";
/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* A : Descriptif de la population */
/* Question 1 */
/* 1. Avec une proc transpose, créer la table de données qui aura les variables suivantes: */
/*----------------------------------------------------------------------------------------------------------------------------------------*/
data alcool;
set Projet.don2;
rename r2 = R2 r3 = R3 r5 = R5 r12 = R12 r13=R13 r14=R14 cognac=Cognac;
run;

data Alcool_keys;
   set alcool;
   length key $ 7; 
   key = ifc(missing(item), ' ', put(item, 2.));
   key = catx('_', var, key);
   drop var item;
run;

PROC SORT data = Alcool_keys SORTSEQ=linguistic
	(NUMERIC_COLLATION=ON);
BY nuq key;
run; 

PROC TRANSPOSE data = Alcool_keys out = Alcool_A_Q1 delimiter=_;
by nuq;
id key;
var value;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* Question 2*/
/* Appliquer le format approprié aux variables R2 et R5 Vous vous servirez des informations présentes dans la table excel pour déclarer ce 
format (proc format) et vous l’appliquerez à la table */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC FORMAT;
value segment_sex
1 = "Male"
2 = "Female"
;
run;

PROC FORMAT;
value segment_freq
1 = "Never"
2 = "Less often"
3 = "Once every 4 to 6 months"
4 = "Once every 2 or 3 months"
5 = "1 to 3 times per month"
6 = "1 to 4 times per week"
7 = "Almost every day or more often"
;
run;

data Alcool_A_Q2;
set Alcool_A_Q1;
Format R2 segment_sex.;
Format R5 segment_freq.;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* Question 3 */
/* 3. Créer une variable en classe d’âge (20-34 ; 35-49 ; 50-65) */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC FORMAT;
value segment_age_group
19<-34 = "20-34"
34<-49 = "35-49"
49<-65 = "50-65"
;
run;

data Alcool_A_Q3;
set Alcool_A_Q2;
Format Age_group segment_age_group.;
Age_group = R3;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* Question 4 */
/* 4. Quel est la répartition en âge, en sexe, en revenu, en statut marital et présence d’enfants */
/*----------------------------------------------------------------------------------------------------------------------------------------*/
/*
PROC FREQ data = Alcool_A_Q3;
tables Age_group*R2*R12*R13*R14
/chisq
;
title 'Distribution within clusters based on age groups';
run;
*/
PROC FREQ data = Alcool_A_Q3;
tables Age_group
/chisq
;
title 'Distribution by age group';
run;


PROC SGPLOT data = Alcool_A_Q3;
histogram R2 / binstart = 1 binwidth = 1;
xaxis label="Sex";
yaxis label="Percentage";
title 'Distribution by sex';
run;

PROC SGPLOT data = Alcool_A_Q3;
histogram R12 / binwidth = 1;
xaxis label="Income";
yaxis label="Percentage";
title 'Distribution by Household income';
run;

PROC SGPLOT data = Alcool_A_Q3;
histogram R13 / binwidth = 1;
xaxis label="Status";
yaxis label="Percentage";
title 'Distribution by Marital status';
run;

PROC SGPLOT data = Alcool_A_Q3;
histogram R14 / binwidth = 1;
xaxis label="Presence of children";
yaxis label="Percentage";
title 'Distribution by presence of children in the household';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* Question 5 */
/* 5. Descriptif des consommateurs de cognac (cognac=1 versus cognac=2)
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC SGPLOT data = Alcool_A_Q3;
histogram Cognac / binwidth = 1;
xaxis label="Consumes or not alcohol";
yaxis label="Percentage";
title 'Distribution of cognac consumers';
run;

PROC FREQ data = Alcool_A_Q3;
tables Cognac
/chisq
;
title 'Distribution of cognac consumers (YES/NO)';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/*a. Les hommes et les femmes consomment-ils de la même façon du cognac ? */
/*----------------------------------------------------------------------------------------------------------------------------------------*/
PROC SGPLOT data = Alcool_A_Q3;
histogram R6_1 / group = R2 transparency=0.5 showbins binwidth=1;
xaxis label = "How often do you drink Cognac?";
title 'Distribution of Cognac consumers based on sex';
run;

PROC FREQ data = Alcool_A_Q3;
tables R6_1*R2
/chisq
;
title 'Distribution of cognac consumers based on sex';
run;
/*----------------------------------------------------------------------------------------------------------------------------------------*/
/*b. Les personnes, hommes et femmes confondus, consomment-elles du cognac de façon identique en fonction de l’âge ? */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC SGPLOT data = Alcool_A_Q3;
histogram R6_1 / group = Age_group transparency=0.5 showbins binwidth=1;
xaxis label = "How often do you drink Cognac?";
title 'Distribution of Cognac consumers based on age groups';
run;

PROC FREQ data = Alcool_A_Q3;
tables R6_1*Age_group
/chisq
;
title 'Distribution of cognac consumers based on age groups';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* B : L’offre */
/* Analyse de l’offre en fonction des intentions d’achat : l’objectif est de réaliser une Analyse en Composantes Principales (ACP) des variables IA1 (intention d’achat)
1. Recodez les réponses aux questions codées initialement de 1 à 5 (1-Certainement pas à 5-Certainement) en « probabilité » d’achat appelé los (likelihood of success) (1=0, 2=0, 3=0, 4=0.7 et 5=1). */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC FORMAT;
value los
1 = 0
2 = 0
3 = 0
4 = 0.7
5 = 1
;
run;

data Alcool_B_Q1;
  set Alcool_A_Q3;
  array prob ia1_2 ia1_3 ia1_4 ia1_5 ia1_6 ia1_7 ia1_8 ia1_9 ia1_10 ia1_11 ia1_15 ia1_17 ia1_18 ia1_19 ia1_21 ia1_22 ia1_23 ia1_24 ia1_25 ia1_30 ia1_31 ia1_33 ia1_34 ia1_35 ia1_36 ia1_37;
  do index=1 to dim(prob);
    prob[index] = input(put(prob[index],los.),32.);
  end;
  drop index;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* 2. Réaliser une analyse en composante principale avec une rotation varimax (R=V). Combien d’axes retenez-vous et pourquoi ? */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC FACTOR DATA = Alcool_B_Q1 
out=Alcool_B_Q2
method =principal
scree
nfactors=6 
rotate=varimax
fuzz = 0.32
;
var ia1_2 ia1_3 ia1_4 ia1_5 ia1_6 ia1_7 ia1_8 ia1_9 ia1_10 ia1_11 ia1_15 ia1_17 ia1_18 ia1_19 ia1_21 ia1_22 ia1_23 ia1_24 ia1_25 ia1_30 ia1_31 ia1_33 ia1_34 ia1_35 ia1_36 ia1_37;
title "PCA"
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* 4. Donner un nom à chacun des facteurs (groupe de produits) */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

data Alcool_B_Q2;
set Alcool_B_Q2;
label Factor1 = 'Cognac_T1'Factor2 = 'Cognac_T2' Factor3 = 'Vodka' Factor4 = 'DessertWine_Gin' Factor5 = 'Whisky' Factor6 = 'Rhum';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* 5. Construisez 6 nouvelles variables (Proba_F1 à Proba_F6) correspondant à la probabilité moyenne d’acheter les produits du 
facteur1, facteur2 --- facteur6 pour chacun des sujets. Les variables crées se nommeront Proba_F1 à Proba_F6. */
/* 6. Appliquer un label à ces facteurs en fonction du nom donné ci-dessus. */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

data Alcool_B_Q5;
set Alcool_B_Q2;
Proba_F1 = sum(of ia1_4 ia1_5 ia1_8 ia1_11 ia1_15 ia1_17 ia1_18 ia1_19)/8;
Proba_F2 = sum(of ia1_2 ia1_3 ia1_6 ia1_7 ia1_9 ia1_10)/6;
Proba_F3 = sum(of ia1_33 ia1_34 ia1_35)/3;
Proba_F4 = sum(of ia1_21 ia1_22 ia1_23 ia1_36 ia1_37)/5;
Proba_F5 = sum(of ia1_24 ia1_25)/2;
Proba_F6 = sum(of ia1_30 ia1_31)/2;
label Proba_F1 = 'Prob_Cognac_T1'Proba_F2 = 'Prob_Cognac_T2' Proba_F3 = 'Prob_Vodka' Proba_F4 = 'Prob_DessertWine_Gin' Proba_F5 = 'Proba_Whisky' Proba_F6 = 'Prob_Rhum';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* 7. Calculer les moyennes des variables Proba_F1 à Proba_F6 selon le sexe et la classe d’âge (créée à l’exercice A3) – Commenter */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

proc means data=Alcool_B_Q5 nway;
	class R2 Age_group;
	var Proba_F1;
	output out = Cognac_1_per_Gender_And_Age mean=avg_result;
	title "Average";
run;

proc means data=Alcool_B_Q5 nway;
	class R2 Age_group;
	var Proba_F2;
	output out = Cognac_2_per_Gender_And_Age mean=avg_result;
	title "Average";
run;

proc means data=Alcool_B_Q5 nway;
	class R2 Age_group;
	var Proba_F3;
	output out = Vodka_per_Gender_And_Age mean=avg_result;
run;

proc means data=Alcool_B_Q5 nway;
	class R2 Age_group;
	var Proba_F4;
	output out = DWine_Gin_per_Gender_And_Age mean=avg_result;
	title "Average";
run;

proc means data=Alcool_B_Q5 nway;
	class R2 Age_group;
	var Proba_F5;
	output out = Whisky_per_Gender_And_Age mean=avg_result;
	title "Average";
run;

proc means data=Alcool_B_Q5 nway;
	class R2 Age_group;
	var Proba_F6;
	output out = Rhum_per_Gender_And_Age mean=avg_result;
	title "Average";
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* C : Classification en fonction de la probabilité d’acheter. */
/* 1. Réaliser une Classification (CAH) en utilisant les variables Proba_F1 à Proba_F6. 
La méthode utilisée est la méthode de WARD et l’identifiant des individus est la variable NUQ.*/
/*----------------------------------------------------------------------------------------------------------------------------------------*/

ods graphics on;

PROC cluster data=Alcool_B_Q5 plots(maxpoints=264) outtree=tree ccc method=ward pseudo print=15;
id nuq;
var Proba_F1 Proba_F2 Proba_F3 Proba_F4 Proba_F5 Proba_F6;
title "CLUSTERING"
run;

proc tree data=tree out=clus5 nclusters=5;
id nuq;
copy Proba_F1 Proba_F2 Proba_F3 Proba_F4 Proba_F5 Proba_F6; 
run;

ods graphics off;

proc sort data=clus5;
by nuq;
run;

data Alcool_C_Q3;
	merge Alcool_B_Q5 clus5;
	by nuq Proba_F1 Proba_F2 Proba_F3 Proba_F4 Proba_F5 Proba_F6;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/*a. Calculez l’effectif des classes */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

PROC sort data=clus5;
by cluster;
run;

proc sql;
select cluster, count(*) as size from clus5 group by cluster;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/*b. Descriptif des classes en termes de variables qui ont construit les classes (les variables Proba_F1 à Proba_F6) */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

Proc print data = clus5;
by cluster;
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* c. Descriptif des classes en termes de sexe, classe d’âge et de consommation de vins spiritueux (question R5). */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

/*NOTE: Je ne savais pas si vous vouliez que toute la distribution soit séparée ou ensemble (by gender, age, etc), alors j'ai fait les deux. */

/*
PROC FREQ data = Alcool_C_Q3;
tables cluster*R2*Age_group*R5
/chisq
;
title 'Distribution within clusters based on sex, age_group and wine consumption';
run;
*/

PROC FREQ data = Alcool_C_Q3;
tables cluster*R2
/chisq
;
title 'Distribution within clusters based on sex';
run;

PROC FREQ data = Alcool_C_Q3;
tables cluster*Age_group
/chisq
;
title 'Distribution within clusters based on age groups';
run;

PROC FREQ data = Alcool_C_Q3;
tables cluster*R5
/chisq
;
title 'Distribution within clusters based on wine consumption';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* Construisez 3 indicatrices (variable valant 1=oui ou 0=Non)
 traduisant le fait d’utiliser souvent ou très souvent « Pure, no ice » ET « Pure with ICE » ET « dans un cocktail ».*/
/*----------------------------------------------------------------------------------------------------------------------------------------*/

data Alcool_C_Q3;
set Alcool_C_Q3;
label us5a_1 = 'Pure, with no ice' us5a_2 = 'Pure, with ice' us5a_3 = 'Mixed in a cocktail';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* Caractériser les 5 classes avec ces 3 nouvelles variables. */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

/*
PROC FREQ data = Alcool_C_Q3;
tables cluster*us5a_1*us5a_2*us5a_3
/chisq
;
title 'Distribution within clusters based on sex, age_group and wine consumption';
run;
*/

PROC FREQ data = Alcool_C_Q3;
tables cluster*us5a_1
/chisq
;
title 'Distribution within clusters - Pure with no ice';
run;

PROC FREQ data = Alcool_C_Q3;
tables cluster*us5a_2
/chisq
;
title 'Distribution within clusters - Pure with ice';
run;

PROC FREQ data = Alcool_C_Q3;
tables cluster*us5a_3
/chisq
;
title 'Distribution within clusters ';
run;

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* D : Régression logistique. */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------------------------------------------------------------------*/
/* 1. Faite une régression logistique pour calculer la chance d’appartenir à cette classe en 
fonction des variables suivantes : r2 clage us5a_1 us5a_2 us5a_3 r12 r13 r14. Qu’observer vous ? Commentez. */
/*----------------------------------------------------------------------------------------------------------------------------------------*/

data Alcool_D_Q1;
set Projet.log;

proc logistic data = Alcool_D_Q1 descending;
model classe5 = r2 clage us5a_1 us5a_2 us5a_3 r12 r13 r14;
output out = Alcool_pred pred = classe_pred;
title 'Logistic regression';
run;

/*
proc logistic data = Alcool_D_Q1 descending;
model classe5 = r2 clage us5a_1 us5a_2 us5a_3 r12 r13 r14 ia1_2 ia1_3 ia1_4 ia1_5 ia1_6 ia1_7 ia1_8 ia1_9 ia1_10 ia1_11 ia1_15 ia1_17 ia1_18 ia1_19 ia1_21 ia1_22 ia1_23 ia1_24 ia1_25 ia1_30 ia1_31 ia1_33 ia1_35 ia1_36 ia1_37;
output out = Alcool_pred pred = classe_pred;
title 'Logistic regression';
run;
*/








