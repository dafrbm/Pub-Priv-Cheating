*************************************************************************
*Power Calculations: Public and Private Cheating*
*Estimando efectos de controles con datos reales*
*Elaborado por: David Becerra*
*************************************************************************

cls

clear all

* Macros para directorios de trabajo

global Data "C:\Users\Lenovo\OneDrive - Universidad de los andes\TREES\public and private cheating\power calculations\data"

global Analysis "C:\Users\Lenovo\OneDrive - Universidad de los andes\TREES\public and private cheating\power calculations\analysis"

global Results "C:\Users\Lenovo\OneDrive - Universidad de los andes\TREES\public and private cheating\power calculations\results"

/*REGRESIONES

Vamos a probar una especificación con 3 sets de datos diferentes.

Encuesta de Cultura Política - DANE

LAPOP

World Values Survey

La idea es probar regresiones de la forma:

Percepción de corrupción = Edad + Sexo + Educación + Ingresos

*/

*1. ECP

*Cargar y pegar módulos de ECP

use "$Data/ECP/Caracteristicas generales.dta", clear

merge 1:1 DIRECTORIO NRO_ENCUESTA HOGAR_NUMERO PERSONA_NUMERO using "$Data/ECP/Democracia.dta", gen(_merge)

merge m:1 DIRECTORIO using "$Data/ECP/Viviendas.dta", gen(_merge2)

drop if _merge == 1

drop _merge _merge2

*Filtrando variables

keep P4031S1A1 FEX_P P220 P5785 P6210 P2016S1 P2016S2 P2016S3 P2016S4 P2016S5 P2016S6 P2016S7 P2016S8 P2016S9

drop if P2016S1 == 99

rename (P4031S1A1 P220 P5785 P6210 P2016S1 P2016S2 P2016S3 P2016S4 P2016S5 P2016S6 P2016S7 P2016S8 P2016S9 ) (estrato sex age educ cor_gobnac cor_gobdep cor_gobmun cor_emp cor_orgcon cor_cong cor_jud cor_mil cor_pol) 

drop if educ == 99

drop if age >85

drop if estrato == 0 | estrato == 9

save "$Data/ECP/ECP_power.dta", replace

*Arreglando variables de corrupción

codebook cor_gobnac cor_gobdep cor_gobmun cor_emp cor_orgcon cor_cong cor_jud cor_mil cor_pol

foreach v of varlist cor_gobnac cor_gobdep cor_gobmun cor_emp cor_orgcon cor_cong cor_jud cor_mil cor_pol {
	
	replace `v' = . if `v' == 99
	
}

*Mirando algunas descriptivas y revisando un índice

mean cor_gobnac cor_gobdep cor_gobmun cor_emp cor_orgcon cor_cong cor_jud cor_mil cor_pol [pw=FEX_P]

gen cor_sum = cor_gobnac + cor_gobdep + cor_gobmun + cor_emp + cor_orgcon + cor_cong + cor_jud + cor_mil + cor_pol

gen index = cor_sum/9

save "$Data/ECP/ECP_power.dta", replace

sum index

histogram index

foreach v of varlist cor_gobnac cor_gobdep cor_gobmun cor_emp cor_orgcon cor_cong cor_jud cor_mil cor_pol index {
	
	histogram `v', discrete normal subtitle("Histogram of `v'") 
	
	graph export "$Results/ECP_`v'.png", replace
	
}

*Estandarizar el índice y observar desempeño

egen std_index = std(index)

sum std_index

histogram std_index, normal

*Regresiones

use "$Data/ECP/ECP_power.dta", clear

h egen

reg index age i.sex i.educ i.estrato [pw=FEX_P]
reg std_index age i.sex i.educ i.estrato [pw=FEX_P]
estimates store ECP_1

outreg2 ECP_1 using myfile, excel

*2. LAPOP

*Cargar y pegar módulos de ECP

use "$Data/LAPOP/lapop_col.dta", clear

*Filtrando variables

keep idnum uniq_id cluster upm wt estratopri q1 q2 exc7 exc7new q10g q10new q10e q12c ed

rename (idnum uniq_id cluster upm wt estratopri q1 q2 exc7 exc7new q10g q10new q10e q12c ed) (idnum uniq_id cluster upm wt estratopri sex age corrupt_bur corrupt_pol pers_inc hh_inc l2y_inc hh_pnum educ) 

codebook

*Corriendo regresiones

reg corrupt_pol age i.sex educ hh_inc
estimates store LAPOP_1

outreg2 LAPOP_1 using myfile, excel

*2. WVS

*Cargar y pegar módulos de ECP

use "$Data/WVS/wvs_col.dta", clear

*Filtrando variables

keep Q112 Q113 Q114 Q115 Q116 Q117 Q260 Q262 Q270 Q275 Q275R Q288

rename (Q112 Q113 Q114 Q115 Q116 Q117 Q260 Q262 Q270 Q275 Q275R Q288) (corrp_country corrp_stateauth corrp_exec corrp_locauth corrp_civprov corrp_media sex age hh_ppl educ educ_rec hh_inc) 

codebook

drop if educ<0

*Corriendo regresiones

reg corrp_country age i.sex i.educ i.hh_inc
estimates store WVS_1

outreg2 WVS_1 using myfile, excel