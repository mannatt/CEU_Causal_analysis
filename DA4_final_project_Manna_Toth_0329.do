*** data cleaning
/*clear
import excel "/Users/manna/Desktop/Desktop/SCE2.xlsx", sheet("Data") firstrow

rename Time year
drop TimeCode 
rename GDPconstant2010USNYGDP gdp
rename LaborforcetotalSLTLFTOTL labor
rename UrbanpopulationoftotalS urb_pop
rename Schoolenrollmenttertiaryg enrollment
rename MediumandhightechIndustryi hightech

encode CountryCode, generate(country)
set more off
cap log using countrycodes.txt, text replace
 lab lis country
cap log close
set more on

xtset country year
xtset 
xtdes 
des country
codebook enrollment
save "SCE_cleaned_full"
clear 
use "SCE_cleaned_full"

keep year country enrollment labor gdp urb_pop
gen prod=gdp/labor

keep year country enrollment prod urb_pop
drop if enrollment==.
drop if prod==.

drop if year==2017
drop if year==1990
drop if year==1991
drop if year==1992
drop if year==1993
drop if year==1994
drop if year==1995
drop if year==1996
drop if year==1997
drop if year==1998
drop if year==1999

xtset country year
xtset 
xtdes 
des country

gen temp=1
egen T=sum(temp), by(country)
drop if T<16

//drop if year==2000
//drop if year==2001
//drop if year==2002
//drop if year==2003

drop T
drop temp
gen temp=1
egen T=sum(temp), by(country)
xtdes if T==16
des country 

//drop if year==2000
//drop if year==2016

export excel using "/Users/manna/Desktop/SCE0223.xls", firstrow(variables)
*********************************************************************************
*********************************************************************************
*Building full database
****** World Bank Country classification by income and resource dependence country index was added manually in excel (Vlookup)
* income classification: http://databank.worldbank.org/data/download/site-content/OGHIST.xls
* resource dependence classification: https://assets.aeaweb.org/assets/production/articles-attachments/jep/app/3001/30010161_app.pdf
clear
use "SCE_cleaned_full"
keep year CountryCode hightech
save "SCE_hightech.dta", replace

clear
import excel "/Users/manna/Desktop/SCE_resource_income.xls", sheet("Sheet1") firstrow
rename country CountryCode
save "SCE_resource_income.dta", replace
use "SCE_resource_income.dta"
merge 1:1 CountryCode year using "SCE_hightech.dta"
keep if _merge==3

encode CountryCode, generate(country)
set more off
cap log using countrycodes.txt, text replace
lab lis country
cap log close
set more on

xtset country year
xtset 
xtdes 
des country

ssc install mdesc
mdesc
*150 observation, 12,25% of hightech is missing
tab country if hightech==.
* UZB, SDN, LAO, CUB, BEN
tab year if hightech==.
* 2016: 70 countries missing

save "SCE_full.dta", replace
export excel using "/Users/manna/Desktop/SCE_full.xls", firstrow(variables)
*********************************************************************************
*********************************************************************************
*********************************************************************************/

*distribution of variables
clear
use "SCE_full.dta"
sort year

tabstat enrollment prod  ///
  s(mean min p10 p25 med p75 p90 max n) format(%5.0f) varw(8)

tabstat enrollment prod if year==2006, ///
  s(mean min p10 p25 med p75 p90 max n) format(%5.0f) varw(8)
  
gen lnprod=ln(prod)

hist lnprod, graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(white) ifcolor(white)) fcolor(dknavy) lcolor(ltblue) lpattern(blank) title("Log productivity")
graph save "lnprod", replace

hist prod, graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(white) ifcolor(white)) fcolor(dknavy) lcolor(ltblue) lpattern(blank) title("Distribution of productivity")
graph save "prod", replace

hist enrollment, fcolor(dknavy) lcolor(ltblue) lpattern(blank) title("Gross tertiary education enrollment ratio") graphregion(fcolor(white) ifcolor(none))  plotregion(fcolor(white) ifcolor(white))
graph save "enrollment", replace

sort enrollment
save "SCE_full.dta", replace
clear
use "SCE_full.dta"

collapse enrollment lnprod prod, by(year) // we take average of countries weighted by population
lab var enrollment "Average enrollment"
tabstat enrollment, by(year) s(mean n) // 1995 is minimum

 
tabstat prod, by(year) s(mean n) // 1995 is minimum
egen minlnprod=min(ln(prod))
 gen rellnprod=ln(prod)-minlnprod
 lab var rellnprod "ln prod  2000=0"
line enrollment year, lp(longdash dash) lw(thick thick) ytitle("change from 2000") ///
  || line rellnprod year , lw(thick) yaxis(2) ///
  title(Trends in yearly averages)



collapse hexppc lifeexp gdppc pop [w=pop], by(year) // we take average of countries weighted by population
lab var lifeexp "Average life expectancy"
tabstat lifeexp, by(year) s(mean n) // 1995 is minimum
egen minlnhex=min(ln(hex))
 gen rellnhex=ln(hex)-minlnhex 
 lab var rellnhex "ln health exp percap, 1995=0"
 
tabstat gdp, by(year) s(mean n) // 1995 is minimum
egen minlngdp=min(ln(gdp))
 gen rellngdp=ln(gdp)-minlngdp
 lab var rellngdp "ln GDP percap, 1995=0"
line rellnhex rellngdp year, lp(longdash dash) lw(thick thick) ytitle("ln change from 1995") ///
  || line lifeexp year , lw(thick) yaxis(2) ///
  title(Trends in yearly averages)

clear
use "SCE_full.dta"
  
  
  twoway lfit lnprod enrollment if year==2006, lw(vthick) lc(navy) legend(off) ///
 || scatter lnprod enrollment if year==2006, ///
 ms(X) msize(vlarge) mlw(thick) mcolor(orange) ///
 ytitle("Log productivity")   ///
  graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "ch22_1.png",replace


*********************************************************************************
*********************************************************************************
*********************************************************************************
 clear
use "SCE_full.dta"
 
 xtset country year
xtset 
xtdes 
des country
 
 
 
 
 *** 2. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(4)") 
  
 areg d.lnprod l(0/6).d.enrollment, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(4)") 
 
 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(11)")
 
 
 *** 2. First difference model, with time trend, 13 year lags
areg d.lnprod l(0/13).d.enrollment, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(13)") 
 
 *** 2. First difference model, with time trend, 14 year lags
areg d.lnprod l(0/14).d.enrollment, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(14)") 
 

*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year, fe cluster(country)
	outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FE") 
	
*** 4. Long dif
	
sort country year
//gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
//gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]



reg longDlnprod longDenroll, robust 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("LONG")

	
*** controll for urban_pop

 
 *** 2. First difference model, with time trend, 4 year lags
 
 areg d.lnprod l(0/4).d.enrollment urb_pop , absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(4)") 
 

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(11)")

 

*** 4. Long dif	
reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("LONG")
	
******** country dummies into FD and L
	
*** 2. First difference model, with time trend, 4 year lags 
 
 areg d.lnprod l(0/4).d.enrollment urb_pop i.country, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(4)") 
 

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop i.country, absorb(year) cluster(country) 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FD(11)")

 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("FE")
	
reg longDlnprod longDenroll urb_pop i.country, robust 
 outreg2 using "models_019.doc", se bdec(2) append tex text ctitle("LONG")
*********************************************************************************
*********************************************************************************
*********************************************************************************
*Heterogeinity test

clear
use "SCE_full.dta"
keep if income_cat_2000==1

 xtset country year
xtset 
xtdes 
des country
	
 
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) replace tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
//////////////////////////////////////////////////////////////////////////////

clear
use "SCE_full.dta"
keep if income_cat_2000==2

 xtset country year
xtset 
xtdes 
des country
	
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 
 
 
 //////////////////////////////////////////////////////////////////////////////

clear
use "SCE_full.dta"
tab hightech year if CountryCode=="HUN"

keep if income_cat_2000==3

 xtset country year
xtset 
xtdes 
des country
	
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 //////////////////////////////////////////////////////////////////////////////

clear
use "SCE_full.dta"
keep if income_cat_2000==4

 xtset country year
xtset 
xtdes 
des country
	
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")

 //////////////////////////////////////////////////////////////////////////////

clear
use "SCE_full.dta"
tab country
tab year hightech if country="HUN"
keep if res_dep==0

 xtset country year
xtset 
xtdes 
des country
	
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 //////////////////////////////////////////////////////////////////////////////
 
 clear
use "SCE_full.dta"
tab country res_dep
keep if res_dep==1

 xtset country year
xtset 
xtdes 
des country
	

   *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-16] if country==country[_n-16]
gen longDlnprod=lnprod-lnprod[_n-16] if country==country[_n-16]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 //////////////////////////////////////////////////////////////////////////////

clear
use "SCE_full.dta"
keep if year>=2009

 xtset country year
xtset 
xtdes 
des country
	

  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(4)")
 
   *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/5).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(4)")

 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-6] if country==country[_n-6]
gen longDlnprod=lnprod-lnprod[_n-6] if country==country[_n-6]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 //////////////////////////////////////////////////////////////////////////////
 
 
 
clear
use "SCE_full.dta"
keep if year<2009

 xtset country year
xtset 
xtdes 
des country
	
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(4)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/7).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(8)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-8] if country==country[_n-8]
gen longDlnprod=lnprod-lnprod[_n-8] if country==country[_n-8]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 //////////////////////////////////////////////////////////////////////////////
 
 
 
 
 clear
use "SCE_full.dta"
tab hightech
keep if hightech>29.9
drop if hightech==.

 xtset country year
xtset 
xtdes 
des country
	

	

  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-14] if country==country[_n-14]
gen longDlnprod=lnprod-lnprod[_n-14] if country==country[_n-14]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 //////////////////////////////////////////////////////////////////////////////
 

 clear
use "SCE_full.dta"
tab hightech
keep if hightech<29.9
drop if hightech==.

 xtset country year
xtset 
xtdes 
des country
	
 
  *** 1. First difference model, with time trend, 4 year lags
areg d.lnprod l(0/4).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(2)")

 *** 2. First difference model, with time trend, 11 year lags
areg d.lnprod l(0/11).d.enrollment urb_pop, absorb(year) cluster(country) 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FD(11)")
 
*** 3. Fixed effects model with time and country fixed effects
xtreg lnprod enrollment i.year urb_pop, fe cluster(country)
	outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("FE")

*** 4. Long
gen longDenroll=enrollment-enrollment[_n-15] if country==country[_n-15]
gen longDlnprod=lnprod-lnprod[_n-15] if country==country[_n-15]

 reg longDlnprod longDenroll urb_pop, robust 
 outreg2 using "models_ht.doc", se bdec(2) append tex text ctitle("LONG")
 
 
 ///////////////////////////////////////////////////////////////////////////////
 //////////////////////////////////////////////////////////////////////////////
 /////////////////////////////////////////////////////////////////////////////
 
 
 
 
 
 


keep year country prod TE_p CountryName

codebook TE_p
codebook prod

drop temp
drop T
gen temp=1
egen T=sum(temp), by(country)
drop if T<6
drop if T<7


xtdes
des country

codebook TE_p

save "WB_xy_merged_cleaned_TE.dta", replace
export excel using "/Users/manna/Desktop/WB_xy_merged_cleaned_country_TE.xls", firstrow(variables)

 


