* datamining-InformationValue-PiecewiseWOEVariables.sps
* Created:		05-Jul-2015
* Author:		Daniel Mastropietro
* Project:		WOE variables investigation
* Description:	Test of the newly implemented linearly interpolated WOE variables, whose SPSS code is implemented as part of the InformationValue() function in R.
* Ref:		datamining-functions.r
*

* Read the test dataset.
get file="E:\NATDesarrollo\Personales_Daniel_Mastropietro\Tools\data\BF-PE-PA_test_1ConsumoOtros_First1000obs.sav".
dataset name test.
show N.

* Check target frequency.
frequencies B_TARGET.

* Read the data into R.
begin program R.
test = spssdata.GetDataFromSPSS()
end program.

* Latest version of InformationValue() function.
begin program.
RSource("E:/NATDesarrollo/Personales_Daniel_Mastropietro/Tools/R")
end program.

begin program R.
iv = InformationValue(
	data=test,
	varclass="I_PA_AREA
I_PA_PRODUCTO",
	varnum="N_CLI_EDAD
N_PA_ANTIG
P_CLI_TC_DEUDA_v_CREDITO",
	target="B_TARGET",
	stat="median",
	groups=16,
	spsscode=TRUE,
	woeprefix="woe2_",
	woevartype="linear"
)
cat(iv$SPSS, sep="\n")
end program.

* Create the constant WOE variables.
if ( I_PA_AREA = 'FA' ) woe0_I_PA_AREA = 0.06897 . 
if ( I_PA_AREA = 'GO' ) woe0_I_PA_AREA = 0.9311 . 
if ( I_PA_AREA = 'OI' ) woe0_I_PA_AREA = -0.1286 . 
if ( I_PA_PRODUCTO = 1 ) woe0_I_PA_PRODUCTO = -0.07999 . 
if ( I_PA_PRODUCTO = 13 ) woe0_I_PA_PRODUCTO = -3.797 . 
if ( I_PA_PRODUCTO = 17 ) woe0_I_PA_PRODUCTO = 0.1208 . 
if ( I_PA_PRODUCTO = 48 ) woe0_I_PA_PRODUCTO = 0.7062 . 
if ( N_CLI_EDAD <= 23 ) woe0_N_CLI_EDAD = 0.2868 . 
if ( 23 < N_CLI_EDAD  and  N_CLI_EDAD <= 25 ) woe0_N_CLI_EDAD = 0.5753 . 
if ( 25 < N_CLI_EDAD  and  N_CLI_EDAD <= 26 ) woe0_N_CLI_EDAD = 0.3356 . 
if ( 26 < N_CLI_EDAD  and  N_CLI_EDAD <= 28 ) woe0_N_CLI_EDAD = 0.3521 . 
if ( 28 < N_CLI_EDAD  and  N_CLI_EDAD <= 29 ) woe0_N_CLI_EDAD = -0.3043 . 
if ( 29 < N_CLI_EDAD  and  N_CLI_EDAD <= 30 ) woe0_N_CLI_EDAD = 0.4441 . 
if ( 30 < N_CLI_EDAD  and  N_CLI_EDAD <= 31 ) woe0_N_CLI_EDAD = 0.6176 . 
if ( 31 < N_CLI_EDAD  and  N_CLI_EDAD <= 32 ) woe0_N_CLI_EDAD = -6.79 . 
if ( 32 < N_CLI_EDAD  and  N_CLI_EDAD <= 33 ) woe0_N_CLI_EDAD = 0.8815 . 
if ( 33 < N_CLI_EDAD  and  N_CLI_EDAD <= 35 ) woe0_N_CLI_EDAD = 0.1713 . 
if ( 35 < N_CLI_EDAD  and  N_CLI_EDAD <= 37 ) woe0_N_CLI_EDAD = -0.6261 . 
if ( 37 < N_CLI_EDAD  and  N_CLI_EDAD <= 39 ) woe0_N_CLI_EDAD = -0.2861 . 
if ( 39 < N_CLI_EDAD  and  N_CLI_EDAD <= 41 ) woe0_N_CLI_EDAD = -0.6774 . 
if ( 41 < N_CLI_EDAD  and  N_CLI_EDAD <= 46 ) woe0_N_CLI_EDAD = -2.224 . 
if ( 46 < N_CLI_EDAD  and  N_CLI_EDAD <= 52 ) woe0_N_CLI_EDAD = -0.2482 . 
if ( 52 < N_CLI_EDAD ) woe0_N_CLI_EDAD = -0.7446 . 
if ( N_PA_ANTIG <= 0 ) woe0_N_PA_ANTIG = -0.6647 . 
if ( 0 < N_PA_ANTIG  and  N_PA_ANTIG <= 1 ) woe0_N_PA_ANTIG = -0.241 . 
if ( 1 < N_PA_ANTIG  and  N_PA_ANTIG <= 2 ) woe0_N_PA_ANTIG = -0.4057 . 
if ( 2 < N_PA_ANTIG  and  N_PA_ANTIG <= 3 ) woe0_N_PA_ANTIG = -0.1686 . 
if ( 3 < N_PA_ANTIG  and  N_PA_ANTIG <= 4 ) woe0_N_PA_ANTIG = -0.1305 . 
if ( 4 < N_PA_ANTIG  and  N_PA_ANTIG <= 5 ) woe0_N_PA_ANTIG = 0.02108 . 
if ( 5 < N_PA_ANTIG  and  N_PA_ANTIG <= 6 ) woe0_N_PA_ANTIG = 0.2559 . 
if ( 6 < N_PA_ANTIG  and  N_PA_ANTIG <= 7 ) woe0_N_PA_ANTIG = 0.3645 . 
if ( 7 < N_PA_ANTIG  and  N_PA_ANTIG <= 8 ) woe0_N_PA_ANTIG = 0.342 . 
if ( 8 < N_PA_ANTIG  and  N_PA_ANTIG <= 10 ) woe0_N_PA_ANTIG = -0.3579 . 
if ( 10 < N_PA_ANTIG  and  N_PA_ANTIG <= 11 ) woe0_N_PA_ANTIG = 0.4448 . 
if ( 11 < N_PA_ANTIG  and  N_PA_ANTIG <= 14 ) woe0_N_PA_ANTIG = 0.3572 . 
if ( 14 < N_PA_ANTIG ) woe0_N_PA_ANTIG = 0.5017 . 
if ( P_CLI_TC_DEUDA_v_CREDITO <= 0 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = -0.4352 . 
if ( 0 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 0.676 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = 0.1529 . 
if ( 0.676 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 23.5 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = -5.74 . 
if ( 23.5 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 48.8 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = -2.191 . 
if ( 48.8 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 71.5 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = -6.823 . 
if ( 71.5 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 87.3 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = 0.8327 . 
if ( 87.3 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 95.6 ) woe0_P_CLI_TC_DEUDA_v_CREDITO = 0.2429 . 
if ( 95.6 < P_CLI_TC_DEUDA_v_CREDITO ) woe0_P_CLI_TC_DEUDA_v_CREDITO = 0.262 .

* Create the LINEAR WOE variables on the mean.
if ( I_PA_AREA = 'FA' ) woe_I_PA_AREA = 0.06897 . 
if ( I_PA_AREA = 'GO' ) woe_I_PA_AREA = 0.9311 . 
if ( I_PA_AREA = 'OI' ) woe_I_PA_AREA = -0.1286 . 
if ( I_PA_PRODUCTO = 1 ) woe_I_PA_PRODUCTO = -0.07999 . 
if ( I_PA_PRODUCTO = 13 ) woe_I_PA_PRODUCTO = -3.797 . 
if ( I_PA_PRODUCTO = 17 ) woe_I_PA_PRODUCTO = 0.1208 . 
if ( I_PA_PRODUCTO = 48 ) woe_I_PA_PRODUCTO = 0.7062 . 
if ( N_CLI_EDAD <= 22.12 ) woe_N_CLI_EDAD = 0.2868 . 
if ( 22.12 < N_CLI_EDAD  and  N_CLI_EDAD <= 24.5 ) woe_N_CLI_EDAD = 0.2868 + 0.121218487394958 * ( N_CLI_EDAD - 22.12 ). 
if ( 24.5 < N_CLI_EDAD  and  N_CLI_EDAD <= 26 ) woe_N_CLI_EDAD = 0.5753 + -0.1598 * ( N_CLI_EDAD - 24.5 ). 
if ( 26 < N_CLI_EDAD  and  N_CLI_EDAD <= 27.52 ) woe_N_CLI_EDAD = 0.3356 + 0.0108552631578947 * ( N_CLI_EDAD - 26 ). 
if ( 27.52 < N_CLI_EDAD  and  N_CLI_EDAD <= 29 ) woe_N_CLI_EDAD = 0.3521 + -0.443513513513513 * ( N_CLI_EDAD - 27.52 ). 
if ( 29 < N_CLI_EDAD  and  N_CLI_EDAD <= 30 ) woe_N_CLI_EDAD = -0.3043 + 0.7484 * ( N_CLI_EDAD - 29 ). 
if ( 30 < N_CLI_EDAD  and  N_CLI_EDAD <= 31 ) woe_N_CLI_EDAD = 0.4441 + 0.1735 * ( N_CLI_EDAD - 30 ). 
if ( 31 < N_CLI_EDAD  and  N_CLI_EDAD <= 32 ) woe_N_CLI_EDAD = 0.6176 + -7.4076 * ( N_CLI_EDAD - 31 ). 
if ( 32 < N_CLI_EDAD  and  N_CLI_EDAD <= 33 ) woe_N_CLI_EDAD = -6.79 + 7.6715 * ( N_CLI_EDAD - 32 ). 
if ( 33 < N_CLI_EDAD  and  N_CLI_EDAD <= 34.56 ) woe_N_CLI_EDAD = 0.8815 + -0.455256410256410 * ( N_CLI_EDAD - 33 ). 
if ( 34.56 < N_CLI_EDAD  and  N_CLI_EDAD <= 36.63 ) woe_N_CLI_EDAD = 0.1713 + -0.385217391304348 * ( N_CLI_EDAD - 34.56 ). 
if ( 36.63 < N_CLI_EDAD  and  N_CLI_EDAD <= 38.44 ) woe_N_CLI_EDAD = -0.6261 + 0.187845303867404 * ( N_CLI_EDAD - 36.63 ). 
if ( 38.44 < N_CLI_EDAD  and  N_CLI_EDAD <= 40.37 ) woe_N_CLI_EDAD = -0.2861 + -0.202746113989637 * ( N_CLI_EDAD - 38.44 ). 
if ( 40.37 < N_CLI_EDAD  and  N_CLI_EDAD <= 43.59 ) woe_N_CLI_EDAD = -0.6774 + -0.48031055900621 * ( N_CLI_EDAD - 40.37 ). 
if ( 43.59 < N_CLI_EDAD  and  N_CLI_EDAD <= 48.66 ) woe_N_CLI_EDAD = -2.224 + 0.389704142011835 * ( N_CLI_EDAD - 43.59 ). 
if ( 48.66 < N_CLI_EDAD  and  N_CLI_EDAD <= 59.36 ) woe_N_CLI_EDAD = -0.2482 + -0.046392523364486 * ( N_CLI_EDAD - 48.66 ). 
if ( 59.36 < N_CLI_EDAD ) woe_N_CLI_EDAD = -0.7446 . 
if ( N_PA_ANTIG <= 0 ) woe_N_PA_ANTIG = -0.6647 . 
if ( 0 < N_PA_ANTIG  and  N_PA_ANTIG <= 1 ) woe_N_PA_ANTIG = -0.6647 + 0.4237 * ( N_PA_ANTIG - 0 ). 
if ( 1 < N_PA_ANTIG  and  N_PA_ANTIG <= 2 ) woe_N_PA_ANTIG = -0.241 + -0.1647 * ( N_PA_ANTIG - 1 ). 
if ( 2 < N_PA_ANTIG  and  N_PA_ANTIG <= 3 ) woe_N_PA_ANTIG = -0.4057 + 0.2371 * ( N_PA_ANTIG - 2 ). 
if ( 3 < N_PA_ANTIG  and  N_PA_ANTIG <= 4 ) woe_N_PA_ANTIG = -0.1686 + 0.0381 * ( N_PA_ANTIG - 3 ). 
if ( 4 < N_PA_ANTIG  and  N_PA_ANTIG <= 5 ) woe_N_PA_ANTIG = -0.1305 + 0.15158 * ( N_PA_ANTIG - 4 ). 
if ( 5 < N_PA_ANTIG  and  N_PA_ANTIG <= 6 ) woe_N_PA_ANTIG = 0.02108 + 0.23482 * ( N_PA_ANTIG - 5 ). 
if ( 6 < N_PA_ANTIG  and  N_PA_ANTIG <= 7 ) woe_N_PA_ANTIG = 0.2559 + 0.1086 * ( N_PA_ANTIG - 6 ). 
if ( 7 < N_PA_ANTIG  and  N_PA_ANTIG <= 8 ) woe_N_PA_ANTIG = 0.3645 + -0.0225000000000000 * ( N_PA_ANTIG - 7 ). 
if ( 8 < N_PA_ANTIG  and  N_PA_ANTIG <= 9.427 ) woe_N_PA_ANTIG = 0.342 + -0.490469516468115 * ( N_PA_ANTIG - 8 ). 
if ( 9.427 < N_PA_ANTIG  and  N_PA_ANTIG <= 11 ) woe_N_PA_ANTIG = -0.3579 + 0.510298792116974 * ( N_PA_ANTIG - 9.427 ). 
if ( 11 < N_PA_ANTIG  and  N_PA_ANTIG <= 12.86 ) woe_N_PA_ANTIG = 0.4448 + -0.0470967741935484 * ( N_PA_ANTIG - 11 ). 
if ( 12.86 < N_PA_ANTIG  and  N_PA_ANTIG <= 16.96 ) woe_N_PA_ANTIG = 0.3572 + 0.0352439024390244 * ( N_PA_ANTIG - 12.86 ). 
if ( 16.96 < N_PA_ANTIG ) woe_N_PA_ANTIG = 0.5017 . 
if ( P_CLI_TC_DEUDA_v_CREDITO <= -0.007789 ) woe_P_CLI_TC_DEUDA_v_CREDITO = -0.4352 . 
if ( -0.007789 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 0.6634 ) woe_P_CLI_TC_DEUDA_v_CREDITO = -0.4352 + 0.876206254870089 * ( P_CLI_TC_DEUDA_v_CREDITO - -0.007789 ). 
if ( 0.6634 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 15.87 ) woe_P_CLI_TC_DEUDA_v_CREDITO = 0.1529 + -0.387522523114963 * ( P_CLI_TC_DEUDA_v_CREDITO - 0.6634 ). 
if ( 15.87 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 37.51 ) woe_P_CLI_TC_DEUDA_v_CREDITO = -5.74 + 0.164001848428836 * ( P_CLI_TC_DEUDA_v_CREDITO - 15.87 ). 
if ( 37.51 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 61.52 ) woe_P_CLI_TC_DEUDA_v_CREDITO = -2.191 + -0.192919616826322 * ( P_CLI_TC_DEUDA_v_CREDITO - 37.51 ). 
if ( 61.52 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 80.6 ) woe_P_CLI_TC_DEUDA_v_CREDITO = -6.823 + 0.40124213836478 * ( P_CLI_TC_DEUDA_v_CREDITO - 61.52 ). 
if ( 80.6 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 92.09 ) woe_P_CLI_TC_DEUDA_v_CREDITO = 0.8327 + -0.051331592689295 * ( P_CLI_TC_DEUDA_v_CREDITO - 80.6 ). 
if ( 92.09 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 104.1 ) woe_P_CLI_TC_DEUDA_v_CREDITO = 0.2429 + 0.00159034138218152 * ( P_CLI_TC_DEUDA_v_CREDITO - 92.09 ). 
if ( 104.1 < P_CLI_TC_DEUDA_v_CREDITO ) woe_P_CLI_TC_DEUDA_v_CREDITO = 0.262 .

* Create the linear interpolated WOE variables on the median.
if ( I_PA_AREA = 'FA' ) woe2_I_PA_AREA = 0.06897 . 
if ( I_PA_AREA = 'GO' ) woe2_I_PA_AREA = 0.9311 . 
if ( I_PA_AREA = 'OI' ) woe2_I_PA_AREA = -0.1286 . 
if ( I_PA_PRODUCTO = 1 ) woe2_I_PA_PRODUCTO = -0.07999 . 
if ( I_PA_PRODUCTO = 13 ) woe2_I_PA_PRODUCTO = -3.797 . 
if ( I_PA_PRODUCTO = 17 ) woe2_I_PA_PRODUCTO = 0.1208 . 
if ( I_PA_PRODUCTO = 48 ) woe2_I_PA_PRODUCTO = 0.7062 . 
if ( N_CLI_EDAD <= 22 ) woe2_N_CLI_EDAD = 0.2868 . 
if ( 22 < N_CLI_EDAD  and  N_CLI_EDAD <= 24.5 ) woe2_N_CLI_EDAD = 0.2868 + 0.1154 * ( N_CLI_EDAD - 22 ). 
if ( 24.5 < N_CLI_EDAD  and  N_CLI_EDAD <= 26 ) woe2_N_CLI_EDAD = 0.5753 + -0.1598 * ( N_CLI_EDAD - 24.5 ). 
if ( 26 < N_CLI_EDAD  and  N_CLI_EDAD <= 28 ) woe2_N_CLI_EDAD = 0.3356 + 0.00825 * ( N_CLI_EDAD - 26 ). 
if ( 28 < N_CLI_EDAD  and  N_CLI_EDAD <= 29 ) woe2_N_CLI_EDAD = 0.3521 + -0.6564 * ( N_CLI_EDAD - 28 ). 
if ( 29 < N_CLI_EDAD  and  N_CLI_EDAD <= 30 ) woe2_N_CLI_EDAD = -0.3043 + 0.7484 * ( N_CLI_EDAD - 29 ). 
if ( 30 < N_CLI_EDAD  and  N_CLI_EDAD <= 31 ) woe2_N_CLI_EDAD = 0.4441 + 0.1735 * ( N_CLI_EDAD - 30 ). 
if ( 31 < N_CLI_EDAD  and  N_CLI_EDAD <= 32 ) woe2_N_CLI_EDAD = 0.6176 + -7.4076 * ( N_CLI_EDAD - 31 ). 
if ( 32 < N_CLI_EDAD  and  N_CLI_EDAD <= 33 ) woe2_N_CLI_EDAD = -6.79 + 7.6715 * ( N_CLI_EDAD - 32 ). 
if ( 33 < N_CLI_EDAD  and  N_CLI_EDAD <= 35 ) woe2_N_CLI_EDAD = 0.8815 + -0.3551 * ( N_CLI_EDAD - 33 ). 
if ( 35 < N_CLI_EDAD  and  N_CLI_EDAD <= 37 ) woe2_N_CLI_EDAD = 0.1713 + -0.3987 * ( N_CLI_EDAD - 35 ). 
if ( 37 < N_CLI_EDAD  and  N_CLI_EDAD <= 38 ) woe2_N_CLI_EDAD = -0.6261 + 0.34 * ( N_CLI_EDAD - 37 ). 
if ( 38 < N_CLI_EDAD  and  N_CLI_EDAD <= 40 ) woe2_N_CLI_EDAD = -0.2861 + -0.19565 * ( N_CLI_EDAD - 38 ). 
if ( 40 < N_CLI_EDAD  and  N_CLI_EDAD <= 43 ) woe2_N_CLI_EDAD = -0.6774 + -0.515533333333333 * ( N_CLI_EDAD - 40 ). 
if ( 43 < N_CLI_EDAD  and  N_CLI_EDAD <= 48 ) woe2_N_CLI_EDAD = -2.224 + 0.39516 * ( N_CLI_EDAD - 43 ). 
if ( 48 < N_CLI_EDAD  and  N_CLI_EDAD <= 59 ) woe2_N_CLI_EDAD = -0.2482 + -0.0451272727272727 * ( N_CLI_EDAD - 48 ). 
if ( 59 < N_CLI_EDAD ) woe2_N_CLI_EDAD = -0.7446 . 
if ( N_PA_ANTIG <= 0 ) woe2_N_PA_ANTIG = -0.6647 . 
if ( 0 < N_PA_ANTIG  and  N_PA_ANTIG <= 1 ) woe2_N_PA_ANTIG = -0.6647 + 0.4237 * ( N_PA_ANTIG - 0 ). 
if ( 1 < N_PA_ANTIG  and  N_PA_ANTIG <= 2 ) woe2_N_PA_ANTIG = -0.241 + -0.1647 * ( N_PA_ANTIG - 1 ). 
if ( 2 < N_PA_ANTIG  and  N_PA_ANTIG <= 3 ) woe2_N_PA_ANTIG = -0.4057 + 0.2371 * ( N_PA_ANTIG - 2 ). 
if ( 3 < N_PA_ANTIG  and  N_PA_ANTIG <= 4 ) woe2_N_PA_ANTIG = -0.1686 + 0.0381 * ( N_PA_ANTIG - 3 ). 
if ( 4 < N_PA_ANTIG  and  N_PA_ANTIG <= 5 ) woe2_N_PA_ANTIG = -0.1305 + 0.15158 * ( N_PA_ANTIG - 4 ). 
if ( 5 < N_PA_ANTIG  and  N_PA_ANTIG <= 6 ) woe2_N_PA_ANTIG = 0.02108 + 0.23482 * ( N_PA_ANTIG - 5 ). 
if ( 6 < N_PA_ANTIG  and  N_PA_ANTIG <= 7 ) woe2_N_PA_ANTIG = 0.2559 + 0.1086 * ( N_PA_ANTIG - 6 ). 
if ( 7 < N_PA_ANTIG  and  N_PA_ANTIG <= 8 ) woe2_N_PA_ANTIG = 0.3645 + -0.0225000000000000 * ( N_PA_ANTIG - 7 ). 
if ( 8 < N_PA_ANTIG  and  N_PA_ANTIG <= 9 ) woe2_N_PA_ANTIG = 0.342 + -0.6999 * ( N_PA_ANTIG - 8 ). 
if ( 9 < N_PA_ANTIG  and  N_PA_ANTIG <= 11 ) woe2_N_PA_ANTIG = -0.3579 + 0.40135 * ( N_PA_ANTIG - 9 ). 
if ( 11 < N_PA_ANTIG  and  N_PA_ANTIG <= 13 ) woe2_N_PA_ANTIG = 0.4448 + -0.0438 * ( N_PA_ANTIG - 11 ). 
if ( 13 < N_PA_ANTIG  and  N_PA_ANTIG <= 16 ) woe2_N_PA_ANTIG = 0.3572 + 0.0481666666666667 * ( N_PA_ANTIG - 13 ). 
if ( 16 < N_PA_ANTIG ) woe2_N_PA_ANTIG = 0.5017 . 
if ( P_CLI_TC_DEUDA_v_CREDITO <= 0 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = -0.4352 . 
if ( 0 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 0.6759 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = -0.4352 + 0.870099127089806 * ( P_CLI_TC_DEUDA_v_CREDITO - 0 ). 
if ( 0.6759 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 16.24 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = 0.1529 + -0.378621314435142 * ( P_CLI_TC_DEUDA_v_CREDITO - 0.6759 ). 
if ( 16.24 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 37.64 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = -5.74 + 0.165841121495327 * ( P_CLI_TC_DEUDA_v_CREDITO - 16.24 ). 
if ( 37.64 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 61.82 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = -2.191 + -0.191563275434243 * ( P_CLI_TC_DEUDA_v_CREDITO - 37.64 ). 
if ( 61.82 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 81.02 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = -6.823 + 0.398734375 * ( P_CLI_TC_DEUDA_v_CREDITO - 61.82 ). 
if ( 81.02 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 92.24 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = 0.8327 + -0.0525668449197861 * ( P_CLI_TC_DEUDA_v_CREDITO - 81.02 ). 
if ( 92.24 < P_CLI_TC_DEUDA_v_CREDITO  and  P_CLI_TC_DEUDA_v_CREDITO <= 103 ) woe2_P_CLI_TC_DEUDA_v_CREDITO = 0.2429 + 0.00177509293680297 * ( P_CLI_TC_DEUDA_v_CREDITO - 92.24 ). 
if ( 103 < P_CLI_TC_DEUDA_v_CREDITO ) woe2_P_CLI_TC_DEUDA_v_CREDITO = 0.262 .

* Update the test dataset copy in R.
begin program R.
test = spssdata.GetDataFromSPSS()
end program.

* Make the comparison graphs.
begin program R.
vars = unlist(strsplit("N_CLI_EDAD N_PA_ANTIG P_CLI_TC_DEUDA_v_CREDITO", " "))
for (v in vars) {
	# Variable names
	woe0v = paste("woe0_", v, sep="")
	woev = paste("woe_", v, sep="")
	woe2v = paste("woe2_", v, sep="")

	# Define what variable to plot and what statistic was used for its computation
	plotv = woe2v
	stat = "median"

	X = cbind(test[,c(v, woe0v, woev, woe2v)])
	# Ascending order of x values
	ord = order(X[,v])

	# Bin centers
	ind = iv$WOE$var==v
	xvalues = iv$WOE[ind, stat]
	# WOE of each bin
	woevalues = iv$WOE[ind, "woe"]
	cat("Variable", v, "\n")
	print(cbind(xvalues, woevalues, slopes=c(diff(woevalues) / diff(xvalues), NA)))

	# Get the bin upper bounds
	bounds = extract(iv$WOE[iv$WOE$var==v,"group"], what="upper")

	# Plot both WOE variables
	# IMPORTANT NOTE: The linear interpolated WOE may not exactly pass through the constant WOE value at the bin center
	# and this is because the variable may not take values that pass through the center value for instance.
	# Using the median as representative value of each bin, the above does NOT happen!
	plot(X[ord, v], X[ord, plotv], type="l", col="blue", xlab=v, ylab=paste(woe0v, plotv, sep=" / "))
	points(xvalues, woevalues, pch=21, col="blue", bg="blue")
	lines(X[ord, v], X[ord, woe0v], type="l", col="red")
	# right-bound of the bin
	abline(v=bounds, lty=2, col="black")
	# center value of the bin
	abline(v=xvalues, lty=2, col="green")
}
end program.
