* PrepareTestData.sps
* Created:		22-Aug-2016
* Author:		Daniel Mastropietro
* Project:		Test R and SPSS Tools
* Description:	Prepare data for testing
*

* Prepare a test dataset from the Banco Familiar project.
get file="E:\NATDesarrollo\BancoFamiliar-py\PerdidaEsperada\B-Datos\BF-PE-PA_test_1ConsumoOtros.sav".
dataset name test.
show N.

* Select a small sample for the tests.
dataset activate test.
select if $CASENUM <= 1000.
execute.

save outfile="E:\NATDesarrollo\Personales_Daniel_Mastropietro\Tools\data\BF-PE-PA_test_1ConsumoOtros_First1000obs.sav".
