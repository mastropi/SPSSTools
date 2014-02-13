import spss
import spssaux

### Variables globales del modulo. Basicamente usadas en las funciones de Regresion
regrVarDep = ''
regrVarIndep = []
regrCatList = []
regrPRED = ''
regrRESID = ''
lastVariables = []
lastCategoricas = []
varWeight= ''
valorPonderacion=1
numList = []
nomList = []
ordList = []
noEvaluarList = []

###################################################################################################
############                                                                                       
############    *DeleteVariables(varsADeletear, **kwargs)                                          
############          kwargs: CMD                                                                  
############          varsADeletear:  ['var1 var2']          sin ',' entre las variables           
############                                                                                       
###################################################################################################
def pyDeleteVariables(varsADeletear, **kwargs):
    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]
    listaABorrar = [var for var in varsADeletear[0].split(' ') if var in fullList]

    if len(listaABorrar) > 0:
        cmd = 'DELETE VARIABLES ' + ' '.join(listaABorrar) + '.\nEXECUTE.\n'
        if 'CMD' in kwargs:
            if kwargs['CMD'] > 0:
                print(cmd)
            if kwargs['CMD'] < 2:
                spss.Submit(cmd)
            elif kwargs['CMD'] == 2:
                print fullList
                print listaABorrar
        else:
            spss.Submit(cmd)

###################################################################################################
############                                                                                       
############    *VarLabelsInicial()                                                                
############          kwargs: ninguna                                                              
############                                                                                       
###################################################################################################
def pyVarLabelsInicial():
    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]
    for var in fullList:
        spss.Submit("VARIABLE LABELS " + var + " '" + var + "'.")
    

################################################################################################### 
############                                                                                        
############    *ListasVarOriginales(**kwargs)                      GENERAR                         
############          kwargs: VER          Por default solo genera la lista, se puede pedir el print
############          valPondera: numero                                                            
############           Solamente deberia llamarse inmediatamente despues de cargado el dataset      
############                                                                                        
###################################################################################################
def pyListasVarOriginales(**kwargs):
    global numList
    global nomList
    global ordList
    global noEvaluarList

    # Solamente deberia llamarse inmediatamente despues de cargado el dataset, a fin de tener la lista realmente original
    numList = []
    nomList = []
    ordList = []
    noEvaluarList = ['id', 'CUITn', 'MesEval', 'Cotiz', 'Sucursal', 'Outliers', 'MarcaMoraBinaria', 'Maduro', 'SeleccionAnalisisCriterio1', 'SeleccionAnalisisCriterio2', 'SeleccionAnalisisCriterio3']

    varCount = spss.GetVariableCount()
    for i in xrange(varCount):
        tipo = spss.GetVariableMeasurementLevel(i)

        if tipo == 'scale':
            numList.append(spss.GetVariableName(i))
        elif tipo == 'nominal':
            nomList.append(spss.GetVariableName(i))
        elif tipo == 'ordinal':
            ordList.append(spss.GetVariableName(i))

    if 'VER' in kwargs:
        pyVerListasVarOriginales(**kwargs)

###################################################################################################   
############                                                                                          
############    *VerListasVarOriginales(**kwargs)                                                     
############          kwargs: VER          Se pueden ver las continuas, o las categ, o ambas (1, 2, 3)
############          valPondera: numero                                                              
############           Solamente deberia llamarse inmediatamente despues de cargado el dataset        
############                                                                                          
###################################################################################################
def pyVerListasVarOriginales(**kwargs):
    if 'VER' in kwargs:
        if kwargs['VER'] == 1 or kwargs['VER'] == 2:
            print numList
        if kwargs['VER'] > 1:
            print nomList
            print ordList

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    ESTUDIO VARIABLES   #
###################################################################################################   
############                                                                                          
############    *Histograma(varEstudiada, **kwargs)                                           
############          kwargs: CMD            BINCOUNT: cant bins histograma                           
############                      LIMITMINABS y LIMITSUPABS: fuerza limites en el eje de abscisas                   OJO: esta cableada para VISION.              F A L T A   T E R M I N A R
############                      EJELOG: eje abs. logaritmico    LIMITORD: limite sup. eje ordenadas 
############                      NORMAL   RANK                                                       
############                                                                                          
###################################################################################################
def pyHistograma(varEstudiada, **kwargs):


    bins = ''
    if 'BINCOUNT' in kwargs:
        bins = ', binCount(' + str(kwargs['BINCOUNT']) + ')'

    cmd = '''SUMMARIZE
  /TABLES=''' + varEstudiada + ''' BY tOperacion BY tTipoEmpresa
  /FORMAT=NOLIST TOTAL
  /TITLE='Resumenes de casos'
  /MISSING=VARIABLE
  /CELLS=COUNT MEAN MEDIAN GMEDIAN MIN MAX STDDEV.

GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=''' + varEstudiada + ''' tOperacion[LEVEL=NOMINAL] 
    tTipoEmpresa[LEVEL=NOMINAL] MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ''' + varEstudiada + '''=col(source(s), name("''' + varEstudiada + '''"))
  DATA: tOperacion=col(source(s), name("tOperacion"), unit.category())
  DATA: tTipoEmpresa=col(source(s), name("tTipoEmpresa"), unit.category())
  GUIDE: axis(dim(1), label("''' + varEstudiada + '''"))
  GUIDE: axis(dim(2), label("Frecuencia"))
  GUIDE: axis(dim(3), label("tOperacion"), opposite())
  GUIDE: axis(dim(4), label("tTipoEmpresa"), opposite())
  SCALE: cat(dim(3), include("1.00", "2.00"))
  SCALE: cat(dim(4), include("1.00", "2.00", "3.00"), sort.values("1.00", "2.00", "3.00"))
  ELEMENT: interval(position(summary.count(bin.rect(''' + varEstudiada + '''*1*tOperacion*tTipoEmpresa''' + bins + '''))), 
    shape.interior(shape.square))
  ELEMENT: line(position(density.normal(''' + varEstudiada + '''*1*tOperacion*tTipoEmpresa)), color.interior(color.blue))
'''


    if 'EJELOG' in kwargs:
        cmd = cmd + '  SCALE: log(dim(1), base(10)'
    else:
        cmd = cmd + '  SCALE: linear(dim(1)'

    if 'LIMITMINABS' in kwargs:
        cmd = cmd + ', min(' + str(kwargs['LIMITMINABS']) + ')'
    elif 'EJELOG' in kwargs:
        cmd = cmd + ', min(0.01)'

    if 'LIMITSUPABS' in kwargs:
        cmd = cmd + ', max(' + str(kwargs['LIMITSUPABS']) + ')'

    cmd = cmd + ')\n'

    if 'LIMITORD' in kwargs:
        cmd = cmd + '  SCALE: linear(dim(2), max(' + str(kwargs['LIMITORD']) + '))\n'
        
    cmd = cmd + 'END GPL.\n'
    

    pyExec(cmd, **kwargs)
###################################################################################################   
############                                                                                          
############    *HistogrAbierto(varIndep, varDep, **kwargs)                                           
############          kwargs: CMD            BINCOUNT: cant bins histograma                           
############                      LIMITMINABS y LIMITSUPABS: fuerza limites en el eje de abscisas     
############                      EJELOG: eje abs. logaritmico    LIMITORD: limite sup. eje ordenadas 
############                      NORMAL   RANK                                                       
############                                                                                          
###################################################################################################
def pyHistogrAbierto(varIndep, varDep, **kwargs):

    labelLog1 = ''
    labelLog2 = ''

    cmd = ''

    if 'EJELOG' in kwargs:
        if kwargs['EJELOG']==1:
            labelLog1 = 'LOG('
            labelLog2 = ')'
            cmd = cmd + 'TEMPORARY.\nSELECT IF (' + varIndep + ' > 0).\n\n'

    bins = ''
    if 'BINCOUNT' in kwargs:
        bins = ', binCount(' + str(kwargs['BINCOUNT']) + ')'

    cmd = cmd + '''
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=''' + varIndep + ''' ''' + varDep + '''[LEVEL=NOMINAL] 
    MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ''' + varIndep + '''=col(source(s), name("''' + varIndep + '''"))
  DATA: ''' + varDep + '''=col(source(s), name("''' + varDep + '''"), unit.category())
  COORD: transpose(mirror(rect(dim(1,2))))
  GUIDE: axis(dim(1), label("''' + labelLog1 + varIndep + labelLog2 + '''"))
  GUIDE: axis(dim(1), opposite(), label("''' + labelLog1 + varIndep + labelLog2 + '''"))
  GUIDE: axis(dim(2), label("Frecuencia"))
  GUIDE: axis(dim(3), label("''' + varDep + '''"), opposite(), gap(0px))
  GUIDE: legend(aesthetic(aesthetic.color), null())
  '''

    if 'EJELOG' in kwargs:
        cmd = cmd + '  SCALE: log(dim(1), base(10)'
    else:
        cmd = cmd + '  SCALE: linear(dim(1)'

    if 'LIMITMINABS' in kwargs:
        cmd = cmd + ', min(' + str(kwargs['LIMITMINABS']) + ')'
    elif 'EJELOG' in kwargs:
        cmd = cmd + ', min(0.01)'

    if 'LIMITSUPABS' in kwargs:
        cmd = cmd + ', max(' + str(kwargs['LIMITSUPABS']) + ')'

    cmd = cmd + ')\n'

    if 'LIMITORD' in kwargs:
        cmd = cmd + '  SCALE: linear(dim(2), max(' + str(kwargs['LIMITORD']) + '))\n'
    
    cmd = cmd + '  ELEMENT: interval(position(summary.count(bin.rect(' + varIndep + '*1*' + varDep + bins + '))), \n    color.interior(' + varDep + '))\n'
    if 'NORMAL' in kwargs:
        cmd = cmd + '  ELEMENT: line(position(density.normal(' + varIndep + '*1*' + varDep + ')))\n'
    
    cmd = cmd + 'END GPL.\n'
    
    
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)

    if 'RANK' in kwargs:
        pyDeleteVariables(['nx_' + varIndep])
        
        cmd = 'RANK VARIABLES = ' + varIndep + '/ntiles(10) into nx_' + varIndep + ' /print=no /ties=mean.\n'
        cmd = cmd + 'CROSSTABS /TABLES=nx_' + varIndep + ' by ' + varDep + ' /FORMAT=AVALUE TABLES /CELLS=COUNT EXPECTED ROW.\nEXECUTE.\n'
        
        if 'CMD' in kwargs:
            if kwargs['CMD'] > 0:
                print(cmd)
            if kwargs['CMD'] < 2:
                spss.Submit(cmd)
        else:
            spss.Submit(cmd)



###################################################################################################
############                                                                                       
############    *BoxPlots(listaVarsString)                          ['var1 var2 var3']             
############          kwargs:   CMD                                                                
############                                                                                       
###################################################################################################
def pyBoxPlots(listaVarsString):
    if len(listaVarsString) == 0:
        return

    ListaVariablesAGraficar = []
    # En SPSS la longitud max de un string es de 251 caracteres. Las variables independientes me vienen entonces en una lista de strings parciales
    for i in range(len(listaVarsString)):
        ListaVariablesAGraficar = ListaVariablesAGraficar + listaVarsString[i].split()

    for var in ListaVariablesAGraficar:
        print("Evaluando variable numerica " + var + "...")
        # Primero me fijo si figura en la lista de variables que no deben ser estudiadas estadisticamente (cableadas al principio)
        try:
            i = noEvaluarList.index(var)
        except ValueError:
            # No existia, la creo y la agrego al final de numList
            i = -1  # Sigo adelante

        if i >= 0:  # Paso a la siguiente variable...
            continue   

        cmd = "EXAMINE " + var + "\n/plot boxplot histogram\n/percentiles(5,10,25,50,75,90,95,99)."
        spss.Submit(cmd)

###################################################################################################
############                                                                                       
############    *DisplayGrLegend(a, b)             
############          kwargs:                                                                   
############                                                                                       
###################################################################################################
def pyDisplayGrLegend(a, b):
    try:
        a = float(a.replace(',', '.'))
    except ValueError:
        pass

    try:
        b = float(b.replace(',', '.'))
    except ValueError:
        pass

    if abs(a) >= 1000000 or abs(b) >= 1000000:
        a /= 1000000.0
        b /= 1000000.0
        sep = 'M'
    elif abs(a) >= 1000 or abs(b) >= 1000:
        a /= 1000.0
        b /= 1000.0
        sep = 'k'
    else:
        a *= 1.0
        b *= 1.0
        sep = ','

    return  "De {0:1.2f} a {1:1.2f}".format(a, b).replace( '.', sep)

###################################################################################################
############                                                                                        
############    *Rank(var, byVar="", percentiles = 0, **kwargs)                                     
############          kwargs: CMD       DIVISOR Primero divide la variable a rankear por aluna otra variable que se pasa aca.                                                            
############          Si  byVar != ""  se hace un CrossTabs por byVar                               
############                                                                                        
###################################################################################################
def pyRank(var, byVar="", percentiles = 0, **kwargs):
    if percentiles <= 0:
        percentiles = 20

    print "Percentiles = " + str(percentiles)

    pyDeleteVariables([ 'nx_' + var,  'dividida_' + var])

    cmd = ''
    if 'DIVISOR' in kwargs and kwargs['DIVISOR'] != '':
        cmd = '''COMPUTE ''' +  var + '''DIV''' + kwargs['DIVISOR'] + ''' = ''' + var + ''' / ''' + kwargs['DIVISOR'] + '''.\nEXECUTE.\n'''
        var = var + '''DIV''' + kwargs['DIVISOR']
        
    cmd = cmd + "RANK \n  VARIABLES = " + var + "\n  /NTILES(" + str(percentiles) + ") INTO nx_" + var + " \n  /PRINT = YES \n  /TIES = MEAN . \nEXECUTE. \n\n"
    if len(byVar) > 0:
        cmd = cmd + "CROSSTABS\n  /TABLES=nx_" + var + " BY " + byVar + "\n  /FORMAT= AVALUE TABLES\n  /CELLS= COUNT ROW.\nEXECUTE.\n\n"
    cmd = cmd + "MEANS\n  TABLES=" + var + "  BY nx_" + var + "\n  /CELLS COUNT MIN MAX .\nEXECUTE.\n\n"

    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)

###################################################################################################
############                                                                                        
############    *RankGr(listaVarsString, byVar="", percentiles = 0, **kwargs)                                     
############          kwargs: CMD                                                                   
############          Si  byVar != ""  se hace un CrossTabs por byVar                               
############                                                                                        
###################################################################################################
def pyRankGr(listaVarsString, byVar="", percentiles = 0, **kwargs):


    if percentiles <= 0:
        percentiles = 20

    prefix = 'nx_'
    if 'PREFIX' in  kwargs:
        prefix = kwargs['PREFIX']

    listaVars = []
    for i in range(len(listaVarsString)):
        listaVars = listaVars + listaVarsString[i].split()

    for var in listaVars:

        pyDeleteVariables([ prefix + var])

        cmd = ''
        if 'DIVISOR' in kwargs and kwargs['DIVISOR'] != '':
            pyDeleteVariables([ prefix + var + '''DIV''' + kwargs['DIVISOR'] ])
            cmd = '''COMPUTE ''' +  var + '''DIV''' + kwargs['DIVISOR'] + ''' = ''' + var + ''' / ''' + kwargs['DIVISOR'] + '''.\nEXECUTE.\n'''
            var = var + '''DIV''' + kwargs['DIVISOR']
        
        cmd = cmd + "RANK \n  VARIABLES = " + var + "\n  /NTILES(" + str(percentiles) + ") INTO " + prefix + var + " \n  /PRINT = YES \n  /TIES = MEAN . \nEXECUTE. \n\n"
        if len(byVar) > 0:

            meansCmd = ""
            if 'MEANS' in kwargs:
                if kwargs['MEANS'] > 0:
                    meansCmd = "MEANS " + var + " /CELLS COUNT MIN MAX.\n"

            if 'PANEL1' in kwargs:
                panelVar = kwargs['PANEL1'][0] + ' > '
                if meansCmd != '':
                    meansCmd = "MEANS " + var + " BY " + kwargs['PANEL1'][0] + " /CELLS COUNT MIN MAX.\n"
            else:
                panelVar = ''

            cmd = cmd + meansCmd
            cmd = cmd + "TABLES\n /FORMAT BLANK MISSING('.')\n /OBSERVATION= " + var + " " + byVar + "\n"
            cmd = cmd + " /GBASE=CASES /TABLE=  " + panelVar + " " + prefix + var + "  BY " + var + " + " + byVar + " /Title = '" + var + "'\n"
            cmd = cmd + " /STATISTICS min(" + var + " ( F5.1 ) 'Desde') max(" + var + " (F5.1) 'Hasta') "
            cmd = cmd + "count(" + var + " ( F7.0 ) 'Cnt. Casos') sum(" + byVar + "  (F7.0) 'Cnt. " + byVar + "s') mean(" + byVar + " (F7.2) '%" + byVar + "s')."
        else:
            cmd = cmd + "MEANS\n  TABLES=" + var + "  BY " + prefix + var + "\n  /CELLS COUNT MIN MAX .\nEXECUTE.\n\n"

        if 'CMD' in kwargs:
            if kwargs['CMD'] > 0:
                print(cmd)
            if kwargs['CMD'] < 2:
                spss.Submit(cmd)
            #elif kwargs['CMD'] == 3:
                #spss.Submit(cmd)
                #tag,err = spssaux.createXmlOutput('Dataset Display',omsid='Dataset Display', subtype='Datasets')
                #dslist = spssaux.getValuesFromXmlWorkspace(tag, 'Datasets')
                #print dslist
                #if 'means' in dslist:
                #    spss.Submit("DATASET CLOSE Means.")

                cmd2 = "MEANS  TABLES=" + var + "  BY " + prefix + var + "  /CELLS MIN MAX.\n"
                #print
                #print cmd2
                #tag, err = spssaux.createDatasetOutput(cmd2,subtype='statistics', visible=True, newdsn='means')
                tag, err = spssaux.CreateXMLOutput(cmd2, omsid='Means', visible=False) 

                minimos = spssaux.GetValuesFromXMLWorkspace(
                                        tag,
                                        tableSubtype='Report',
                                        colCategory='Minimum',
                                        cellAttrib="text")
                #print "Los minimos son: "
                #print minimos

                maximosNUM = spssaux.GetValuesFromXMLWorkspace(
                                        tag,
                                        tableSubtype='Report',
                                        #rowCategory="3",
                                        colCategory='Maximum',
                                        cellAttrib="number")

                maximos = spssaux.GetValuesFromXMLWorkspace(
                                        tag,
                                        tableSubtype='Report',
                                        #rowCategory="3",
                                        colCategory='Maximum',
                                        cellAttrib="text")
                #print "Los maximos son: "
                #print maximos


                categories = spss.EvaluateXPath(tag,"/", "//pivotTable//dimension//group/category/@number")
                #print categories

                spss.DeleteXPathHandle(tag)
                if not(isinstance(minimos[0], str)):
                    cmd3 = "VALUE LABELS " + prefix + var
                    for ix in range(len(minimos)-1):
                        #print 'Valores en el loop: ' + str(ix) + ' -> ' + categories[ix] + ': ' + str(minimos[ix]) + ' a ' + str(maximos[ix])
                        #print  pyDisplayGrLegend(minimos[ix], maximos[ix])
                        cmd3 = cmd3 + "\n " + categories[ix] + " " + pyDisplayGrLegend(minimos[ix], maximos[ix])
                    cmd3 = cmd3 + ".\n" + "EXECUTE.\n"
                    spss.Submit(cmd3)
                #print cmd3

                if 'RECODE' in kwargs and kwargs['RECODE'] >0:
                    # OJO recordar que si se quiere usar CATEG y MISSING, estos tienen que venir como kwargs de la regresion aunque no tengan nada que ver ahi...
                    #print minimos, maximosNUM, categories
                    if 'PREFIX' in  kwargs:
                        kwargs['PREFIX'] = ''
                        pyRecode(prefix + var, range(percentiles), byVar, **kwargs)
                    else:
                        pyRecode(prefix + var, range(percentiles), byVar, PREFIX = '', **kwargs)

                kwargs['CMD'] = 0
        else:
            spss.Submit(cmd)

        if len(byVar) > 0 and 'RECODE' not in kwargs:
            nxVar = prefix + var
            pyGrafBivariante(nxVar, byVar, **kwargs)

###################################################################################################
############                                                                                       
############    *Recode(nombreVar, listaVal, nombreVarDep= 'MarcaMora', **kwargs)          
############          kwargs: CMD    GRAF p/ grafico de barras + linea de media de malos           
############                      CATEG (no genera las piecewise)      MISSING (los asigna al valor pasado)                                                                         
###################################################################################################
def pyRecode(nombreVar, listaVal, nombreVarDep= 'MarcaMoraBinaria', **kwargs):
    listaVal.sort()

    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]
    cmd = 'DELETE VARIABLES '
    cantVar = 0

    if 'pw' + nombreVar in fullList:
        cmd = cmd + 'pw' + nombreVar + ' '
        cantVar += 1

    for i in range(20):
        if 'pw' + nombreVar + str(i+1) in fullList:
            #print 'Encontre: ' + 'pw' + nombreVar + str(i+1) + ' '
            cmd = cmd + 'pw' + nombreVar + str(i+1) + ' '
            cantVar += 1
        else:
            break

    #print 'cantVar = ' + str(cantVar)
    if cantVar > 0:
        spss.Submit(cmd + '.\nEXECUTE.')

    tratarMissing = ''
    if 'MISSING' in kwargs:
        tratarMissing = ' (SYSMIS= ' + str(kwargs['MISSING']) + ')\n'

    prefix = 'pw'
    into = '\ninto pw' + nombreVar
    if 'PREFIX' in  kwargs:
        prefix = kwargs['PREFIX']
        if prefix == '':
            into = ''

    cmd = """Recode """ + nombreVar + """\n""" + tratarMissing + """ (lo thru """
    for i in range(len(listaVal)):
        #print 'Dentro del loop de recode', i, listaVal[i]
        cmd = cmd + str(listaVal[i]) + """ = """ + str(i + 1) + """)\n (""" + str(listaVal[i]) + """ thru """
    cmd = cmd + """hi = """ + str(len(listaVal) + 1) + """) """ + into + """.\n\n"""
    
    if 'MISSING' in kwargs:
        cmd = cmd + """Value Labels """ + prefix + nombreVar + """ 0 'Missing'. \n """

    cmd = cmd + """Value Labels """ + prefix + nombreVar + """\n 1 '> lowest y <= """
    for i in range(len(listaVal)):
        cmd = cmd + str(listaVal[i]) + """'\n """ + str(i+2) + """ '> """ + str(listaVal[i]) + """ y <= """
    cmd = cmd + """hi'. \n\n"""
    
    # Si estoy recodificando a CATEGorica no genero las piecewise
    makePiecewise = 1
    if 'CATEG'  in kwargs and kwargs['CATEG'] > 0:
        makePiecewise = 0
        
    if makePiecewise == 1:
        for i in range(len(listaVal)+1):
            cmd = cmd + """COMPUTE """ + prefix + nombreVar + str(i+1) + """ = 0.\n"""
        cmd = cmd + """EXECUTE.\n\n"""
        
        cmd = cmd + """IF (""" + nombreVar + """ <= """ + str(listaVal[0]) + """)""" + prefix + nombreVar+"""1 = """ + nombreVar + """.\n"""
        for i in range(len(listaVal)-1):
            cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[i]) + """ AND """ + nombreVar + """ <= """ + str(listaVal[i+1]) + """) """ + prefix + nombreVar + str(i+2) + """ = """ + nombreVar + """ - """ + str(listaVal[i])+""".\n"""
        cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[-1]) + """) """ + prefix + nombreVar+str(len(listaVal) + 1) + """ = """ + nombreVar  + """ - """ + str(listaVal[-1])+""".\n"""
        cmd = cmd + """EXECUTE.\n\n"""
        
        cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[0]) + """) """ + prefix + nombreVar+"""1 = """ + str(listaVal[0]) + """.\n"""
        for i in range(len(listaVal)-1):
            cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[i+1]) + """) """ + prefix + nombreVar + str(i+2) + """ = """ + str(listaVal[i+1]) + """ - """ + str(listaVal[i])+""".\n"""
        cmd = cmd + """EXECUTE.\n\n"""
        
        cmd = cmd + """VARIABLE LABELS """ + prefix + nombreVar + """1 '""" + prefix + """ """ + nombreVar + """ > lo y <= """ +  str(listaVal[0])+ """'.\n"""
        for i in range(len(listaVal)-1):
            cmd = cmd + """VARIABLE LABELS """ + prefix + nombreVar + str(i+2)+ """ '""" + prefix + """ """ + nombreVar + """ > """ +  str(listaVal[i])+ """ y <= """ +  str(listaVal[i+1])+ """'.\n""" 
        cmd = cmd + """VARIABLE LABELS """ + prefix + nombreVar + str(len(listaVal) + 1)+ """ '""" + prefix + """ """ + nombreVar + """ > """ +  str(listaVal[-1])+ """ y <= hi'.\n""" 
        cmd = cmd + """EXECUTE."""

    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)


    if 'CATEG'  in kwargs and kwargs['CATEG'] > 0:
        cmd = '''FREQUENCIES ''' + prefix + nombreVar + ''' .\nEXECUTE.\n'''
        if 'CMD' in kwargs:
            if kwargs['CMD'] > 0:
                print(cmd)
            if kwargs['CMD'] < 2:
                spss.Submit(cmd)
        else:
            spss.Submit(cmd)

    cmd = ''
    colorLinea = "green" # Por default VERDE
    if 'COLORLINEA' in kwargs:
           colorLinea = kwargs['COLORLINEA']

    if 'GRAF' in kwargs:
        if kwargs['GRAF'] > 0:
            colorCaja = "FC924A" # Por default NARANJA
            if 'COLOR' in kwargs:
                if kwargs['COLOR'] == 1:
                   colorCaja = "3E58AC" # Forzar a AZUL
            cmd = '''
* Generador de graficos       OJO Color NARANJA = "FC924A"      Color AZUL = "3E58AC".
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES= ''' + prefix + nombreVar + '''[LEVEL=ORDINAL] COUNT()[name="COUNT"] 
    MEAN(''' + nombreVarDep + ''')[name="MEAN''' + nombreVarDep + '''"] MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ''' + prefix + nombreVar + '''=col(source(s), name("''' + prefix + nombreVar + '''"), unit.category())
  DATA: COUNT=col(source(s), name("COUNT"))
  DATA: MEAN_''' + nombreVarDep + '''=col(source(s), name("MEAN''' + nombreVarDep + '''"))
  GUIDE: axis(dim(1), label("''' + nombreVar + '''"))
  GUIDE: axis(scale(y1), label("Recuento"), color(color."''' + colorCaja + '''"))
  GUIDE: axis(scale(y2), label("Media ''' + nombreVarDep + '''"), color(color."2EB848"), opposite())
  SCALE: y1 = linear(dim(2), include(0))
  SCALE: y2 = linear(dim(2), include(0))
  ELEMENT: interval(position(''' + prefix + nombreVar + '''*COUNT), shape.interior(shape.square), 
    color.interior(color."''' + colorCaja + '''"), scale(y1))
  ELEMENT: line(position(''' + prefix + nombreVar + '''*MEAN_''' + nombreVarDep + '''), missing.wings(), 
    color.interior(color.''' + colorLinea + '''), scale(y2))
END GPL.\n
'''
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)


###################################################################################################
############                                                                                       
############    *RecodeTeresa(nombreVar, listaVal, nombreVarDep= 'MarcaMoraBinaria', **kwargs)  
############          kwargs: CMD    GRAF p/ grafico de barras + linea de media de malos           
############                                                                                       
############                                      F A L T A   T E R M I N A R !!!!!                
############                                                                                       
###################################################################################################
#
# AGREGO PARAMETRO OPCIONAL PARA GENERAR MAS DE UNA PW PARA UNA VARIABLE
# Falta: opcional para variable dependiente
# PROBAR, PORQUE NUNCA LO EJECUTE
def pyRecodeTeresa(nombreVar, listaVal, varOpcional = "", **kwargs):
    if (varOpcional == ""):
        varOpcional = nombreVar

    listaVal.sort()
    
    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]
    cmd = 'DELETE VARIABLES '
    cantVar = 0

    if 'pw' + nombreVar in fullList:
        cmd = cmd + 'pw' + nombreVar + ' '
        cantVar += 1

    for i in range(20):
        if 'pw' + nombreVar + str(i+1) in fullList:
            #print 'Encontre: ' + 'pw' + nombreVar + str(i+1) + ' '
            cmd = cmd + 'pw' + nombreVar + str(i+1) + ' '
            cantVar += 1
        else:
            break

    #print 'cantVar = ' + str(cantVar)
    if cantVar > 0:
        spss.Submit(cmd + '.\nEXECUTE.')


    cmd = """Recode """ + nombreVar + """\n (lo thru """
    for i in range(len(listaVal)):
        cmd = cmd + str(listaVal[i]) + """ = """ + str(i + 1) + """)\n (""" + str(listaVal[i]) + """ thru """
    cmd = cmd + """hi = """ + str(len(listaVal) + 1) + """) \ninto pw""" + nombreVar + """.\n\n"""
    
    cmd = cmd + """Value Labels pw""" + nombreVar + """\n 1 '> lowest y <= """
    for i in range(len(listaVal)):
        cmd = cmd + str(listaVal[i]) + """'\n """ + str(i+2) + """ '> """ + str(listaVal[i]) + """ y <= """
    cmd = cmd + """hi'. \n\n"""
    
    for i in range(len(listaVal)+1):
        cmd = cmd + """COMPUTE pw""" + nombreVar + str(i+1) + """ = 0.\n"""
    cmd = cmd + """EXECUTE.\n\n"""
    
    cmd = cmd + """IF (""" + nombreVar + """ <= """ + str(listaVal[0]) + """) pw""" + nombreVar+"""1 = """ + varOpcional + """.\n"""
    for i in range(len(listaVal)-1):
        cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[i]) + """ AND """ + nombreVar + """ <= """ + str(listaVal[i+1]) + """) pw""" + nombreVar + str(i+2) + """ = """ + nombreVar + """ - """ + str(listaVal[i])+""".\n"""
    cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[-1]) + """) pw""" + varOpcional+str(len(listaVal) + 1) + """ = """ + nombreVar  + """ - """ + str(listaVal[-1])+""".\n"""
    cmd = cmd + """EXECUTE.\n\n"""
    
    cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[0]) + """) pw""" + nombreVar+"""1 = """ + str(listaVal[0]) + """.\n"""
    for i in range(len(listaVal)-1):
        cmd = cmd + """IF (""" + nombreVar + """ > """ + str(listaVal[i+1]) + """) pw""" + nombreVar + str(i+2) + """ = """ + str(listaVal[i+1]) + """ - """ + str(listaVal[i])+""".\n"""
    cmd = cmd + """EXECUTE.\n\n"""
    
    cmd = cmd + """VARIABLE LABELS pw""" + nombreVar + """1 'pw """ + nombreVar + """ > lo y <= """ +  str(listaVal[0])+ """'.\n"""
    for i in range(len(listaVal)-1):
        cmd = cmd + """VARIABLE LABELS pw""" + nombreVar + str(i+2)+ """ 'pw """ + nombreVar + """ > """ +  str(listaVal[i])+ """ y <= """ +  str(listaVal[i+1])+ """'.\n""" 
    cmd = cmd + """VARIABLE LABELS pw""" + nombreVar + str(len(listaVal) + 1)+ """ 'pw """ + nombreVar + """ > """ +  str(listaVal[-1])+ """ y <= hi'.\n""" 
    cmd = cmd + """EXECUTE."""
    
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)

    if 'GRAF' in kwargs:
        if kwargs['GRAF'] > 0:
            cmd = '''
* Generador de graficos       OJO Color NARANJA = "FC924A"      Color AZUL = "3E58AC".
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=pw''' + nombreVar + '''[LEVEL=ORDINAL] COUNT()[name="COUNT"] 
    MEAN(''' + varOpcional + ''')[name="MEAN''' + varOpcional + '''"] MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: pw''' + nombreVar + '''=col(source(s), name("pw''' + nombreVar + '''"), unit.category())
  DATA: COUNT=col(source(s), name("COUNT"))
  DATA: MEAN_''' + varOpcional + '''=col(source(s), name("MEAN''' + varOpcional + '''"))
  GUIDE: axis(dim(1), label("''' + nombreVar + '''"))
  GUIDE: axis(scale(y1), label("Recuento"), color(color."FC924A"))
  GUIDE: axis(scale(y2), label("Media ''' + varOpcional + '''"), color(color."2EB848"), opposite())
  SCALE: y1 = linear(dim(2), include(0))
  SCALE: y2 = linear(dim(2), include(0))
  ELEMENT: interval(position(pw''' + nombreVar + '''*COUNT), shape.interior(shape.square), 
    color.interior(color."FC924A"), scale(y1))
  ELEMENT: line(position(pw''' + nombreVar + '''*MEAN_''' + varOpcional + '''), missing.wings(), 
    color.interior(color."2EB848"), scale(y2))
END GPL.\n
'''
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)


###################################################################################################
############                                                                                       
############    *GrafBivariante(nombreVar, nombreBinaria, **kwargs)                                
############          kwargs: CMD    COLOR: 1 - Azul   PANEL1    PANEL2
############                                                                                       
###################################################################################################
def pyGrafBivariante(nombreVar, nombreBinaria, **kwargs):
    colorCaja = "FC924A" # Por default NARANJA
    if 'COLOR' in kwargs:
        if kwargs['COLOR'] == 1:
           colorCaja = "3E58AC" # Forzar a AZUL

    colorLinea = "green" # Por default VERDE
    if 'COLORLINEA' in kwargs:
           colorLinea = kwargs['COLORLINEA']

    panelData = ''
    panelGuide = ''
    panelDeclares = ''
    panelElements = ''
    if 'PANEL1' in kwargs:
        panelDeclares = kwargs['PANEL1'][0] + ' [LEVEL=NOMINAL] '
        panelData = '  DATA: ' + kwargs['PANEL1'][0] + '=col(source(s), name("' + kwargs['PANEL1'][0] + '"), notIn(' + kwargs['PANEL1'][1] + '), unit.category())\n'
        panelGuide = '  GUIDE: axis(dim(3), label("' + kwargs['PANEL1'][0] + '"), opposite())\n'
        panelElements = '*' + kwargs['PANEL1'][0]

    if 'PANEL2' in kwargs:
        panelDeclares = panelDeclares + kwargs['PANEL2'][0] + ' [LEVEL=NOMINAL] '
        panelData = panelData + '  DATA: ' + kwargs['PANEL2'][0] + '=col(source(s), name("' + kwargs['PANEL2'][0] + '"), notIn(' + kwargs['PANEL2'][1] + '), unit.category())\n'
        panelGuide = panelGuide + '  GUIDE: axis(dim(4), label("' + kwargs['PANEL2'][0] + '"), opposite())\n'
        panelElements = panelElements + '*' + kwargs['PANEL2'][0]

    cmd = '''
* Generador de graficos       OJO Color NARANJA = "FC924A"      Color AZUL = "3E58AC".
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=''' + nombreVar + '''[LEVEL=ORDINAL] COUNT()[name="COUNT"] 
   MEAN(''' + nombreBinaria + ''')[name="MEAN''' + nombreBinaria + '''"] ''' + panelDeclares + '''MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ''' + nombreVar + '''=col(source(s), name("''' + nombreVar + '''"), unit.category())
  DATA: COUNT=col(source(s), name("COUNT"))
  DATA: MEAN_''' + nombreBinaria + '''=col(source(s), name("MEAN''' + nombreBinaria + '''")) 
''' + panelData + '''  GUIDE: axis(dim(1), label("''' + nombreVar + '''"))
  GUIDE: axis(scale(y1), label("Recuento"), color(color."''' + colorCaja + '''"))
  GUIDE: axis(scale(y2), label("Media ''' + nombreBinaria + '''"), color(color."2EB848"), opposite())
''' + panelGuide + '''    SCALE: y1 = linear(dim(2), include(0))
  SCALE: y2 = linear(dim(2), include(0))
  ELEMENT: interval(position(''' + nombreVar + '''*COUNT''' + panelElements + '''), shape.interior(shape.square), 
    color.interior(color."''' + colorCaja + '''"), scale(y1))
  ELEMENT: line(position(''' + nombreVar + '''*MEAN_''' + nombreBinaria + panelElements + '''), missing.wings(), 
    color.interior(color.''' + colorLinea + '''), scale(y2))
END GPL.\n
'''
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print(cmd)
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)


###################################################################################################
############                                                                                       
############    *GrafBivariantes(listaVarsString, listaByString, **kwargs)                         
############          kwargs: CMD              COLOR:   0 o nada- Naranja           1 - Azul       
############                      TABLA:  ['MarcaMalo1 MarcaMalo2 MarcaMalo3']                     
############                           si hay una sola marca sale un grafico simple                
############                           si hay 3 sale un grafico complejo (ver con Paulina)         
############                      COMPLETAR: forzar todas las categoricas, las marcadas + las que  
############                       empiezan con 't' minuscula' seguida de una letra MAYUSCULA      
############                                                                                       
############            Ambas listas se escriben:             ['var1 var2 var3']                   
############                                                                                       
###################################################################################################
def pyGrafBivariantes(listaVarsString, listaByString, **kwargs):
    import re

    pyDeleteVariables(['TodosLosCasos'])
    spss.Submit("COMPUTE TodosLosCasos = 1.\nEXECUTE.\nVARIABLE LABELS TodosLosCasos 'Total'.\nVALUE LABELS TodosLosCasos 1 'Total'.")

    listaVars = []
    for i in range(len(listaVarsString)):
        listaVars = listaVars + listaVarsString[i].split()

    listaBy = []
    for i in range(len(listaByString)):
        listaBy = listaBy + listaByString[i].split()

    if 'COMPLETAR' in kwargs:
        if kwargs['COMPLETAR'] > 0:
                varCount = spss.GetVariableCount()
                for i in xrange(varCount):
                    tipo = spss.GetVariableMeasurementLevel(i)
                    nombre = spss.GetVariableName(i)
        
                     ###  OJO CHANCHADA: las variables "categoricas" que creamos nosotros, pero que quedan como numericas DEBEN empezar con 't' minuscula' y estar seguidas de una letra MAYUSCULA ###
                    if tipo == 'nominal' or tipo == 'ordinal' or re.search('^t[A-Z].*', nombre):
                        if nombre not in listaVars and nombre not in listaBy:
                            listaVars.append(nombre)


    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print listaVars

    for varX in listaVars:
        for varBy in listaBy:
            print "**** ********* ************** " + varX + " by " + varBy + " *** "
            if (varBy == listaBy[0])  and ('TABLA' in kwargs):
                varsTabla = kwargs['TABLA'][0].split()

                # La version "oficial y standard" va con 3 variables. Hay una "quick and dirty" para una variable mas abajo.
                if len(varsTabla) == 3:
                    cmd = """
TABLES
  /FORMAT BLANK MISSING('.')
  /OBSERVATION= """ + varsTabla[0] + """ """ + varsTabla[1] + """
  /GBASE=CASES
  /TABLE=  """ + varsTabla[2] + """ > (TodosLosCasos + """ + varX + """)
            by   (""" + varsTabla[0] + """ + """ + varsTabla[1] + """ ) 
   /Title = 'Analisis NBC scorings'
  /STATISTICS
count(  """ + varsTabla[0] + """ ( F6.0 ) 'Cant. solicitudes')
cpct(  """ + varsTabla[0] + """ ( F5.3 ) '% de Cant. solicitudes':""" + varsTabla[2] + """)
sum(  """ + varsTabla[0] + """ ( F6.0 ) 'Cant con producto' )
SPCT (  """ + varsTabla[0] + """ ( F5.3 ) ' % de producto' :""" + varsTabla[2] + """)
mean  ( """ + varsTabla[1] + """ ( F5.3 ) '% malos de productos') .
"""
                # Mini tablita rapida. Titulo mejorado de Camila
                elif len(varsTabla) == 1:
                    if 'PANEL1' in kwargs:
                        panelVar = kwargs['PANEL1'][0] + ' > '
                    else:
                        panelVar = ''
                    
                    cmd = """
TABLES
  /FORMAT BLANK MISSING('.')
  /OBSERVATION= """ + varsTabla[0] + """
  /GBASE=CASES
  /TABLE= """ + panelVar + """ (""" + varX + """)
            by   (""" + varsTabla[0] + """ ) 
   /Title = '""" + varX + """'
  /STATISTICS
count(  """ + varsTabla[0] + """ ( F6.0 ) 'Cant. Casos')
sum(""" + varsTabla[0] + """  (F7.0) 'Cnt. Malos') 
mean(""" + varsTabla[0] + """ (F7.2) '%Malos').
"""
                else:
                    print 
                if 'CMD' in kwargs:
                    if kwargs['CMD'] > 0:
                        print cmd
                    if kwargs['CMD'] < 2:
                        spss.Submit(cmd)

            pyGrafBivariante(varX, varBy, **kwargs)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    WEIGHT   #
###################################################################################################
############                                                                                       
############    *SetWeight(paramWeight)                 Fija la variable a usar como ponderacion   
############          kwargs: ninguna                                     paramWeight:  'var1'     
############                                                                                       
###################################################################################################
def pySetWeight(paramWeight):
    global varWeight
    varWeight = paramWeight

###################################################################################################
############                                                                                       
############    *PrintWeight()                   Solamente imprime la variable de ponderacion      
############          kwargs: ninguna                                                              
############                                                                                       
###################################################################################################
def pyPrintWeight():
    print varWeight



###################################################################################################
############                                                                                       
############    *SetValorPonderacion(valPondera)                                                   
############          kwargs: ninguna                                                              
############          valPondera: numero                                                           
############                                                                                       
###################################################################################################
def pySetValorPonderacion(valPondera):
    global valorPonderacion
    valorPonderacion = valPondera

###################################################################################################
############                                                                                       
############    *GetValorPonderacion()                  Solamente imprime el valor de ponderacion  
############          kwargs: ninguna                                                              
############                                                                                       
###################################################################################################
def pyGetValorPonderacion():
    print 'Pondera = ' + str(valorPonderacion)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    REGRESION   #
###################################################################################################
############                                                                                       
############    *RegrSetup(nombreVarDep, listaVarsIndepString, listaVarsCateg = None,              
############             outPRED = 'Pm_nuevo', outRESID = 'RES_nuevo', **kwargs)                   
############          kwargs: CMD      DIRECCION      CASOS                                        
############                       SELECTVAR   tiene precedencia sobre    TRAIN                    
############                      RANK    ROC     KS                                               
############                                                                                       
###################################################################################################
def pyRegrSetup(nombreVarDep, listaVarsIndepString, listaVarsCateg = None, outPRED = 'Pm_nuevo', outRESID = 'RES_nuevo', **kwargs):
    import spssaux
    import re

    global regrVarDep
    global regrVarIndep
    global regrCatList
    global regrPRED
    global regrRESID
    global lastVariables
    global lastCategoricas
    
    if listaVarsCateg is None:  # Ver explicacion de esto en: http://effbot.org/zone/default-values.htm
        listaVarsCateg = []
    
    regrPRED = outPRED
    regrRESID = outRESID
    
    cmd = ""
    # Si ya existen las variables de salida de la regresion hay que borrarlas, si no, SPSS pincha...
    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]
    if outPRED in fullList:
        fullList.remove(regrPRED)
        cmd = cmd + "DELETE VARIABLE " + regrPRED + ".\nEXECUTE.\n"
    if outRESID in fullList:
        fullList.remove(regrRESID)
        cmd = cmd + "DELETE VARIABLE " + regrRESID + ".\nEXECUTE.\n"
    
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print cmd
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)

    # Limpio el estado previo: pyRevert solo tiene sentido despues de un pyRegrModif
    lastVariables = []
    lastCategoricas = []

    # Paso los parametros de la corrida a globales
    regrVarDep = nombreVarDep
    regrVarIndep = []
    # En SPSS la longitud max de un string es de 251 caracteres. Las variables independientes me vienen entonces en una lista de strings parciales
    for i in range(len(listaVarsIndepString)):
        regrVarIndep = regrVarIndep + listaVarsIndepString[i].split()
    regrCatList = []
    
    # Armo una lista de Categoricas del dataset para poder clasificar adecuadamente las variables que me pasaron
    catList = []
    
    varCount = spss.GetVariableCount()
    for i in xrange(varCount):
        tipo = spss.GetVariableMeasurementLevel(i)
        nombre = spss.GetVariableName(i)
        
                                                ###  OJO CHANCHADA: las variables "categoricas" que creamos nosotros, pero que quedan como numericas DEBEN empezar con 't' minuscula' y estar seguidas de una letra MAYUSCULA ###
        if tipo == 'nominal' or tipo == 'ordinal':    ###or re.search('^t[A-Z].*', nombre):
            catList.append(nombre)
    
    
    # Busco cada variable a usar en la regresion en la lista de categoricas del set de datos
    for var in regrVarIndep:
        if var in catList:
            # Es categorica, la voy a tener que marcar como tal en el comando de regresion
            regrCatList.append(var)
    
    # Tambien esta la lista de variables categoicas forzadas a mano
    for var in (set(listaVarsCateg[0].split()) - set(regrCatList)):
        regrCatList.append(var)


    print regrVarIndep

    pyCorrerRegresion(fullList, **kwargs)

###################################################################################################
############                                                                                       
############    *RegrModif(fullList, **kwargs)                                               
############          kwargs: CMD      SELECTVAR      TRAIN      DIRECCION      CASOS              
############                      RANK    ROC     KS                                               
############                                                                                       
###################################################################################################
def pyRegrModif(agregarVarsIndepString, borrarVarsIndepString, listaVarsCateg = None, **kwargs):
    import re

    global regrVarDep
    global regrVarIndep
    global regrCatList
    global regrPRED
    global regrRESID
    global lastVariables
    global lastCategoricas
    
    if listaVarsCateg is None:  # Ver explicacion de esto en: http://effbot.org/zone/default-values.htm
        listaVarsCateg = []
    
    # Si ya existen las variables de salida de la regresion hay que borrarlas, si no, SPSS pincha...
    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]
    if regrPRED in fullList:
        fullList.remove(regrPRED)
        spss.Submit("DELETE VARIABLE " + regrPRED + ".\nEXECUTE.\n")
    if regrRESID in fullList:
        fullList.remove(regrRESID)
        spss.Submit("DELETE VARIABLE " + regrRESID + ".\nEXECUTE.\n")
    
    # Me guardo el estado previo para poder hacer un Undo con pyRevert
    #								##  OJO: pasar una copia!!!!! ##
    lastVariables = regrVarIndep[:]
    lastCategoricas = regrCatList[:]

    # Elimino variables
    varAEliminar = []
    # En SPSS la longitud max de un string es de 251 caracteres. Las variables independientes me vienen entonces en una lista de strings parciales
    for i in range(len(borrarVarsIndepString)):
        varAEliminar = varAEliminar + borrarVarsIndepString[i].split()
    for var in ( set(varAEliminar) & set(regrVarIndep) ):
            regrVarIndep.remove(var)

    # Ahora agrego las nuevas
    varAAgregar = []
    # En SPSS la longitud max de un string es de 251 caracteres. Las variables independientes me vienen entonces en una lista de strings parciales
    if len(agregarVarsIndepString) > 0:
        for i in range(len(agregarVarsIndepString)):
            varAAgregar = varAAgregar + agregarVarsIndepString[i].split()
        for var in varAAgregar:
            if var not in regrVarIndep:
                regrVarIndep.append(var)


    # Armo una lista de Categoricas del dataset para poder clasificar adecuadamente las variables que quedaron
    catList = []

    varCount = spss.GetVariableCount()
    for i in xrange(varCount):
        tipo = spss.GetVariableMeasurementLevel(i)
        nombre = spss.GetVariableName(i)

                                                ###  OJO CHANCHADA: las variables "categoricas" que creamos nosotros, pero que quedan como numericas DEBEN empezar con 't' minuscula' y estar seguidas de una letra MAYUSCULA ###
        if tipo == 'nominal' or tipo == 'ordinal' or re.search('^t[A-Z].*', nombre):
            catList.append(nombre)

    # Tambien esta la lista de variables categoricas forzadas a mano
    if len(listaVarsCateg) > 0:
        for var in (set(listaVarsCateg[0].split()) - set(regrCatList)):
            regrCatList.append(var)

    # Ahora hago algo parecido para la lista de categoricas a agregar =
    #      Conjunto de las que quiero agregar  que todavia NO estan en regrCatList INTERSECTION  Conjunto de Categoricas
    for var in ( (set(varAAgregar) - set(regrCatList)) & set(catList) ):
        regrCatList.append(var)

    # Idem para la lista de categoricas a eliminar =
    #     Conjunto de las que quiero eliminar INTERSECTION Conjunto de variables que Si estan en regrCatList
    for var in ( set(varAEliminar) & set(regrCatList) ):
        regrCatList.remove(var)

    varAEliminar = []
    varAAgregar = []
    pyCorrerRegresion(fullList, **kwargs)


###################################################################################################
############                                                                                        
############    *Revert(**kwargs)                                                                   
############          kwargs: ninguna                                                               
############                                                                                        
###################################################################################################
def pyRevert(**kwargs):
    global regrVarIndep
    global regrCatList

    # Por las dudas de que llamen a pyRevert antes de haber corrido pyRegrModif, chequeo que las listas 'last' no esten vacias
    if lastVariables:
        regrVarIndep = lastVariables[:]	# Copia
    if lastCategoricas:
        regrCatList = lastCategoricas[:]
    print regrVarIndep
    print regrCatList
    print varWeight

###################################################################################################
############                                                                                       
############    *RegrShowVars()                                                                    
############          kwargs: ninguna                                                              
############                                                                                       
###################################################################################################
def pyRegrShowVars():
    print regrVarDep
    print regrVarIndep
    print regrCatList
    print regrPRED
    print regrRESID
    print lastVariables
    print lastCategoricas
    print varWeight


###################################################################################################
############                                                                                       
############    *CorrerRegresion(fullList, **kwargs)                                               
############          kwargs: CMD      DIRECCION      CASOS                                        
############                       SELECTVAR   tiene precedencia sobre    TRAIN                    
############                      RANK    ROC     KS                                               
############                                                                                       
###################################################################################################
def pyCorrerRegresion(fullList, **kwargs):
    print regrVarIndep

    cmd = ""
    if 'CASOS' in kwargs:
        if kwargs['CASOS'] > 0:
            cmd = "CROSSTABS MotivoFiltrado MarcaMoraGral BY MarcaMoraBinaria BY UsarEnRegres.\n"
            if 'CMD' in kwargs:
                if kwargs['CMD'] > 0:
                    print cmd
                if kwargs['CMD'] < 2:
                    spss.Submit(cmd)
            else:
                spss.Submit(cmd)

    
    cmd = ""
    if varWeight != '':
        cmd = cmd + "WEIGHT BY " + varWeight + ".\nEXECUTE.\n"
        spss.Submit(cmd)
    
    recordarSelectVar = ""
    recordarTrain = ""


    
    # Tanto SELECTVAR  como  TRAIN  controlan el SELECT de la regresion.
    # Por ahora SELECTVAR tiene prioridad: si esta presente no anda el TRAIN.

    if 'SELECTVAR' in kwargs:
        print "'SELECTVAR' in kwargs"
        recordarSelectVar =  "/SELECT " + kwargs['SELECTVAR'] + " EQ 1\n"
        print "Se usara 'SELECTVAR' = " + str(kwargs['SELECTVAR']) + "\n" + recordarSelectVar

    elif 'TRAIN' in kwargs:
        print "'TRAIN' in kwargs"
        if kwargs['TRAIN'] > 0 and kwargs['TRAIN'] < 100:
            pyDeleteVariables(['Train'])
            cmd = cmd + "COMPUTE Train=(UNIFORM(1)<=" + str(kwargs['TRAIN'] / 100.00) + ").\n"  # SET RNG=MC SEED=9191972.\n
            cmd = cmd + "VARIABLE LABELS Train 'Set de TRAINING: aprox. " + str(kwargs['TRAIN']) + "% de los casos.'.\nFORMAT Train (f1.0).\nEXECUTE.\n\n"
            
            recordarTrain =  "/SELECT Train EQ 1\n"
            print "Se usara 'TRAIN' = " + str(kwargs['TRAIN']) + "\n" + recordarTrain
        else:
            print "'TRAIN' fuera de rango: " + str(kwargs['TRAIN'])
        
    
    direccMetodo = "FSTEP"
    if 'DIRECCION' in kwargs:
        if kwargs['DIRECCION'] in ['B', 'F']:
            print 'Direccion del metodo especificada como ' + kwargs['DIRECCION']
            direccMetodo = kwargs['DIRECCION'] + "STEP"
        elif kwargs['DIRECCION'] in ['E']:
            print 'Direccion del metodo especificada como ' + kwargs['DIRECCION']
            direccMetodo = 'ENTER'
        else:
            print 'Error en Direccion del metodo especificada: ' + kwargs['DIRECCION'] + ". Se usara 'FSTEP'."
    
    
    cmd = "LOGISTIC REGRESSION VAR=" + regrVarDep + "\n" + recordarTrain + recordarSelectVar
    cmd = cmd + "/METHOD = " + direccMetodo + "\n"

    # Listo TODAS las variables a incluir
    for var in regrVarIndep:
        cmd = cmd + var +"\n"
    
    # Ahora declaro las categoricas
    if regrCatList != []:
        cmd = cmd + "/CATEGORICAL=\n"
        for var in regrCatList:
            cmd = cmd + var +"\n"
    
        # Ahora declaro los INDICATORS    <--> de prepo 1
        for var in regrCatList:
            cmd = cmd + "/CONTRAST (" + var +")=indicator(1)\n"
    
    # Lineas "fijas" del final
    ###                        Elimino "/CLASSPLOT\n" antes del "/PRINT" de la linea que sigue
    cmd = cmd + "/PRINT=SUMMARY CORR"
    if 'REGRDETALLE' in kwargs:
        cmd = cmd + " GOODFIT CI(95)\n"
    else:
         cmd = cmd + "\n"
    
    cmd = cmd + "/SAVE PRED(" + regrPRED + ") ZRESID(" + regrRESID + ")\n/CRITERIA PIN(.05) POUT (.05) ITERATE (20) CUT (.50)\n/EXTERNAL.\n"
    
    cmd = cmd + "EXECUTE.\nWEIGHT OFF.\n"
    
    if 'TRAIN' in kwargs:
        cmp = "FREQ Train.\nDELETE VARIABLES Train.\nEXECUTE.\n"
    
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print cmd
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)

    # Siempre hago un RANK despues de la regresion.  El kwarg RANK me da la cant de percentiles a usar, por default son 20
    if 'RANK' in kwargs:
        percentiles = kwargs['RANK']
    else:
        percentiles = 20       
        
    if 'nx_' + regrPRED in fullList:
        spss.Submit("DELETE VARIABLE nx_" + regrPRED + ".\nEXECUTE.\n")
    
    print 'Uso percentiles = ' + str(percentiles)

    if 'SELECTVAR' in kwargs and kwargs['SELECTVAR'] != '':
        cmd = "RANK \n  VARIABLES =  " + regrPRED + " BY " + kwargs['SELECTVAR'] + " \n  /NTILES(" + str(percentiles) + ") INTO  nx_" + regrPRED + " \n  /PRINT = NO \n  /TIES = MEAN . \nEXECUTE. \n"
    else: #Caso sin Training/Test
        cmd = "RANK \n  VARIABLES =  " + regrPRED + " \n  /NTILES(" + str(percentiles) + ") INTO  nx_" + regrPRED + " \n  /PRINT = NO \n  /TIES = MEAN . \nEXECUTE. \n"

    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print cmd
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)
    
    if 'ROC' in kwargs:                 # Si figura ROC genero el grafico
        # Preparo el comando para poder hacer mas de una ROC  si hay SELECTVAR
        cmdFin = '/PLOT=CURVE(REFERENCE)\n'
        if kwargs['ROC'] > 1:                     # En general no imprimo coordenadas, salvo que ROC > 1
            cmdFin =  cmdFin + '/PRINT=COORDINATES \n'
        
        cmdFin = cmdFin + '/CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95) \n/MISSING=EXCLUDE .\n\n'


        if 'SELECTVAR' in kwargs and kwargs['SELECTVAR'] != '':
            pyDeleteVariables(['nx_' + regrPRED + '1',  'nx_' + regrPRED + '0'])

            cmd = "IF " + kwargs['SELECTVAR'] + " = 1  nx_" + regrPRED + "1 = nx_" + regrPRED + ".\n"
            cmd = cmd + "IF " + kwargs['SELECTVAR'] + " = 0  nx_" + regrPRED + "0 = nx_" + regrPRED + ".\n"
            pyExec(cmd, **kwargs)
            print '*** *** *** *** *** *** *** *** ***    C U R V A   R O C   D E L   T O T A L    *** *** *** *** *** *** *** *** *** *** *** ***'
            cmd = "ROC\n nx_" + regrPRED + " BY " + regrVarDep + "(1) \n" + cmdFin
            pyExec(cmd, **kwargs)
            print '*** *** *** *** *** *** *** *** ***   C U R V A   R O C   D E   T R A I N I N G   *** *** *** *** *** *** *** *** *** *** *** ***'
            cmd = "ROC\n nx_" + regrPRED + "1 BY " + regrVarDep + "(1) \n" + cmdFin
            pyExec(cmd, **kwargs)
            print '*** *** *** *** *** *** *** *** ***   C U R V A   R O C   D E   T E S T   *** *** *** *** *** *** *** *** *** *** *** ***'
            cmd = "ROC\n nx_" + regrPRED + "0 BY " + regrVarDep + "(1) \n" + cmdFin
        else: #Caso sin Training/Test
            cmd = "ROC\n nx_" + regrPRED + " BY " + regrVarDep + "(1) \n " + cmdFin
        
    else:
        cmd = ""
    
    if 'KS' in kwargs:
        if 'SELECTVAR' in kwargs and kwargs['SELECTVAR'] != '':
            cmd = cmd + "NPAR TESTS \n/K-S= nx_" + regrPRED + "0 nx_" + regrPRED + "1 BY " + regrVarDep + "(0 1) \n  /STATISTICS=DESCRIPTIVES \n  /MISSING ANALYSIS.\n\n"
        else:
            cmd = cmd + "NPAR TESTS \n/K-S= nx_" + regrPRED + " BY " + regrVarDep + "(0 1) \n/MISSING ANALYSIS.\n\n"
        if kwargs['KS'] > 1:
            cmd = cmd + "NPAR TESTS \n/K-S=nx_UsarRegr2  BY " + regrVarDep + "(0 1) \n/MISSING ANALYSIS.\n\n"
    
    if 'TABLE' in kwargs:
        cmd = cmd + """
TABLES
  /FORMAT BLANK MISSING('.')
  /OBSERVATION= 
""" + regrPRED + """ MarcaMaloMaduro MarcaBuenoMaduro id  TotalAprob MontoMaloMaduro MontoBuenoMaduro
  /GBASE=CASES
  /TABLE= nx_UsarRegr1
             By  """ + regrPRED + """ + MarcaMaloMaduro + MarcaBuenoMaduro + id  + TotalAprob + MontoMaloMaduro + MontoBuenoMaduro
   /Title = 'Desempenho UsarRegr1 rank de 20 en MarcaMaloMaduro '
  /STATISTICS
  MINIMUM( """ + regrPRED + """ ( F5.4 ) 'Desde')
  MAXIMUM( """ + regrPRED + """ ( F5.4 ) 'Hasta')
  MEAN (MarcaMaloMaduro ( F5.4 ) '% Malos')
  SUM(MarcaMaloMaduro (F7.0) 'Cant Malos')
  MEAN (MarcaBuenoMaduro ( F5.4 ) '% Buenos')
  SUM(MarcaBuenoMaduro (F7.0) 'Cant Buenos')
  CPCT( id ( F4.2 ) '% casos') 
  COUNT( id ( F7.0 ) 'Cant. casos') 
  SUM(TotalAprob (F12.2) 'Monto Aprobado')
  SPCT(TotalAprob (F5.2) '% Monto Aprobado') 
  SUM(MontoMaloMaduro (F12.2) 'MontoMaloMaduro')
  SUM(MontoBuenoMaduro (F12.2) 'MontoBuenoMaduro') .

"""
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print cmd
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)

###################################################################################################
############                                                                                        
############    *CheckExist(listaVarsString, **kwargs)                                     
############          kwargs: CMD                                                                   
############                                       
############                                                                                        
###################################################################################################
def pyCheckExist(listaVarsString, **kwargs):
    listaVars = []
    for i in range(len(listaVarsString)):
        listaVars = listaVars + listaVarsString[i].split()

    fullList = [spss.GetVariableName(i) for i in range(spss.GetVariableCount())]

    noExisten = set(listaVars).difference(set(fullList))
    if len(noExisten) == 0:
        print 'Existen todas.'
    else:
        print 'No existe(n)', noExisten


###################################################################################################
############                                                                                        
############    *Exec(cmd, **kwargs)                                     
############          kwargs: CMD                                                                   
############                                       
############                                                                                        
###################################################################################################
def pyExec(cmd, **kwargs):
    if 'CMD' in kwargs:
        if kwargs['CMD'] > 0:
            print cmd
        if kwargs['CMD'] < 2:
            spss.Submit(cmd)
    else:
        spss.Submit(cmd)


###################################################################################################
############                                                                                        
############    *TablaAcum2(varProbMora, varRankProbMora, varMalo, varTodos, varMonto, **kwargs)                                     
############          kwargs: CMD                                                                   
############                                       
############                                                                                        
###################################################################################################
def pyTablaAcum2(varProbMora, varRankProbMora, varMalo, varTodos='', varMonto='', **kwargs):

    if varMonto == '':
        incluirVarMonto = ''
        signoSumaVarMonto = ''
        totalesMonto = ''
    else:
        incluirVarMonto = ' \n                ' + varMonto
        signoSumaVarMonto = ' + '
        totalesMonto = '''  mean(''' + varMonto + ''' ( F16.0 ) 'Monto Promedio')
  sum (''' + varMonto + ''' ( F20.0 ) 'Monto Colocacion')
  cpct (''' + varMonto + ''' ( F5.2 ) 'cpct: Monto Colocacion')
  spct (''' + varMonto + ''' ( F5.2 ) 'spct: Monto Colocacion')'''


    if varTodos == '':
        incluirVarTodos = ''
    else:
        incluirVarTodos = varTodos +  ' + ' 

    cmd = '''
TABLES
  /FORMAT BLANK MISSING(".")
  /OBSERVATION= 
                ''' + varProbMora + ''' 
                 ''' + varMalo + incluirVarMonto + '''
  /GBASE=CASES
  /TABLE= ''' + incluirVarTodos + varRankProbMora + '''
             By  ''' + varProbMora + ''' + 
                ''' + varMalo + signoSumaVarMonto + incluirVarMonto + '''
   /Title = 'Informacion desempenho x Prob. Mora'
  /STATISTICS
  minimum( ''' + varProbMora + ''' ( F5.4 ) 'Desde')
  maximum( ''' + varProbMora + ''' ( F5.4 ) 'Hasta')
  mean (''' + varMalo + ''' ( F5.4 ) '% CR MarcaMora ')
  sum(''' + varMalo + ''' (F7.0) 'Cnt. Malos')
  count ( ''' + varProbMora + ''' ( F7.0 ) 'Cant. creditos')
''' + totalesMonto + '''.
'''
    pyExec(cmd, **kwargs)


###################################################################################################
############                                                                                        
############    *TablaAcum(varProbMora, varRankProbMora, varMalo, varTodos, varMonto, **kwargs)                                     
############          kwargs: CMD                                                                   
############                                       
############                                                                                        
###################################################################################################
def pyTablaAcum(varProbMora, varRankProbMora, varMalo, varTodos='', varMonto='', **kwargs):

    if varMonto == '':
        totalesMonto = ''
    else:
        totalesMonto =  varMonto + ''' [S][MINIMUM F40.0, MEAN F40.0, MAXIMUM F40.0, SUM 'Total de la fila' F40.0, TABLEPCT.SUM '% del Total' PCT40.1]'''

    cmd = '''
CTABLES
  /VLABELS VARIABLES=''' + varTodos + ''' ''' + varRankProbMora + ''' DISPLAY=NONE
  /VLABELS VARIABLES=''' + varProbMora + ''' DISPLAY=LABEL
  /VLABELS VARIABLES=''' + varMalo + ''' ''' + varMonto + ''' DISPLAY=DEFAULT
  /TABLE ''' + varTodos + ('' if varTodos == '' else ' [C] + ') + varRankProbMora + ''' [C]
    BY ''' + varProbMora + ''' [S][MINIMUM  'Desde' F40.4, MAXIMUM 'Hasta' F40.4] +
    ''' + varMalo + ''' [C][COUNT 'Cantidad' F40.0, ROWPCT.COUNT '% de la fila' PCT40.2, TOTALS[COUNT 'Cant. Total' F40.0, COLPCT.COUNT '% Casos del total' PCT40.1]] ''' + ('' if varMonto == '' else ' +\n') + totalesMonto + '''
  /CATEGORIES VARIABLES=''' + varMalo + ''' ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER.'''

    pyExec(cmd, **kwargs)
