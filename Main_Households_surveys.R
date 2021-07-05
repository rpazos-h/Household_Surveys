# RESUME

# This script uses household's survey data to analyze job-transitions, 
# It studies how probable is for one employee moving from one occupation (industry) to another one
# in terms of its current occupation

# Similarity of skills and the concept of industrial space (Neffke et al 2017) in terms of labor mobility
# Employment flows between industries provide information about the local productive structure


# Plots:
#   - Job to Job transitions (from old to new occupation)
#   - Heatmap prob of job transitions (from industry X to Y) 
#   - Network of interactions (flows) (GRAFOS)
#   - Strength of activities Plot 

# Regressions : Distance between ocupations


rm(list = ls())
gc()

#### Libraries #####

libraries <- c('data.table','foreign','magrittr','ggplot2','ggthemes','ggraph',
               'RColorBrewer', 'igraph', 'viridis','ggraph','tidygraph','ggrepel','viridis')
new.packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(libraries, require, character.only=TRUE)

### Sources ####

source('https://raw.githubusercontent.com/rpazos-h/Household_Surveys/main/download_files.R')
source('https://raw.githubusercontent.com/rpazos-h/Household_Surveys/main/code_auxs.R')


#### DATA ####

datos <- getEPHS(dir = '/Users/usuario/Desktop/')

clasificadorCAES <- fread("https://raw.githubusercontent.com/rpazos-h/Household_Surveys/main/code_convert.txt") %>% 
  mutate(CAES_nuevo = as.numeric(CAES_nuevo),
         CAES_viejo_4d = as.numeric(CAES_viejo_4d))


cno <- fread('https://raw.githubusercontent.com/rpazos-h/Household_Surveys/main/Code_NO_Trans.csv',
             encoding = 'UTF-8') %>% 
  mutate(Código = ifelse(nchar(Código)==1,paste0('0',Código),Código))

  
clasificacionACT <- fread('https://raw.githubusercontent.com/rpazos-h/Household_Surveys/main/Clas_2d.txt',
                          encoding='UTF-8') %>% 
  mutate(V7=NULL,
         `2d` = ifelse(nchar(`2d`)==1,paste0(0,`2d`),`2d`))

inflacion <- fread('https://raw.githubusercontent.com/rpazos-h/Household_Surveys/main/CPI.csv',
                   encoding = 'UTF-8') %>%
  as.data.table(.) %>%
  .[complete.cases(.)] %>%
  .[,list(IPC=mean(IPC)), by=c('Year','Trimestre')]


#### Tidy #1 ####

datos <- datos %>% 
  .[ESTADO %in% c(1:3) & REGION==1] %>% 
  .[,CAT_ACT:=as.numeric(ifelse(ANO4 >2011 & !is.na(PP04B_CAES),PP04B_CAES,
                                           ifelse(ANO4 > 2011 & is.na(PP04B_CAES),PP04B_COD,NA)))] %>% 
  .[ESTADO %in% c(1:3) & REGION==1] %>% 
  .[,CAT_ACT:=as.numeric(ifelse(ANO4 >2011 & !is.na(PP04B_CAES),PP04B_CAES,
                                           ifelse(ANO4 > 2011 & is.na(PP04B_CAES),PP04B_COD,NA)))] %>% 
  .[,CAT_ACT:=ifelse(!CAT_ACT %in% na.omit(clasificadorCAES$CAES_nuevo),NA,CAT_ACT)] %>% 
  .[clasificadorCAES,on=c('CAT_ACT'='CAES_nuevo'),
               CAT_ACT:=ifelse(ANO4<=2011,NA,i.CAES_viejo_4d)] %>% 
  .[,CAT_ACT := ifelse(ANO4<=2011, PP04B_COD, CAT_ACT)] %>% 
  .[,CAT_ACT := ifelse(!CAT_ACT %in% clasificadorCAES$CAES_viejo_4d,NA,CAT_ACT)] %>% 
  .[,CAT_ACT := ifelse(nchar(CAT_ACT)==3,paste0(0,CAT_ACT),CAT_ACT)]

clasificadorCAESLetra <- fread("https://raw.githubusercontent.com/martinmontane/EPHelper/master/Clasificadores/Conversores/caes_viejo_4d_a_letra.csv", encoding = "UTF-8") %>% 
  .[nchar(CAES_VIEJO)>2] %>% 
  .[,CAES_VIEJO:=ifelse(nchar(CAES_VIEJO)==3,paste0('0',CAES_VIEJO), CAES_VIEJO)]

datos <- datos %>% 
  .[clasificadorCAESLetra,on=c('CAT_ACT'='CAES_VIEJO'),
               LETRA_ACT:=Letra] %>% 
  .[,PONDERA_W:=ifelse(ANO4<2016,PONDERA,PONDIIO)] %>% 
  mutate(PP3E_TOT = as.numeric(PP3E_TOT))

promedioAnual <- datos[P21>0 & PP3E_TOT>0,
                       list(Variable=sum((P21/PP3E_TOT)*PONDERA_W)/sum(PONDERA_W)),
                       by=.(ANO4)]

resumenLetras <- datos %>% 
  .[P21>0 & PP3E_TOT>0,list(Variable=sum((P21/PP3E_TOT)*PONDERA_W)/sum(PONDERA_W)),
                       by=.(LETRA_ACT, ANO4)] %>% 
  .[promedioAnual,on='ANO4',SALARIO_AVE:=i.Variable] %>% 
  .[,arribaPromedio:=ifelse(Variable>SALARIO_AVE,TRUE,FALSE)]

datos <- datos[resumenLetras, on=c('LETRA_ACT','ANO4'),
               SECTOR_ABOVE_AVG:=ifelse(!is.na(LETRA_ACT),arribaPromedio,NA)]

datos <- datos %>%
  .[,CODUSU:=trimws(CODUSU,which = 'both')] %>%
  .[,ID:=paste(CODUSU,NRO_HOGAR,COMPONENTE,sep='_')] %>%
  .[,nActivo:=sum(ESTADO==1), by= c('ID')] %>%
  .[nActivo>1] %>%
  .[order(ID,ANO4,TRIMESTRE)] %>%
  .[, TRIMESTRES:=((ANO4-shift(ANO4,type = 'lag'))*4)+(TRIMESTRE-shift(TRIMESTRE, type='lag')), by=c('ID')] %>%
  .[, TRIMESTRES:=ifelse(is.na(TRIMESTRES), 0, TRIMESTRES)] %>%
  .[,PP04D_COD:=substr(PP04D_COD,1,2)] %>%
  .[,CAT_ACT := substr(CAT_ACT, 1, 2)]%>%
  .[,saltoInactividad:=ifelse(ESTADO == 1 & shift(type = 'lag',n = 1,x = ESTADO) == 3 &
                                shift(type = 'lag',n = 2,x = ESTADO) == 1, 'Inactividad',
                              ifelse(ESTADO == 1 & shift(type = 'lag',n = 1,x = ESTADO) == 2 &
                                       shift(type = 'lag',n = 2,x = ESTADO) == 1,'Desocupado','Otro')), by=c('ID')] %>%
  .[,CAT_OCUP:=ifelse(CAT_OCUP %in% c(1,2,4),'Cuentapropista',
                      ifelse(CAT_OCUP == 3 & (PP07H == 1 | PP07I ==1),'Asalariado registrado',
                             ifelse(CAT_OCUP == 3 & !(PP07H == 1 | PP07I ==1), 'Asalariado no registrado',
                                    ifelse(!ESTADO %in% 1, 'No ocupado','Otro'))))]


datos[which(saltoInactividad %in% c('Desocupado'))-2, saltoInactividad:='Desocupado']
datos[which(saltoInactividad %in% c('Inactividad'))-2, saltoInactividad:='Inactividad']

saltosInactividad <- datos[saltoInactividad %in% c('Desocupado','Inactividad')]

datos <- datos %>% 
  .[!ID %in% unique(saltosInactividad$ID)] %>% 
  .[, EXP_EMP:=ifelse(PP07A %in% c(1,2), 2/3,
                      ifelse(PP07A == 3, 5/3,
                             ifelse(PP07A == 4,9/3,
                                    ifelse(PP07A == 5, 36/3,72))))] %>%
  .[, saltoLaboral := ifelse(!TRIMESTRES %in% 0 & (TRIMESTRES-EXP_EMP) > 0, TRUE,FALSE), by='ID'] %>% 
  .[which(saltoLaboral==TRUE)-1,saltoLaboral:=TRUE] %>% 
  .[saltoLaboral==TRUE] %>% 
  .[,apariciones:=.N, by=c('ID')] %>% 
  .[apariciones==2]

saltosInactividad <- saltosInactividad %>%
  .[, OCUP_LAG:=shift(type = 'lag',n = 1,x = PP04D_COD), by=c("ID")] %>%
  .[, ACT_LAG:=shift(type = 'lag',n = 1,x = CAT_ACT), by=c("ID")] %>%
  .[, W_LAG:=shift(type = 'lag',n = 1,x = P21), by=c("ID")] %>%
  .[, H_LAG:=shift(type = 'lag',n = 1,x = PP3E_TOT), by=c("ID")] %>%
  .[, CAT_OCUP_LAG:=shift(type = 'lag',n = 1,x = CAT_OCUP), by=c("ID")] %>%
  .[, UNEM_TRANS:=TRUE] %>%
  .[, TRIM_LAG:=shift(type = 'lag',n = 1,x = TRIMESTRE), by=c("ID")] %>%
  .[, YEAR_LAG:=shift(type = 'lag',n = 1,x = ANO4), by=c("ID")]  %>%
  .[, IDIMPP_LAG:=shift(type = 'lag',n = 1,x =IDIMPP), by=c("ID")] %>%
  .[, SECTOR_ABOVE_AVG_LAG:=shift(type = 'lag',n = 1,x =SECTOR_ABOVE_AVG), by=c("ID")] %>%
  .[!is.na(W_LAG)]

datos <- datos %>%
  .[, OCUP_LAG:=shift(type = 'lag',n = 1,x = PP04D_COD), by=c("ID")] %>%
  .[, ACT_LAG:=shift(type = 'lag',n = 1,x = CAT_ACT), by=c("ID")] %>%
  .[, CAT_OCUP_LAG:=shift(type = 'lag',n = 1,x = CAT_OCUP), by=c("ID")] %>%
  .[, W_LAG:=shift(type = 'lag',n = 1,x = P21), by=c("ID")] %>%
  .[, H_LAG:=shift(type = 'lag',n = 1,x = PP3E_TOT), by=c("ID")] %>%
  .[, TRIM_LAG:=shift(type = 'lag',n = 1,x = TRIMESTRE),] %>%
  .[, YEAR_LAG:=shift(type = 'lag',n = 1,x = ANO4),] %>%
  .[, UNEM_TRANS:=FALSE]  %>%
  .[,IDIMPP_LAG:=shift(type = 'lag',n = 1,x =IDIMPP), by=c("ID")] %>%
  .[, SECTOR_ABOVE_AVG_LAG:=shift(type = 'lag',n = 1,x =SECTOR_ABOVE_AVG), by=c("ID")] %>%
  .[!is.na(W_LAG)]

datos <- rbind(datos, saltosInactividad, fill=TRUE)

datosOcupaciones <- datos[datos$PP04D_COD %in% cno$Código & datos$OCUP_LAG %in% cno$Código,]


#### PLOT TRANSITIONS: IN-OUT FLOWS by occupation ####

matrizTransicionesOcupaciones <- unclass(table(datosOcupaciones$PP04D_COD[!datosOcupaciones$PP04D_COD==datosOcupaciones$OCUP_LAG],
                                               datosOcupaciones$OCUP_LAG[!datosOcupaciones$PP04D_COD==datosOcupaciones$OCUP_LAG]))
propSalidas <- data.table(Sector= rownames(matrizTransicionesOcupaciones),
                          Valor=apply(matrizTransicionesOcupaciones,1,sum)/sum(matrizTransicionesOcupaciones))
propLlegadas <- data.table(Sector= colnames(matrizTransicionesOcupaciones),
                           Valor=apply(matrizTransicionesOcupaciones,2,sum)/sum(matrizTransicionesOcupaciones))

cno<-cno %>% 
  mutate(IDDouble = as.double(Código))

SalidasLlegadasSector <- propSalidas[propLlegadas,on=c('Sector'),valor2:=i.Valor] %>% 
  select(Sector, 'PropSalida' = Valor, 'PropLlegada' = valor2) %>% 
  mutate(Sector = as.double(Sector)) %>% 
  .[as.data.table(cno),on=c('Sector'='IDDouble'),cnoIng:=Desc_eng] %>% 
  mutate(Agrupación = ifelse(PropSalida>0.03,cnoIng,NA))

salidaEntradaPlot <- ggplot(SalidasLlegadasSector, aes(x=PropSalida, y=PropLlegada)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_text_repel(aes(label=Agrupación),force = 4, size=5) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  labs(x='Share of job-to-job transitions (old occupation)',
       y='Share of job-to-job transitions (new occupation)') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))
salidaEntradaPlot


#### NEFTKE (2017) ####

flujosOcupaciones <- as.data.table(table(datosOcupaciones$PP04D_COD,
                                         datosOcupaciones$OCUP_LAG))
flujosOcupaciones <- getDistanceMeasureProduccion(sectorSalida = flujosOcupaciones$V2,
                                                  sectorLlegada = flujosOcupaciones$V1,
                                                  flow = flujosOcupaciones$N)


flujosActividades <- as.data.table(table(datos$CAT_ACT,datos$ACT_LAG))
flujosActividades <- getDistanceMeasureProduccion(sectorSalida = flujosActividades$V2,
                                                  sectorLlegada = flujosActividades$V1,
                                                  flow = flujosActividades$N)


### HEATMAP AND GRAFO PLOTS ####


# Heatmap de ocupaciones Red-Blue

heatmap <- ggplot(flujosOcupaciones,aes(x=sectorSalida,y=sectorLlegada, fill=RijCorregido, label=round(RijCorregido,2))) +
  geom_tile(color=NA) +
  scale_fill_viridis()+
  labs(fill="", x="",y="")
heatmap


# Heatmap de ocupaciones (Related)
flujosOcupacionesRelated <- flujosOcupaciones %>% 
  mutate(RijCorregido = ifelse(RijCorregido<0,0,RijCorregido))

heatmap <- ggplot(flujosOcupacionesRelated[!sectorSalida==71 & !sectorLlegada==71],aes(x=sectorSalida,y=sectorLlegada, fill=RijCorregido, label=round(RijCorregido,2))) +
  geom_tile(color=NA) +
  scale_fill_viridis() +
  labs(fill="", x="",y="")
heatmap

# GRAFO 

flujosActividades <- flujosActividades %>% 
  dplyr::rename(weight = RijCorregido)

flujosGrafo<-flujosActividades[order(weight,decreasing = TRUE)][!sectorSalida==sectorLlegada][1:(length(unique(flujosActividades$sectorSalida))*3)]

grafo <- graph_from_data_frame(flujosGrafo,directed = FALSE)

g_mst <- as_tbl_graph(grafo)

g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::left_join(clasificacionACT[,c('2d','LetraDescripcion')],by=c('name'='2d')) 

g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::mutate(Agrupacion=as.factor(LetraDescripcion))


nodesMST <- g_mst %>% activate(nodes) %>% as.data.frame()
edgesMST <- g_mst %>% activate(edges) %>% as.data.frame()
edgesMST$nameFrom <- nodesMST[edgesMST$from,c('name')]
edgesMST$agrupacionFrom <- nodesMST[edgesMST$from,c('Agrupacion')]
edgesMST$nameTo <- nodesMST[edgesMST$to,c('name')]
edgesMST$agrupacionTo <- nodesMST[edgesMST$to,c('Agrupacion')]

grafoMST <- ggraph(layout = 'fr',graph = g_mst) +
  geom_edge_link(show.legend = FALSE,color='grey60') +
  geom_node_point(aes(color=Agrupacion), size=4) +
  theme_minimal() +
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color='', x="", y="")

grafoMST

grafo <- graph_from_data_frame(flujosActividades[weight>0],directed = TRUE)


# Grafo ocupaciones

flujosOcupaciones<-flujosOcupaciones %>% 
  dplyr::rename(weight = RijCorregido )

flujosGrafo <- flujosOcupaciones[!sectorSalida %in% c("02","03","04","05","06","07") & !sectorLlegada %in% c("02","03","04","05","06","07") ]
flujosGrafo<-flujosGrafo[order(weight,decreasing = TRUE)][!sectorSalida==sectorLlegada][1:141]

grafo <- graph_from_data_frame(flujosGrafo,directed = FALSE)
g_mst <- as_tbl_graph(grafo,weighted=TRUE)

cno <- cno %>% 
  dplyr::rename(Agrupacion = Desc_eng)

g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::left_join(cno[,c('Código','Agrupacion')],by=c('name'='Código'))
g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::mutate(Agrupacion=as.factor(Agrupacion))

nodesMST <- g_mst %>% activate(nodes) %>% as.data.frame()
edgesMST <- g_mst %>% activate(edges) %>% as.data.frame()
edgesMST$nameFrom <- nodesMST[edgesMST$from,c('name')]
edgesMST$agrupacionFrom <- nodesMST[edgesMST$from,c('Agrupacion')]
edgesMST$nameTo <- nodesMST[edgesMST$to,c('name')]
edgesMST$agrupacionTo <- nodesMST[edgesMST$to,c('Agrupacion')]
edgesMST$agrupacionFrom <- gsub(x=edgesMST$agrupacionFrom,pattern = '* occupations*',replacement = '')
edgesMST$agrupacionTo <- gsub(x=edgesMST$agrupacionTo,pattern = '* occupations*',replacement = '')
nodesMST$Agrupacion <- gsub(x=nodesMST$Agrupacion,pattern = '* occupations*',replacement = '')


g_mst <- g_mst %>%
  activate(nodes) %>%
  dplyr::mutate(transporte=ifelse(Agrupacion=='Transporte y telecomunicaciones',1,0),
                Comercio=ifelse(Agrupacion=='Comercio',1,0),
                Gestion=ifelse(Agrupacion=='Gestión',1,0),
                Primario=ifelse(Agrupacion=='Ocupaciones del sector primario',1,0),
                Directivos=ifelse(Agrupacion=='Directores',1,0),
                Agrupacion=gsub(x=Agrupacion,pattern = '* occupations*',replacement = ''))


grafoMST <- ggraph(#layout = 'kk',
  graph = g_mst)+ #%>% connected) +
  geom_edge_link(show.legend = FALSE, color='grey50') +
  geom_node_point(aes(color=Agrupacion), size=4) +
  theme_minimal() +
  theme(legend.position = 'bottom', axis.text = element_blank()) +
  labs(color='', x="", y="")
grafoMST

colnames(flujosOcupacionesRelated)[3] <- 'weight'
grafo <- graph_from_data_frame(flujosOcupacionesRelated[weight>0],directed = TRUE)


#### STRENGTH Grafo #####
strengthGrafo <- sort(strength(grafo))
strengthGrafo <- data.table(ocupacion=names(strengthGrafo),
                            strength = strengthGrafo)
strengthGrafo <- strengthGrafo[order(strength)]
cno <- data.table(cno)
cno$IDDouble <- as.character(cno$IDDouble)
strengthGrafo <- strengthGrafo[cno,on=c('ocupacion'='IDDouble'), Desc_esp:=Desc_esp]
strengthGrafo$ocupacion <- factor(strengthGrafo$ocupacion,
                                  levels = strengthGrafo$ocupacion[order(strengthGrafo$strength)])

strengthPlot <- ggplot(strengthGrafo) +
  geom_bar(aes(x=ocupacion, y =strength), fill='#016392', stat = 'identity') +
  geom_text(aes(x=ocupacion, y=strength,label=Desc_esp, hjust=-0.1)) +
  coord_flip() +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal() +
  labs(x="",y="") +
  theme(axis.text.y=element_blank())
strengthPlot


#### GRAPHS WAGES VARIATION ####

# Tidy #2: Education redef

listaEducacion <- list("Sin instrucción",'Sin instrucción','Primaria Completa','Secundaria Incompleta','Secundaria Completa','Superior Incompleta','Superior Completa')
names(listaEducacion) <- c(1,7,2,3,4,5,6)
datos <- datos[,NIVEL_ED:=dplyr::recode(NIVEL_ED,!!!listaEducacion)]
datos <- datos[flujosOcupaciones,
               on=c('OCUP_LAG'='sectorSalida','PP04D_COD'='sectorLlegada'),
               RijOcupaciones:=weight]

datos <- datos[flujosActividades,on=c('ACT_LAG'='sectorSalida','CAT_ACT'='sectorLlegada'),
               RijActividad:=weight]

datos<-datos %>% 
  mutate(ANO4 = factor(ANO4),
         TRIMESTRE = factor(TRIMESTRE),
         IDTRIM = paste(ANO4,TRIMESTRE,sep='-'),
         IDTRIM = factor(IDTRIM))

datos <- datos[,NEW_NIVEL_ED:=forcats::fct_relevel(NIVEL_ED,"Sin instrucción")]


datos <- data.table(datos) %>%
  .[P21>0 & W_LAG >0 & PP3E_TOT>0 & H_LAG>0 & PP3E_TOT<999 & H_LAG<999] %>%
  .[,variacion:=((P21/PP3E_TOT)/(W_LAG/H_LAG))-1]

datos<-datos %>% 
  mutate(ANO4 = as.numeric(as.character(ANO4)),
         TRIMESTRE = as.numeric(as.character(TRIMESTRE)))

datos <- datos[inflacion,
               on=c('ANO4'='Year',
                    'TRIMESTRE'='Trimestre'),
               IPC_ACTUAL:=IPC]
datos <- datos[inflacion,
               on=c('YEAR_LAG'='Year',
                    'TRIM_LAG'='Trimestre'),
               IPC_LAG:=IPC]

datos <- datos[,variacionReal:=((variacion+1)/(IPC_ACTUAL/IPC_LAG))-1]
datos <- datos[,CAT_SEQ:=ifelse(saltoInactividad %in% c('Desocupado','Inactividad'),'Inactividad',
                                ifelse(PP04D_COD==OCUP_LAG, 'Misma ocupacion','Distinta ocupacion'))]
datos <- datos[,VariacionHoraria:=(PP3E_TOT/H_LAG)-1]


# Plots 

ggplot(datos) +
  geom_density(aes(x=VariacionHoraria, color=CAT_SEQ))

ggplot(datos[PP3E_TOT<999 & H_LAG<999]) +
  geom_point(aes(x=variacionReal, y=H_LAG, color = CAT_SEQ))


datos<-datos %>% 
  mutate(ANO4 = factor(ANO4),
         difTrimestre = (as.numeric(as.character(ANO4)) - YEAR_LAG)*4+(TRIMESTRE-TRIM_LAG),
         variacionRealTrimestral = ((variacionReal+1)^(1/difTrimestre))-1)

datos <- datos[,topPercent:=ifelse(variacionReal>quantile(variacionReal,0.95,na.rm=TRUE) | variacionReal<quantile(variacionReal,0.05, na.rm=TRUE),TRUE,FALSE),by=.(CAT_SEQ)]

# Plots #2

ggplot(datos[topPercent==TRUE],
       aes(x=CAT_SEQ, y=variacionRealTrimestral, color=CAT_SEQ)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=3,show.legend = FALSE)

datos <- datos[,WReal:=P21*unique(IPC_ACTUAL[ANO4==2013 & TRIMESTRE ==2])/IPC_ACTUAL]

salariosTrimestrales <- datos[,list(WReal=mean(WReal)),
                              by=.(ANO4,TRIMESTRE,CAT_SEQ)] %>% 
  mutate(TRIM = paste(ANO4,' Q',TRIMESTRE, sep=''))

salariosTrimestrales$TRIM <- zoo::as.yearqtr(salariosTrimestrales$TRIM)

serieCompleta <-seq(as.Date("2003-07-01"), by="quarter", length.out = 42)
serieCompleta <- zoo::as.yearqtr(serieCompleta)

missingQtr <- serieCompleta[!serieCompleta %in% salariosTrimestrales$TRIM]

salariosTrimestrales <- rbind(salariosTrimestrales %>% select(TRIM,WReal, CAT_SEQ),
                              data.table(data.frame(TRIM=zoo::as.yearqtr(rep(missingQtr,length(unique(salariosTrimestrales$CAT_SEQ)))),
                                                    WReal=rep(NA,length(missingQtr)*length(unique(salariosTrimestrales$CAT_SEQ))),
                                                    CAT_SEQ=rep(unique(salariosTrimestrales$CAT_SEQ),length(missingQtr)))),
                              fill=TRUE)
salariosTrimestrales$CAT_SEQ <- as.character(salariosTrimestrales$CAT_SEQ)
salariosTrimestrales$CAT_SEQ <- plyr::mapvalues(salariosTrimestrales$CAT_SEQ,
                                                c('Misma ocupacion','Distinta ocupacion','Inactividad'),
                                                c('Same occupation','Different occupation','Unemployment or inactivity'))
plotSalariosTransicion <- ggplot(salariosTrimestrales) +
  geom_line(aes(x=TRIM, y=WReal, color=CAT_SEQ), size=1.5) +
  theme_fivethirtyeight() +
  scale_color_wsj() +
  theme(axis.text.x = element_text(angle=90)) +
  zoo::scale_x_yearqtr(n = 14,format = 'Q%q-%Y') +
  labs(color='')

plotSalariosTransicion

datos <- datos[,distintaOcupacion:= ifelse(!PP04D_COD==OCUP_LAG, TRUE, FALSE)]
datos <- datos[,distintaActividad:= ifelse(!ACT_LAG==CAT_ACT, TRUE, FALSE)]


#### REGRESSIONS: Distance between ocupations

# Distintas  maneras de medir la distancia entre ocupaciones

gruposDistancia <- c('fullSample','Asalariados','AsalariadosRegistrados', 'FullTime')
listaModelos <- list()
for(gruposDist in gruposDistancia) {
  
  if(gruposDist == 'fullSample') {
    datosOcupacion <- datosOcupaciones
    datosIndustria <- datos
  } else if(gruposDist == 'Asalariados') {
    datosOcupacion <- datosOcupaciones[CAT_OCUP %in% c('Asalariado registrado','Asalariado no registrado') & CAT_OCUP_LAG %in% c('Asalariado registrado','Asalariado no registrado')]
    datosIndustria <- datos[CAT_OCUP %in% c('Asalariado registrado','Asalariado no registrado') & CAT_OCUP_LAG %in% c('Asalariado registrado','Asalariado no registrado')]
  } else if(gruposDist == 'AsalariadosRegistrados') {
    datosOcupacion <- datosOcupaciones[CAT_OCUP %in% c('Asalariado registrado') & CAT_OCUP_LAG %in% c('Asalariado registrado')]
    datosIndustria <- datos[CAT_OCUP %in% c('Asalariado registrado') & CAT_OCUP_LAG %in% c('Asalariado registrado')]
  } else {
    datosOcupacion <- datosOcupacion[PP3E_TOT < 35 & H_LAG <35]
    datosIndustria <- datos[PP3E_TOT < 35 & H_LAG <35]
  }
  
  
  flujosOcupaciones <- as.data.table(table(datosOcupaciones$PP04D_COD,
                                           datosOcupaciones$OCUP_LAG))
  flujosActividades <- as.data.table(table(datosIndustria$CAT_ACT,
                                           datosIndustria$ACT_LAG))
  flujosOcupaciones <- getDistanceMeasureProduccion(sectorSalida = flujosOcupaciones$V2,
                                                    sectorLlegada = flujosOcupaciones$V1,
                                                    flow = flujosOcupaciones$N)
  flujosActividades <- getDistanceMeasureProduccion(sectorSalida = flujosActividades$V2,
                                                    sectorLlegada = flujosActividades$V1,
                                                    flow = flujosActividades$N)
  
  datos <- datos[flujosOcupaciones, on=c('OCUP_LAG'='sectorSalida',
                                         'PP04D_COD'='sectorLlegada'),
                 RijOcupaciones:=i.RijCorregido]
  datos <- datos[flujosActividades, on=c('CAT_ACT'='sectorSalida',
                                         'ACT_LAG'='sectorLlegada'),
                 RijActividad:=i.RijCorregido]
  
  
  # Medida de distancia basada en las trayectorias de los que pasaron por desempleo / inactividad
  flujosOcupacionesUnemployed <- as.data.table(table(datosOcupaciones$PP04D_COD[datosOcupaciones$UNEM_TRANS],
                                                     datosOcupaciones$OCUP_LAG[datosOcupaciones$UNEM_TRANS]))
  flujosOcupacionesUnemployed <- getDistanceMeasureProduccion(sectorSalida = flujosOcupacionesUnemployed$V2,
                                                              sectorLlegada = flujosOcupacionesUnemployed$V1,
                                                              flow = flujosOcupacionesUnemployed$N)
  datos <- datos[flujosOcupacionesUnemployed, on=c('OCUP_LAG'='sectorSalida',
                                                   'PP04D_COD'='sectorLlegada'),
                 RijOcupacionesUnem:=i.RijCorregido]
  flujosActividadesUnemployed <- as.data.table(table(datosIndustria$CAT_ACT[datosIndustria$UNEM_TRANS],
                                                     datosIndustria$ACT_LAG[datosIndustria$UNEM_TRANS]))
  flujosActividadesUnemployed <- getDistanceMeasureProduccion(sectorSalida = flujosActividadesUnemployed$V2,
                                                              sectorLlegada = flujosActividadesUnemployed$V1,
                                                              flow = flujosActividadesUnemployed$N)
  datos <- datos[flujosActividadesUnemployed, on=c('CAT_ACT'='sectorSalida',
                                                   'ACT_LAG'='sectorLlegada'),
                 RijActividadUnem:=i.RijCorregido]
  
  
  # Medida de distancia basada en las trayectorias de los que NO pasaron por desempleo / inactividad
  
  flujosOcupacionesEmployed <- as.data.table(table(datosOcupaciones$PP04D_COD[!(datosOcupaciones$UNEM_TRANS == TRUE) & datosOcupaciones$CAT_OCUP %in% c('Asalariado registrado') & datosOcupaciones$CAT_OCUP_LAG %in% c('Asalariado registrado')],
                                                   datosOcupaciones$OCUP_LAG[!(datosOcupaciones$UNEM_TRANS ==TRUE) & datosOcupaciones$CAT_OCUP %in% c('Asalariado registrado') & datosOcupaciones$CAT_OCUP_LAG %in% c('Asalariado registrado')]))
  flujosOcupacionesEmployed <- getDistanceMeasureProduccion(sectorSalida = flujosOcupacionesEmployed$V2,
                                                            sectorLlegada = flujosOcupacionesEmployed$V1,
                                                            flow = flujosOcupacionesEmployed$N)
  datos <- datos[flujosOcupacionesEmployed, on=c('OCUP_LAG'='sectorSalida',
                                                 'PP04D_COD'='sectorLlegada'),
                 RijOcupacionesEmp:=i.RijCorregido]
  
  
  flujosActividadesEmployed <- as.data.table(table(datosIndustria$CAT_ACT[!datosIndustria$UNEM_TRANS & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP_LAG %in% c('Asalariado registrado')],
                                                   datosIndustria$ACT_LAG[!datosIndustria$UNEM_TRANS & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP %in% c('Asalariado registrado') & datosIndustria$CAT_OCUP_LAG %in% c('Asalariado registrado')]))
  flujosActividadesEmployed <- getDistanceMeasureProduccion(sectorSalida = flujosActividadesEmployed$V2,
                                                            sectorLlegada = flujosActividadesEmployed$V1,
                                                            flow = flujosActividadesEmployed$N)
  datos <- datos[flujosActividadesEmployed, on=c('CAT_ACT'='sectorSalida',
                                                 'ACT_LAG'='sectorLlegada'),
                 RijActividadEmp:=i.RijCorregido]
  
  # Regresiones
  formulas <- cbind(expand.grid(c('variacionReal','variacionRealTrimestral'),
                                c('RijOcupaciones','RijOcupacionesUnem','RijOcupacionesEmp'),
                                c('RijActividad','RijActividadUnem','RijActividadEmp')))
  formulas <- paste(formulas$Var1,'~',formulas$Var2,'+',formulas$Var3,'+ SECTOR_ABOVE_AVG_LAG + CAT_OCUP_LAG + ANO4 + SECTOR_ABOVE_AVG + CAT_OCUP')
  
  names(formulas) <- c('VarReal_RijOcup_RijAct','VarRealTrim_RijOcup_RijAct',
                       'VarReal_RijOcupUnem_RijAct','VarRealTrim_RijOcupUnem_RijAct',
                       'VarReal_RijOcupEmp_RijAct','VarRealTrim_RijOcupEmp_RijAct',
                       'VarReal_RijOcup_RijActUnem','VarRealTrim_RijOcup_RijActUnem',
                       'VarReal_RijOcupUnem_RijActUnem','VarRealTrim_RijOcupUnem_RijActUnem',
                       'varReal_RijOcupEmp_RijActUnem','varRealTrim_RijOcupEmp_RijActUnem',
                       'varReal_RijOcup_RijActEmp','varRealTrim_RijOcup_RijActEmp',
                       'varReal_RijOcupUnem_RijActEmp','varRealTrim_RijOcupUnem_RijActEmp',
                       'varReal_RijOcupEmp_RijActEmp','varRealTrim_RijOcupEmp_RijActEmp')
  
  
  grupos <- list('partTime'='datos[PP3E_TOT < 35 & H_LAG <35 ]',
                 'fullTime'='datos[PP3E_TOT>=35 & H_LAG >=35]',
                 'partTimeUnem'='datos[PP3E_TOT<35 & H_LAG <35 & UNEM_TRANS==TRUE]',
                 'fullTimeUnem'='datos[PP3E_TOT>=35 & H_LAG >=35 & UNEM_TRANS==TRUE]',
                 'partTimeEmp'='datos[PP3E_TOT<35 & H_LAG <35 & !UNEM_TRANS==TRUE]',
                 'fullTimeEmp'='datos[PP3E_TOT>=35 & H_LAG >=35 & !UNEM_TRANS==TRUE]',
                 'pooled'='datos',
                 'pooledUnem'='datos[UNEM_TRANS==TRUE]',
                 'pooledEmp'='datos[!UNEM_TRANS==TRUE]')
  
  regresiones <- lapply(formulas,function(x){
    lapply(grupos,function(y) {
      tidyRegOutput(reg = lm(formula=as.formula(x),
                             data=eval(parse(text=y))),coef.int = 0.95)
      
    })
  })
  
  names(regresiones) <- names(formulas)
  
  
  # for(i in 1:length(regresiones)) {
  #   for(j in 1:length(regresiones[[i]])) {
  #     xlsx::write.xlsx(file = "RegresionesSalida_09-03-2020.xlsx",
  #                      x = as.data.table(regresiones[[i]][[j]]),
  #                      sheetName = paste0('Modelo_',i,'_',j),
  #                      row.names = FALSE,
  #                      append = TRUE)
  #   }
  # }
  
  salida <- lapply(regresiones,function(x){
    grupo <- rep(names(x),sapply(x,function(y) nrow(y)))
    x <- rbindlist(x)
    x$grupo <- grupo
    return(x)
  })
  
  modelo  <- rep(names(salida),sapply(salida,function(x) nrow(x)))
  
  salida <- rbindlist(salida)
  salida$modelo <- modelo
  salida$grupoDistancia <- gruposDist
  listaModelos <- c(listaModelos,list(salida))
}


listaModelos <- rbindlist(listaModelos)
listaModelos <- listaModelos[modelo %in% c('VarRealTrim_RijOcup_RijAct',
                                           'varRealTrim_RijOcupEmp_RijActEmp',
                                           'VarRealTrim_RijOcupUnem_RijActUnem')]
listaModelos <- listaModelos[,grupoDistancia:=ifelse(modelo=='VarReal_RijOcup_RijAct',grupoDistancia,
                                                     ifelse(modelo=='varRealTrim_RijOcupEmp_RijActEmp',paste(grupoDistancia,'Emp',sep=''),
                                                            ifelse(modelo=='VarRealTrim_RijOcupUnem_RijActUnem',paste(grupoDistancia,'Unem',sep=''),grupoDistancia)))]


### Coefficients Plot

coeficientePlot  <- ggplot(data=listaModelos[grepl('*RijOcc*',term)],
                           aes(x = grupoDistancia,y = estimate, ymin = lb, ymax = ub))+
  geom_pointrange()+
  geom_hline(yintercept =0, linetype=2) +
  xlab('Variables Dependientes')+ ylab("Coeficiente asociado a la similitud entre ocupaciones\nIntervalo de 90% de confianza")+
  geom_errorbar(width=0.5,cex=1)+ coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y=element_blank(),
        axis.text.x=element_text(size=12),
        axis.ticks.y=element_blank(),
        axis.text=element_text(color='black'),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180))  +
  facet_grid(grupoDistancia~ grupo,scales = 'free',switch ="y")

coeficientePlot



