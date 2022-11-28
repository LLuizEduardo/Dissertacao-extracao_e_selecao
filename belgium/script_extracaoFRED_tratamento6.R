

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(glmnet)     #selection of variables (Elasticnet)
library(tseries)    #testes adf
library(seastests)  #sazonal test
#library(stats)
#library(forecast)


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = 'belgium'
  setwd(paste0(rootFolder,folder))
  # lista -------------------------------------------------------------------
  belgium <- list('CPMNACSCAB1GQBE','BELBRODFT02STSAM','BELLOCOBDNOSTSAM','BELLOCOBDORSTSAM','BCEMFT02BEM460S','BSEMFT02BEM460S','BVEMFT02BEM460S','BVEMTE02BEM460S','CP0410BEM086NEST','CP0500BEM086NEST','CP0400BEM086NEST','CP1252BEM086NEST','CP0430BEM086NEST','CP0431BEM086NEST','00XHOUBEM086NEST','CP0432BEM086NEST','SERVHOBEM086NEST','CP0550BEM086NEST','CP0440BEM086NEST','BELPERMITMISMEI','BSCICP02BEM460S','BSCICP03BEM665S','BSFGLV02BEM460S','BSOBLV02BEM460S','BSPRFT02BEM460S','BSPRTE02BEM460S','BSSPFT02BEM460S','BELPPDMMINMEI','PIEAFD02BEM661N','PIEAMP02BEM657N','BELPIEAFD02GPM','BELPROMANMISMEI','BELPRMNCG02IXOBSAM','BELPRMNCG03IXOBSAM','BELPRMNIG01IXOBSAM','BELPRMNVG01IXOBSAM','BELPRMNTO01GPSAM','CP0611BEM086NEST','BELLOCOBPNOSTSAM','BELLOCOBPORSTSAM','BELPRCNTO01IXOBM','BELPRINTO01GPSAM','BVCICP02BEM460S','BVDEFT02BEM460S','BVDETE02BEM460S','BELCPHPSE01IXOBM','BELCPGRSE01IXOBM','CPSEHO05BEM661N','CP1120BEM086NEST','CP0562BEM086NEST','CP1260BEM086NEST','CP0621BEM086NEST','SERV00BEM086NEST','SERVCOBEM086NEST','SERVRPBEM086NEST','SERVTRBEM086NEST','SERVMIBEM086NEST','CP0730BEM086NEST','LRHUTTTTBEM156S','LMUNRLTTBEM647S','LMUNRRTTBEM156S','LFHUTTTTBEM647S','NBBEBIS','NNBEBIS','CCUSMA02BEM661N','RBBEBIS','CCRETT01BEM661N','RNBEBIS','CCUSSP01BEM650N','CSINFT02BEM460S','BELCPHPLA01IXOBM','CPGRLE01BEM657N','IR3TIB01BEM156N','IRSTCI01BEM156N','IRLTLT01BEM156N','IRLTCT01BEM156N','BSXRLV02BEM086S','XTEXVA01BEM657S','BELXTEXVA01CXMLM','BELLOCOBXNOSTSAM','BELLOCOBXORSTSAM','EXP4231','XTIMVA01BEM657S','BELXTIMVA01CXMLM','IMP4231')
  lista <- belgium


# extract code first (gdp)-----------------------------------------------------
c<-getSymbols(lista[[1]], src = 'FRED', env = NULL)
  x<-matrix(ncol = 4, nrow = length(c))
  x[,4]<-c
  x[,3]<-substr(index(c), start = 1, stop = 4)
  x[,2]<-substr(index(c), start = 6, stop = 7)
    x[,1][x[,2]=="01"]<-"03"
    x[,1][x[,2]=="04"]<-"06"
    x[,1][x[,2]=="07"]<-"09"
    x[,1][x[,2]=="10"]<-"12"
  
  x[,2]<-NA
  x[,2]<-paste0(x[,3],"-",x[,1])
    x<-x[,-1]
    x<-x[,-2]
  
  x<-data.frame(x)
  colnames(x)<-c("date",colnames(c))
  c<-x
  c[,2]<-as.double(c[,2])
    rm(x)


    #extracao mensal
f<-getSymbols(lista[[2]], src = 'FRED', env = NULL)
  f<-apply.monthly(f, mean, na.rm = TRUE)
  f<-data.frame(date = index(f), coredata(f))
  f$date<-substr(f$date, start = 1, stop = 7)
  if (isSeasonal(f[,2], test = "combined", freq = 4) == TRUE) {
    plot.ts(f[,2], main = i )
  }else{print("ok")}
  #loop
for (i in 3:(length(lista))) {
    d<-getSymbols(lista[[i]], src = 'FRED', env = NULL)
    d<-apply.monthly(d, mean, na.rm = TRUE)
    d<-data.frame(date = index(d), coredata(d))
    d$date<-substr(d$date, start = 1, stop = 7)
    
    if (isSeasonal(d[,2], test = "combined", freq = 12) == TRUE) {
       d[13:nrow(d),2]<-diff(d[,2], lag = 12, diff = 1)
      print(paste0("Seazonality - ",i))
    }
    
    if (max(d$date) >= '2021-12' & min(d$date) <= '2002-12') {
      f<-merge(f,d, by = c('date'), all = TRUE)
    }
}

  
  
  
  #retirado LCWRMN01BEM661N
  
  
  #testar se passa a sazonalidade
  
  
for (i in 2:ncol(f)) {
  if (isSeasonal(f[,i], test = "combined", freq = 12) == TRUE) {
    plot.ts(f[,i], main = i )
    print(paste0('erro - ',i))
  }else{print(paste0('ok - ',i))}
}
  
  
BaseBrutaMensal<-f[,-1]  
rownames(BaseBrutaMensal)<-f[,1]  


f1<-f[,-1]
f1<-ts(f1)
f1<-as.xts(f1)
b<-paste0(f[,1],'-01')
b<-as.Date(b)
index(f1)<-b

  
  # transformando em base trimestral
  for (i in 1:ncol(f1)) {
    e<-apply.quarterly(f1[,i], mean, na.rm = TRUE)
    e<-data.frame(date = index(e), coredata(e))
    e$date<-substr(e$date, start = 1, stop = 7)
    e<-na.omit(e)
    
    c<-merge(c,e, by = c('date'), all = TRUE)
    print(i)
  }    
rm(e)

BaseBrutaTrim<-c[,-1]
rownames(BaseBrutaTrim)<-c[,1]

# ajust the window  -----------------------------------------------------------
d<-na.omit(c)
max(d$date)
min(d$date)



# testar sazonalidade trimestral
for (i in 2:ncol(d)) {
  if (isSeasonal(d[,i], test = "combined", freq = 4) == TRUE) {
    plot.ts(d[,i], main = i )
    print(paste0('erro - ',i))
  }else{print(paste0('ok - ',i))}
}



# extract financial market ------------------------------------------------
mfList<-c('^BFX','BELM.BR')
mf<-suppressWarnings(getSymbols(mfList[1], src = 'yahoo', env = NULL,
                                    periodicity = 'monthly', from = '1990-01-01'))
mf<-mf[,6]
mf<-apply.quarterly(mf, mean, na.rm = TRUE)
mf<-data.frame(date = index(mf), coredata(mf))
mf$date<-substr(mf$date, start = 1, stop = 7)
names(mf) <- sub(".Adjusted", "", names(mf))

#

  if (isSeasonal(mf[,2], test = "combined", freq = 4) == TRUE) {
    plot.ts(mf[,2], main = colnames(mf[,2]) )
    print(paste0('erro - ',i))
  }else{print('ok')}



for (i in 2:length(mfList)) {
  x<-suppressWarnings(getSymbols(mfList[i], src = 'yahoo', env = NULL,
                                  periodicity = 'monthly', from = '1990-01-01'))
  x<-x[,6]
  x<-apply.quarterly(x, mean, na.rm = TRUE)
  x<-data.frame(date = index(x), coredata(x))
  x$date<-substr(x$date, start = 1, stop = 7)
  names(x) <- sub(".Adjusted", "", names(x))
  
  if (max(x$date) >= '2021-12' & min(x$date) <= '2002-12') {
    mf<-merge(mf,x, by = c('date'), all = TRUE)
  }
  #mf<-merge(mf,x, by = c('date'), all = TRUE)
}


d<-merge(d,mf, by = c('date'), all = TRUE)
rm(mf)
d1<-na.omit(d)
max(d1$date)
min(d1$date)

d<-na.omit(d)
rm(d1)


#========== sazonalidade

k<-d[,-1]
k<-ts(k)
rownames(k)<-d[,1]


# AJUSTAR SERIES (APLICAR RETORNOS)

t<-c(
  'BELBRODFT02STSAM',
  'BELLOCOBDORSTSAM',
  'BCEMFT02BEM460S',
  'BSEMFT02BEM460S',
  'BVEMFT02BEM460S',
  'BVEMTE02BEM460S',
  'BSCICP02BEM460S',
  'BSFGLV02BEM460S',
  'BSOBLV02BEM460S',
  'BSPRFT02BEM460S',
  'BSPRTE02BEM460S',
  'BSSPFT02BEM460S',
  'PIEAMP02BEM657N',
  'BELPIEAFD02GPM',
  'BELPRMNTO01GPSAM',
  'BELLOCOBPORSTSAM',
  'BELPRINTO01GPSAM',
  'BVCICP02BEM460S',
  'BVDEFT02BEM460S',
  'BVDETE02BEM460S',
  'LRHUTTTTBEM156S',
  'LMUNRRTTBEM156S',
  'CSINFT02BEM460S',
  'CPGRLE01BEM657N',
  'IR3TIB01BEM156N',
  'IRSTCI01BEM156N',
  'IRLTLT01BEM156N',
  'IRLTCT01BEM156N',
  'XTEXVA01BEM657S',
  'BELLOCOBXORSTSAM',
  'XTIMVA01BEM657S'
  )


#period return
for (i in 1:ncol(k)) {
  if (colnames(d[i+1]) %in% t) {
    k[,i]<-k[,i]
  }else{
    k[,i]<-periodReturn(k[,i], period='quarterly')
  }
}

# /100
for (i in 1:ncol(k)) {
  if (colnames(d[i+1]) %in% t) {
    k[,i]<-k[,i]/100
  }
}

rm(t)
rm(mfList,x)


# c is a complete database without the financial market and data cutting
# d is a complete database without treatments
# k is database with calculated returns



for (i in 1:ncol(k)) {
  print( paste0(isSeasonal(k[,i], test = "combined", freq = 4),' - ',i))
}

# RETIRAR
k<-k[,-49]
k<-k[,-48]

# evaluating the plot of the graphic
for (i in 2:ncol(k)) {
  plot.ts(k[,i], )
}








# testar estacionaridade das series ---------------------------------------


# testing the typeof



k1<-k[-1,]
k2<-k1[-1,]
k3<-k2[-1,]
k4<-k3[-1,]

diffAp<-result<-pValores<-matrix(ncol = 4, nrow = ncol(k))

colnames(diffAp)<-colnames(result)<-colnames(pValores)<-c('1r','2r','3r','4r')
pValoresa<-pValores3<-pValores2<-pValores1<-pValores0<-list()


##############################

#=============================================================
#k trocado por k1
########diffAp<-result<-pValores<-matrix(ncol = 4, nrow = ncol(k2))
for (i in 1:ncol(k)) {
  # 1r=======================================================
  pValores0[i]<-list(suppressWarnings(adf.test(k1[,i])))
  pValores[i,1]<-pValores0[[i]][["p.value"]]
 
  if (pValores[i,1] > 0.05){
     k2[,i]<- diff(k1[,i],lag = 1, differences = 1)
     diffAp[i,1]<-1
  }else{
    result[i,1]<-'Estacionario'
  }
  
  # 2r=======================================================
  pValores1[i]<-list(suppressWarnings(adf.test(k2[,i])))
  pValores[i,2]<-pValores1[[i]][["p.value"]]
  
  if (pValores[i,2] > 0.05){
    k3[,i]<- diff(k2[,i],lag = 1, differences = 1)
    diffAp[i,2]<-1
  }else{
    result[i,2]<-'Estacionario'
  }
  # 3r=======================================================
   pValores2[i]<-list(suppressWarnings(adf.test(k3[,i])))
   pValores[i,3]<-pValores2[[i]][["p.value"]]

  if (pValores[i,3] > 0.05){
     k4[,i]<- diff(k3[,i],lag = 1, differences = 1)
     diffAp[i,3]<-1
   }else{
     result[i,3]<-'Estacionario'
   }
   
   pValores3[i]<-list(suppressWarnings(adf.test(k4[,i])))
   pValores[i,4]<-pValores3[[i]][["p.value"]]
   if (pValores[i,3] > 0.05){
     k4[,i]<- diff(k3[,i],lag = 1, differences = 1)
     diffAp[i,4]<-1
   }else{
     result[i,4]<-'Estacionario'
   }
}






#tratar por fora
a<-80
k2[,a]<- diff(k1[,a],lag = 1, differences = 1)
#diffAp[a,1]<-1

k2<-k2[,-18]



a<-k2
resulta<-matrix(ncol = 4, nrow = ncol(a))
#---------------------------------
for (i in 1:ncol(a)) {
  # 1r=======================================================
  pValoresa[i]<-list(suppressWarnings(adf.test(a[,i])))
  pValores[i,1]<-pValoresa[[i]][["p.value"]]
  
  if (pValores[i,1] < 0.05){
    resulta[i,2]<-'Estacionario'
  }
}
#---------------------------------


rm(pValores0,pValores1,pValores2,pValores3,pValoresa)

# selecionando as variaveis -----------------------------------------------
Z<-k2
row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)
row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)
row.names(resulta)<-colnames(a)


# testar se todas as series sao estacionarias -----------------------------
# dfpValores<-as.data.frame(pValores)
# filter(dfpValores,dfpValores[,2]>0.05)
# rm(dfpValores)
#d5<-d4[,-(grep('LRHUTTTTBEM156S', colnames(d4)))]

#rm(x,y,xStar,i,I,cv,codigos_selecionados,betas)

rm(a,b)
  
  x<-Z[,2:ncol(Z)]
  y=Z[,1]
  
  y<-as.matrix(y)
  x<-as.matrix(x)
  colnames(y)<-colnames(Z[1])
  
  #y<-as.double(y)
  
  cv=cv.glmnet(x,y)
  eq=glmnet(x,y, family = 'gaussian', lambda = cv$lambda.min)
  betas=eq$beta
  I=which(betas!=0)
  
  xStar=x[,I]

  codigos_selecionados <- matrix(ncol = 2, nrow = ncol(xStar))
  colnames(codigos_selecionados)<- c('country', 'codigo')

  xStar<-as.data.frame(xStar)
  codigos_selecionados[,1]<-folder
  
  for (i in 1:ncol(xStar)) {
    codigos_selecionados[i,2]<-colnames(xStar[i])
  }




View(codigos_selecionados)

  write.csv(codigos_selecionados, paste0(rootFolder,'_Base_Selecao_Codigos/',folder,'.csv'))
  write.csv(pValores, paste0(rootFolder,folder,'/csv.pValores.csv'))
  write.csv(result, paste0(rootFolder,folder,'/csv.Result_Estacionaridade.csv'))
  write.csv(diffAp, paste0(rootFolder,folder,'/csv.Diff_Aplicadas.csv'))
  save.image(paste0(rootFolder,folder,'/base.RData'))

