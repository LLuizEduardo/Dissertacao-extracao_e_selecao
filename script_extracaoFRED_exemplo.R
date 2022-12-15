

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(glmnet)     #selection of variables (Elasticnet)
library(tseries)    #testes adf
library(seastests)  #sazonal test


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = 'united kingdom'
  setwd(paste0(rootFolder,folder))
  united_kingdom <- list('UKNGDP','GBRBRODFT02STSAM','GBRLOCOBDNOSTSAM','GBRLOCOBDORSTSAM','BCEMFT02GBM460S','BSEMFT02GBM460S','BVEMFT02GBM460S','BVEMTE02GBM460S','GBRCP040200IXOBM','GBRCPGRHO01IXOBM','GBRCPGRLH01IXOBM','CPRPTT01GBM661N','CPRPTT02GBM661N','GBRCPISXHMINMEI','CP0410GBM086NEST','CP0500GBM086NEST','CP0400GBM086NEST','CP1252GBM086NEST','CP0430GBM086NEST','CP0431GBM086NEST','00XHOUGBM086NEST','CP0432GBM086NEST','SERVHOGBM086NEST','CP0550GBM086NEST','CP0440GBM086NEST','BSCICP02GBM460S','BSCICP03GBM665S','BSFGLV02GBM460S','BSOBLV02GBM460S','BSPRFT02GBM460S','BSPRTE02GBM460S','BSSPFT02GBM460S','GBRPPDMMINMEI','LCEAMN01GBM661S','GBRLCEAMN01GPSAM','PIEAMP02GBM659N','GBRPIEAMP02GPM','GBRPROMANMISMEI','GBRPRMNCG02IXOBSAM','GBRPRMNCG03IXOBSAM','GBRPRMNIG01IXOBSAM','GBRPRMNVG01IXOBSAM','GBRPRMNTO01GPSAM','LCEAMN02GBM661S','CP0611GBM086NEST','GBRLOCOBPNOSTSAM','GBRLOCOBPORSTSAM','GBRPREND401IXOBM','GBRPRINTO01GPSAM','GBRPRMITO01IXOBSAM','BVCICP02GBM460S','BVDEFT02GBM460S','BVDETE02GBM460S','GBRCPHP1200IXOBM','GBRCPHPSE01IXOBM','GBRCP040400IXOBM','GBRCP120000GPM','GBRCPGRLH02IXOBM','GBRCPGRSE01IXOBM','CPSEHO05GBM661N','CP1120GBM086NEST','CP0562GBM086NEST','CP1260GBM086NEST','CP0630GBM086NEST','CP0621GBM086NEST','CP1200GBM086NEST','SERV00GBM086NEST','SERVCOGBM086NEST','SERVRPGBM086NEST','SERVTRGBM086NEST','SERVMIGBM086NEST','CP0730GBM086NEST','LRHUTTTTGBM156S','LMUNRLTTGBM647S','LMUNRRTTGBM156S','LFHUTTTTGBM647S','NBGBBIS','CCEUSP01GBM651N','NNGBBIS','CCUSMA02GBM661N','RBGBBIS','CCRETT01GBM661N','RNGBBIS','EXUSUK','CCUSSP01GBM650N','CSINFT02GBM460S','GBRCPHPLA01IXOBM','CPGRLE01GBM657N','IR3TIB01GBM156N','IRSTCI01GBM156N','INTGSBGBM193N','IRLTLT01GBM156N','TRESEGGBM052N','BSXRLV02GBM086S','XTEXVA01GBM657S','GBRXTEXVA01CXMLM','EXPUK','XTIMVA01GBM657S','GBRXTIMVA01CXMLM','IMPUK')
  lista <- united_kingdom
  
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

    if (isSeasonal(c[,2], test = "combined", freq = 4) == TRUE) {
      c[5:nrow(c),2]<-diff(c[,2], lag = 4, diff = 1)
      plot.ts(c[,2])
      print("Seazonality")
    }else{
      plot.ts(c[,2])
      print("ok")}
#    
#    
#monthly extract
f<-getSymbols(lista[[9]], src = 'FRED', env = NULL)
  f<-apply.monthly(f, mean, na.rm = TRUE)
  f<-data.frame(date = index(f), coredata(f))
  f$date<-substr(f$date, start = 1, stop = 7)
  if (isSeasonal(f[,2], test = "combined", freq = 12) == TRUE) {
    f[13:nrow(f),2]<-diff(f[,2], lag = 12, diff = 1)
    plot.ts(f[,2])
    print("Seazonality")
  }else{
    plot.ts(f[,2])
    print("ok")}
  
  
#loop
for (i in 10:(length(lista))) {
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
    if (i == length(lista)) {
      print("Finalizado!")
    }
}

  
  
#testing seasonality
for (i in 2:ncol(f)) {
  if (isSeasonal(f[,i], test = "combined", freq = 12) == TRUE) {
    plot.ts(f[,i], main = i )
    print(paste0('apparent seasonality - ',i))
  }else{print(paste0('no apparent seasonality - ',i))}
  if (i == ncol(f)) {
    print("Finalizado!")
  }
}
  

BaseBrutaMensal<-f[,-1]  
rownames(BaseBrutaMensal)<-f[,1]  


f1<-f[,-1]
f1<-ts(f1)
f1<-as.xts(f1)
b<-paste0(f[,1],'-01')
b<-as.Date(b)
index(f1)<-b

  
# changing quarterly data
for (i in 1:ncol(f1)) {
  e<-apply.quarterly(f1[,i], mean, na.rm = TRUE)
  e<-data.frame(date = index(e), coredata(e))
  e$date<-substr(e$date, start = 1, stop = 7)
  e<-na.omit(e)
  
  c<-merge(c,e, by = c('date'), all = TRUE)
  print(i)
  if (i == ncol(f1)) {
    print("Finalizado!")
  }
}    
rm(e)

BaseBrutaTrim<-c[,-1]
rownames(BaseBrutaTrim)<-c[,1]

# ajusting the window  -----------------------------------------------------------
d<-na.omit(c)
max(d$date)
min(d$date)



# testing quarterly seasonality
for (i in 2:ncol(d)) {
  if (isSeasonal(d[,i], test = "combined", freq = 4) == TRUE) {
    plot.ts(d[,i], main = i )
    print(paste0('seasonality apparent - ',i))
  }else{print(paste0('ok - ',i))}
}



# extract financial market ------------------------------------------------
mfList<-c('^FTSE')
mf<-suppressWarnings(getSymbols(mfList[1], src = 'yahoo', env = NULL,
                                   periodicity = 'monthly', from = '1990-01-01'))
mf<-mf[,6]
mf<-apply.quarterly(mf, mean, na.rm = TRUE)
mf<-data.frame(date = index(mf), coredata(mf))
mf$date<-substr(mf$date, start = 1, stop = 7)
names(mf) <- sub(".Adjusted", "", names(mf))

# CSV files
#mf<-read.csv(sep = ";","C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/-V1/v1 Selecao/series csv mercado financeiro/sweden_OMX.csv")
#mf1<-read.csv(sep = ";","C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/-V1/v1 Selecao/series csv mercado financeiro/sweden_OMX30.csv")
#mf<-merge(mf,mf1, by = c('date'), all = TRUE)
# 

if (isSeasonal(mf[,2], test = "combined", freq = 4) == TRUE) {
  plot.ts(mf[,2], main = colnames(mf[,2]) )
  print(paste0('erro - ',i))
}else{print('ok')}


# if there is more than one series
if (length(mfList)>1) {
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
  }  
}else{
 print("Only one") 
}
View(mf)


d<-merge(d,mf, by = c('date'), all = TRUE)
rm(mf)
d1<-na.omit(d)
max(d1$date)
min(d1$date)

d<-na.omit(d)
rm(d1)


#========== seasonality

k<-d[,-1]
k<-ts(k)
rownames(k)<-d[,1]



# adjust series (apply returns)

t<-c(
  'GBRBRODFT02STSAM','GBRLOCOBDORSTSAM','BCEMFT02GBM460S','BSEMFT02GBM460S','BVEMFT02GBM460S','BVEMTE02GBM460S','BSCICP02GBM460S','BSFGLV02GBM460S','BSOBLV02GBM460S','BSPRFT02GBM460S','BSPRTE02GBM460S','BSSPFT02GBM460S','GBRLCEAMN01GPSAM','PIEAMP02GBM659N','GBRPIEAMP02GPM','GBRPRMNTO01GPSAM','GBRLOCOBPORSTSAM','GBRPRINTO01GPSAM','BVCICP02GBM460S','BVDEFT02GBM460S','BVDETE02GBM460S','GBRCP120000GPM','LRHUTTTTGBM156S','LMUNRRTTGBM156S','CSINFT02GBM460S','CPGRLE01GBM657N','IR3TIB01GBM156N','IRSTCI01GBM156N','INTGSBGBM193N','IRLTLT01GBM156N','XTEXVA01GBM657S','XTIMVA01GBM657S'
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
rm(mfList)

View(k)
# c is a complete database without the financial market and data cutting
# d is a complete database without treatments
# k is database with calculated returns



for (i in 1:ncol(k)) {
  print( paste0(isSeasonal(k[,i], test = "combined", freq = 4)
                ,' - ',i))
}
k<-k[,-34]

# evaluating the plot of the graphic
for (i in 2:ncol(k)) {
  plot.ts(k[,i], main = i)
}


# testing stationary of the series ---------------------------------------
k1<-k[-1,]
k2<-k1[-1,]
k3<-k2[-1,]
k4<-k3[-1,]

diffAp<-result<-pValores<-matrix(ncol = 4, nrow = ncol(k))

colnames(diffAp)<-colnames(result)<-colnames(pValores)<-c('1r','2r','3r','4r')
pValoresa<-pValores3<-pValores2<-pValores1<-pValores0<-list()


#=============================================================
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
View(result)


# testing out
#a<-45
#k2[,a]<- diff(k1[,a],lag = 1, differences = 1)


k2<-k2[,-22]
#k2<-k2[,-16]


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
View(resulta)


rm(pValores0,pValores1,pValores2,pValores3,pValoresa)

# selecting the variables -----------------------------------------------
Z<-k2
row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)
row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)
row.names(resulta)<-colnames(a)


rm(a,b)
  
  x<-Z[,2:ncol(Z)]
  y=Z[,1]
  
  y<-as.matrix(y)
  x<-as.matrix(x)
  colnames(y)<-colnames(Z[1])
  
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

