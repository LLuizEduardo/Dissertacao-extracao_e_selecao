

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(glmnet)     #selection of variables (Elasticnet)
library(tseries)    #testes adf
library(seastests)  #sazonal test


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = 'usa'
  setwd(paste0(rootFolder,folder))
  usa <- list('A191RP1Q027SBEA','AWHNONAG','A132RC1','CES6056170001','ASPNHSUS','AUTHNOTT','CPIEHOUSE','CAPUTLGMFDS','CAPUTLGMFNS','CAPUTLGMFOS','CAPGMFDS','CAPGMFS','CAPG315S','CAPGMFNS','CAPGMFOS','AMDMUO','AMTMUO','CC4WSA','BC0DCBM027SBOG','BC0FRIM027SBOG','BC0LCBM027SBOG','BC0SCBM027SBOG','EXPINF10YR','EXPINF1YR','EXPINF20YR','EXPINF30YR','CPIAUCSL','CUSR0000SAC','CUSR0000SAD','CPIENGSL','CPIUFDSL','CUSR0000SEHE','CUSR0000SETB01','CUSR0000SAH21','CUSR0000SETA01','CUSR0000SAN','CUSR0000SAS','CPITRNSL','CPGRLE01USM657N','CPALWE01USM661N','EMVMACROINFLATION','DPCCRC1M027SBEA','PCU212399212399','PCU212113212113','PCU2111112111111','PCU2111112111113','PCU311311','PCU2122221222','PCU212322212322','PCU212210212210','PCU311213311213','PCU21222122','PCU221210221210114','PCU21112111','PCU311212311212','PCU213112213112','CPIEALL','MICH','HQMCB100YR','HQMCB10YRP','HQMCB10YR','HQMCB1YR','DTB1YR','HQMCB20YR','CPF1M','CPN1M','DTP30A28','DTP30A29','HQMCB30YRP','HQMCB30YR','HQMCB40YR','HQMCB50YR','HQMCB60YR','DTB6','HQMCB70YR','HQMCB80YR','CPF3M','CPN3M','EMVMACROINTEREST','FEDFUNDS','BAMLC7A0C1015YEY','BAMLC1A0C13YEY','BAMLC8A0C15PYEY','BAMLC2A0C35YEY','BAMLC4A0C710YEY','BAMLC0A2CAAEY','BAMLC0A1CAAAEY','BAMLH0A1HYBBEY','BAMLH0A1HYBB','BAMLHYH0A1BBTRIV','BAMLC0A4CBBBEY','BAMLH0A3HYCEY','BAMLH0A3HYC','BAMLHYH0A3CMTRIV','BAMLC0A3CAEY','BAMLH0A2HYBEY','BAMLH0A2HYB','BAMLH0A2HYBSYTW','BAMLHYH0A2BTRIV','BAMLC0A0CMEY','BAMLH0A0HYM2EY','BAMLH0A0HYM2','BAMLH0A0HYM2SYTW','BAMLHYH0A0HYM2TRIV','GS10','GS1','GS20','GS30','GS5','AAAFFM','AAA','AAA10YM','BAAFFM','BAA','BAA10YM','B069RC1','M2SL','M2MSL','M2REAL','T10Y2YM','T10Y3MM','T10YFFM','T1YFFM','T3MFFM','T5YFFM','T6MFFM','TEDRATE','WOTHAST','BOPGSTB','BOPGTB','BOPSTB','AUESA','BSXRLV02USM086S','BOPTEXP','EXPTOTUS','BOPGEXP','EXPMANUS','EXPNONUS','BOPSEXP','EECTOT','BOPTIMP','BOPGIMP','BOPSIMP')
  lista <- usa
  
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
f<-getSymbols(lista[[2]], src = 'FRED', env = NULL)
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
mfList<-c('^GSPC','^DJI','^VIX')
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
  'CAPUTLGMFDS','CAPUTLGMFNS','CAPUTLGMFOS','A191RP1Q027SBEA','EXPINF10YR','EXPINF1YR','EXPINF20YR','EXPINF30YR','CPGRLE01USM657N','MICH','HQMCB100YR','HQMCB10YRP','HQMCB10YR','HQMCB1YR','DTB1YR','HQMCB20YR','CPF1M','CPN1M','DTP30A28','DTP30A29','HQMCB30YRP','HQMCB30YR','HQMCB40YR','HQMCB50YR','HQMCB60YR','DTB6','HQMCB70YR','HQMCB80YR','CPF3M','CPN3M','FEDFUNDS','BAMLC7A0C1015YEY','BAMLC1A0C13YEY','BAMLC8A0C15PYEY','BAMLC2A0C35YEY','BAMLC4A0C710YEY','BAMLC0A2CAAEY','BAMLC0A1CAAAEY','BAMLH0A1HYBBEY','BAMLH0A1HYBB','BAMLC0A4CBBBEY','BAMLH0A3HYCEY','BAMLH0A3HYC','BAMLC0A3CAEY','BAMLH0A2HYBEY','BAMLH0A2HYB','BAMLH0A2HYBSYTW','BAMLC0A0CMEY','BAMLH0A0HYM2EY','BAMLH0A0HYM2','BAMLH0A0HYM2SYTW','GS10','GS1','GS20','GS30','GS5','AAAFFM','AAA','AAA10YM','BAAFFM','BAA','BAA10YM','T10Y2YM','T10Y3MM','T10YFFM','T1YFFM','T3MFFM','T5YFFM','T6MFFM','TEDRATE','BSXRLV02USM086S'
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
#k<-k[,-34]

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
a<-123
k2[,a]<- diff(k1[,a],lag = 1, differences = 1)


k2<-k2[,-108]
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

