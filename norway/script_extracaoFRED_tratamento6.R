

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(glmnet)     #selection of variables (Elasticnet)
library(tseries)    #testes adf
library(seastests)  #sazonal test


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = 'norway'
  setwd(paste0(rootFolder,folder))
  norway <- list('CPMNACSCAB1GQNO','NORCPIHOUMINMEI','CP0410NOM086NEST','CP0500NOM086NEST','CP0400NOM086NEST','CP1252NOM086NEST','CP0430NOM086NEST','CP0431NOM086NEST','CP0444NOM086NEST','00XHOUNOM086NEST','CP0432NOM086NEST','SERVHONOM086NEST','CP0550NOM086NEST','CP0440NOM086NEST','NORPERMITMISMEI','BSCICP03NOM665S','NORPPDMMINMEI','PIEAFD02NOM661N','PIEAMP02NOM659N','NORPIEAFD02GPM','NORPIEAFD01GPM','NORPIEAMP02GPM','NORPIEAMP01GPM','PIEAFD01NOM661N','PIEAMP01NOM661N','NORPROMANMISMEI','NORPRMNIG01IXOBSAM','NORPRMNVG01IXOBSAM','NORPRMNTO01GPSAM','CP0611NOM086NEST','NORLOCOBPNOSTSAM','NORPREND401IXEBSAM','NORPRINTO01GPSAM','NORPRMITO01IXEBSAM','NORCPHP1200GPM','NORCPHPSE01IXOBM','NORCP040400IXOBM','NORCP120000GPM','CP1120NOM086NEST','CP0562NOM086NEST','CP1260NOM086NEST','CP0621NOM086NEST','CP1200NOM086NEST','SERV00NOM086NEST','SERVCONOM086NEST','SERVRPNOM086NEST','SERVTRNOM086NEST','SERVMINOM086NEST','CP0730NOM086NEST','LRHUTTTTNOM156S','LMUNRLTTNOM647S','LMUNRRTTNOM156S','LFHUTTTTNOM647S','LFHUTTFENOM647S','LFHUTTMANOM647S','NBNOBIS','NNNOBIS','CCUSMA02NOM661N','EXNOUS','RBNOBIS','CCRETT01NOM661N','RNNOBIS','CCUSSP01NOM650N','NORCPHPLA01IXOBM','CPGRLE01NOM657N','IR3TIB01NOM156N','IRSTCI01NOM156N','IRLTLT01NOM156N','XTEXVA03NOM664S','XTEXVA01NOM657S','NORXTEXVA01CXMLM','NORLOCOTXNOSTSAM','NORLOCOTXORCXMLM','EXP4039','CPGDIM01NOM661N','XTIMVA03NOM664S','XTIMVA01NOM657S','NORXTIMVA01CXMLM','IMP4039')
  lista <- norway
  
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
    
    
    #monthly extract
f<-getSymbols(lista[[3]], src = 'FRED', env = NULL)
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
for (i in 4:(length(lista))) {
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
mfList<-c('OSEBX.OL')
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

t<-c('PIEAMP02NOM659N','NORPIEAFD02GPM','NORPIEAFD01GPM','NORPIEAMP02GPM','NORPIEAMP01GPM','NORPRMNTO01GPSAM','NORPRINTO01GPSAM','NORCPHP1200GPM','NORCP120000GPM','LRHUTTTTNOM156S','LMUNRRTTNOM156S','CPGRLE01NOM657N','IR3TIB01NOM156N','IRSTCI01NOM156N','IRLTLT01NOM156N','XTEXVA01NOM657S','XTIMVA01NOM657S'
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
                ,' - no apparent seasonality - ',i))
}
#k<-k[,-44]

# evaluating the plot of the graphic
for (i in 2:ncol(k)) {
  plot.ts(k[,i], main = i)
}


# testing stationarity of the series ---------------------------------------
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
#a<-16
#k2[,a]<- diff(k1[,a],lag = 1, differences = 1)


k2<-k2[,-63]
k2<-k2[,-17]



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

