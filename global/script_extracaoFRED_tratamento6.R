

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(glmnet)     #selection of variables (Elasticnet)
library(tseries)    #testes adf
library(seastests)  #sazonal test


# set folder ------------------------------------------------------------------
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = 'global'
  setwd(paste0(rootFolder,folder))
  global <- list('IGREA','CUURA000SACL1E','CUURX000SACL1E','CUURA000SACL1','CUURX000SACL1','CUURA000SACE','CUURX000SACE','CUURA000SAM1','CUURX000SAM1','MCOILBRENTEU','MCOILWTICO','EMVCOMMMKT','IQAG','IQEXAG','PALLFNFINDEXM','IR','MJFUELUSGULF','PPIACO','WPS012201','WPS014102','WPS011101','WPS012202','WPS011301','WPS012','WPS018','WPS0183','WPS011102','WPS0122','WPS016','WPS013201','WPS0131','WPS0141','WPS013102','WPS0132','WPS0133','WPS013','WPS014','WPS013101','WPS0142','WPS013103','WPS011303','WPS0511','WPS0512','WPS051','WPS0542','WPS0552','WPS054','WPS0571','WPS057302','WPS054321','WPS0553','WPS0573','WPS0554','WPS057303','WPS057','WPS051101','WPS0541','WPS0551','WPS057105','WPS057103','WPS057104','WPS055','WPS043','WPS041','WPS042','WPS043105','WPS0431','PPIIDC','PPICMM','WPS0261','WPS022101','WPS026','WPS023106','WPS023201','WPS021','WPS0214','WPS0254','WPS023','WPS026102','WPS0231','WPS0293','WPS0221','WPS022','WPS0294','WPS023302','WPS0233','WPS0264','WPS022105','WPS023105','WPS023103','WPS029','WPS023303','WPS0222','WPS0292','WPS022206','WPS0223','WPS022203','WPS032601')
  lista <- global

# extract code first (gdp)-----------------------------------------------------
c<-getSymbols(lista[[1]], src = 'FRED', env = NULL)
  x<-matrix(ncol = 4, nrow = length(c))
  x[,4]<-c
  x[,3]<-substr(index(c), start = 1, stop = 4)
  x[,2]<-substr(index(c), start = 6, stop = 7)
    #x[,1][x[,2]=="01"]<-"03"
    #x[,1][x[,2]=="04"]<-"06"
    #x[,1][x[,2]=="07"]<-"09"
    #x[,1][x[,2]=="10"]<-"12"
  
  #x[,2]<-NA
  x[,1]<-paste0(x[,3],"-",x[,2])
    x<-x[,-2]
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
}

  
  
  
  #testing seasonality
  
  
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
#-------------------------------------------------
#   
#   # changing querterly data
#   for (i in 1:ncol(f1)) {
#     e<-apply.quarterly(f1[,i], mean, na.rm = TRUE)
#     e<-data.frame(date = index(e), coredata(e))
#     e$date<-substr(e$date, start = 1, stop = 7)
#     e<-na.omit(e)
#     
#     c<-merge(c,e, by = c('date'), all = TRUE)
#     print(i)
#   }    
# rm(e)

c<-merge(c,f, by = c('date'), all = TRUE)

# BaseBrutaTrim<-c[,-1]
# rownames(BaseBrutaTrim)<-c[,1]

c<-c[,-49]
# ajusting the window  -----------------------------------------------------------
d<-na.omit(c)
max(d$date)
min(d$date)



# testing quarterly seasonality
for (i in 2:ncol(d)) {
  if (isSeasonal(d[,i], test = "combined", freq = 12) == TRUE) {
    plot.ts(d[,i], main = i )
    print(paste0('erro - ',i))
  }else{print(paste0('ok - ',i))}
}



#========== seasonality

k<-d[,-1]
k<-ts(k)
rownames(k)<-d[,1]



# # adjust series (apply returns)
# 
# t<-c(
#   'DEUBRODFT02STSAM',
#   'DEULOCOBDORSTSAM',
#   'BCEMFT02DEM460S',
#   'BSEMFT02DEM460S',
#   'BVEMTE02DEM460S',
#   'DEUBSBUCT02STSAM',
#   'BSCICP02DEM460S',
#   'BSFGLV02DEM460S',
#   'BSOBLV02DEM460S',
#   'BSOITE02DEM460S',
#   'BSPRFT02DEM460S',
#   'BSPRTE02DEM460S',
#   'BSSPFT02DEM460S',
#   'PIEAMP02DEM659N',
#   'DEUPIEAFD02GPM',
#   'DEUPIEAFD01GPM',
#   'DEUPIEAMP02GPM',
#   'DEUPIEAMP01GPM',
#   'DEUPRMNTO01GPSAM',
#   'DEUPRCNTO01GPSAM',
#   'DEUPRINTO01GPSAM',
#   'BVCICP02DEM460S',
#   'BVDEFT02DEM460S',
#   'BVDETE02DEM460S',
#   'DEUCPHP1200GPM',
#   'DEUCP120000GPM',
#   'LRHUTTTTDEM156S',
#   'LMUNRRTTDEM156S',
#   'CSINFT02DEM460S',
#   'CPGRLE01DEM657N',
#   'IR3TIB01DEM156N',
#   'IRSTCI01DEM156N',
#   'IRLTLT01DEM156N',
#   'IRLTCT01DEM156N',
#   'DEULOCOSIORSTM',
#   'XTEXVA01DEM657S',
#   'DEULOCOBXORSTSAM',
#   'XTIMVA01DEM657S')



#period return
for (i in 1:ncol(k)) {
  
    k[,i]<-periodReturn(k[,i], period='monthly')
  
}

# # /100
# for (i in 1:ncol(k)) {
#   if (colnames(d[i+1]) %in% t) {
#     k[,i]<-k[,i]/100
#   }
# }
# 
# rm(t)
# rm(mfList,x)


# c is a complete database without the financial market and data cutting
# d is a complete database without treatments
# k is database with calculated returns



for (i in 1:ncol(k)) {
  print( paste0(isSeasonal(k[,i], test = "combined", freq = 12),' - ',i))
}
k<-k[,-2]

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




# 
# # testing out
# a<-52
# k2[,a]<- diff(k1[,a],lag = 1, differences = 1)


# k2<-k2[,-27]
# 


a<-k1
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

