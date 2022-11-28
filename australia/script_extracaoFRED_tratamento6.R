

# libraries -------------------------------------------------------------------
library(quantmod)   #FRED source
library(zoo)        #time series
library(dplyr)      #time filter
library(glmnet)     #selection of variables (Elasticnet)
library(tseries)    #testes adf
library(seastests)  #sazonal test


# set folder ------------------------------------------------------------------
run{
  rootFolder = 'C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/'
  folder = 'australia'
  
  setwd(paste0(rootFolder,folder))
  
  
  # lista -------------------------------------------------------------------
  
australia <- list('NGDPRSAXDCAUQ','AUSLOCOBDNOSTSAM','LFEMTTTTAUM657S',
        'LREMTTTTAUM156S','LFESEETTAUM647S','AUSPERMITMISMEI','BSCICP03AUM665S',
        'AUSLOCOBPNOSTSAM','LRHUTTTTAUM156S','LFHUTTTTAUM647S','NBAUBIS',
        'NNAUBIS','CCUSMA02AUM661N','RBAUBIS','CCRETT01AUM661N','RNAUBIS',
        'EXUSAL','CCUSSP01AUM650N','CSINFT02AUM460S','IR3TBB01AUM156N',
        'IR3TIB01AUM156N','IRSTCI01AUM156N','INTGSBAUM193N','IRLTLT01AUM156N',
        'TRESEGAUM052N','XTEXVA01AUM657S','AUSXTEXVA01CXMLM','EXP6021',
        'XTIMVA01AUM657S','AUSXTIMVA01CXMLM','IMP6021')
  lista <- australia
}

# extract code first (gdp)-----------------------------------------------------
c<-getSymbols(lista[[1]], src = 'FRED', env = NULL)
#run{
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
    rm(x)
#}


# extract all others codes ----------------------------------------------------
for (i in 2:length(lista)) {
    d<-getSymbols(lista[[i]], src = 'FRED', env = NULL)
    d<-apply.quarterly(d, mean, na.rm = TRUE)
    d<-data.frame(date = index(d), coredata(d))
    d$date<-substr(d$date, start = 1, stop = 7)
    
    if (max(d$date) >= '2021-12' & min(d$date) <= '2002-12') {
      c<-merge(c,d, by = c('date'), all = TRUE)
    }
}
    
  

# ajust the window  -----------------------------------------------------------
d<-na.omit(c)
max(d$date)
min(d$date)


# extract financial market ------------------------------------------------

mfList<-c('^AXJO','^AORD','^AXKO')
mf<-suppressWarnings(getSymbols(mfList[1], src = 'yahoo', env = NULL,
                                    periodicity = 'monthly', from = '1990-01-01'))
mf<-mf[,6]
mf<-apply.quarterly(mf, mean, na.rm = TRUE)
mf<-data.frame(date = index(mf), coredata(mf))
mf$date<-substr(mf$date, start = 1, stop = 7)
names(mf) <- sub(".Adjusted", "", names(mf))


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

# mf<-suppressWarnings(getSymbols('^MERV', src = 'yahoo', env = NULL,
#             periodicity = 'monthly', from = '1990-01-01'))
# mf<-mf[,6]
# mf<-apply.quarterly(mf, mean, na.rm = TRUE)
# mf<-data.frame(date = index(mf), coredata(mf))
# mf$date<-substr(mf$date, start = 1, stop = 7)



#mf1<-na.omit

d<-merge(d,mf, by = c('date'), all = TRUE)
rm(mf)
d<-na.omit(d)
#==========================================================================


for (i in 2:ncol(d)) {
  # if (typeof(d[,i]) != 'double') {
  #   d[,i]<-as.double(d[,i])
  # }
#print( isSeasonal(d[,i], test = "combined", freq = 4))
print(paste0( isSeasonal(d[,i], test = "combined", freq = 4), " - ", i))
}

for (i in 2:ncol(d)) {
  if (isSeasonal(d[,i], test = "combined", freq = 4) == TRUE) {
    plot.ts(d[,i], )
  }
  # plot.ts(d[,i], )
}

#====================================================

k<-d[,-1]
k<-ts(k)
rownames(k)<-d[,1]


# AJUSTAR SERIES (APLICAR RETORNOS)

t<-c(
    'LFEMTTTTAUM657S'
    ,'LREMTTTTAUM156S'
    ,'LRHUTTTTAUM156S'
    ,'CSINFT02AUM460S'
    ,'IR3TBB01AUM156N'
    ,'IR3TIB01AUM156N'
    ,'IRSTCI01AUM156N'
    ,'INTGSBAUM193N'
    ,'IRLTLT01AUM156N'
    ,'XTEXVA01AUM657S'
    ,'XTIMVA01AUM657S'
  )

#period return
for (i in 1:ncol(k)) {
  if (colnames(d[i+1]) %in% t) {
    k[,i]<-k[,i]
  }else{
    k[,i]<-periodReturn(k[,i], period='quarterly')
  }
}

t1<-c(
  'LREMTTTTAUM156S',
  'LRHUTTTTAUM156S',
  'CSINFT02AUM460S',
  'IR3TBB01AUM156N',
  'IR3TIB01AUM156N',
  'IRSTCI01AUM156N',
  'INTGSBAUM193N',
  'IRLTLT01AUM156N'
)

# /100
for (i in 1:ncol(k)) {
  if (colnames(d[i+1]) %in% t1) {
    k[,i]<-k[,i]/100
  }
  # }else{
  #   k[,i]<-k[,i]
  #   #k[,i]<-periodReturn(k[,i], period='quarterly')
  # }
}

rm(t,t1)
rm(mfList,x)

#save.image("C:/Users/luiz.eduardo/OneDrive/Documentos/Dissertação/base de dados - tratamento/argentina/base.RData")

# c is a complete database without the financial market and data cutting
# d is a complete database without treatments
# k is database with calculated returns


run{
for (i in 1:ncol(k)) {
  print( isSeasonal(k[,i], test = "combined", freq = 4))
}

# evaluating the plot of the graphic
for (i in 2:ncol(k)) {
  plot.ts(k[,i], )
}}







#pode apagar
#####################################d1<-d[,-1]


# testar estacionaridade das series ---------------------------------------


# testing the typeof
for (i in 1:ncol(k)) { print(typeof(k[,i])) }
#-------------



k1<-k[-1,]
k2<-k1[-1,]
k3<-k2[-1,]

diffAp<-result<-pValores<-matrix(ncol = 4, nrow = ncol(k))

colnames(diffAp)<-colnames(result)<-colnames(pValores)<-c('1r','2r','3r','4r')
pValores3<-pValores2<-pValores1<-pValores0<-list()


##############################
#================================== parei por aqui =============
########

#=============================================================
for (i in 1:ncol(k)) {
  # 1r=======================================================
  pValores0[i]<-list(suppressWarnings(adf.test(k1[,i])))
  pValores[i,1]<-pValores0[[i]][["p.value"]]
 
  if (pValores[i,1] > 0.05){
    # k2[,i]<- diff(k1[,i],lag = 1, differences = 1)
     diffAp[i,2]<-1
  }else{
    result[i,1]<-'Estacionario'
  }
  
  # 2r=======================================================
  pValores1[i]<-list(suppressWarnings(adf.test(k2[,i])))
  pValores[i,2]<-pValores1[[i]][["p.value"]]
  
  if (pValores[i,2] > 0.05){
    ###k3[,i]<- diff(k2[,i],lag = 1, differences = 1)
    ###diffAp[i,3]<-1
  }else{
    result[i,2]<-'Estacionario'
  }
  # 3r=======================================================
   pValores2[i]<-list(suppressWarnings(adf.test(k3[,i])))
   pValores[i,3]<-pValores2[[i]][["p.value"]]
# ##
# ##  if (pValores[i,3] > 0.05){
# ##    #k4[,i]<- diff(k3[,i],lag = 1, differences = 1)
# ##    diffAp[i,4]<-1
# ##  }else{
# ##    result[i,3]<-'Estacionario'
# ##  }
  
}


#tratar por fora
a<-23
k2[,a]<- diff(k1[,a],lag = 1, differences = 1)
diffAp[a,2]<-1



# 
# 
# 
# 
# 
# # preencher p-valores e resultados de estacionaridade
# for (i in 1:length(d1)) {
#     pValores0[i]<-list(suppressWarnings(adf.test(d1[,i])))
#     pValores[i,1]<-pValores0[[i]][["p.value"]]
#   
#     # 1d ----------------------------------------------------------------------
#   
#     if (pValores[i,1] > 0.05){
#       d2[,i]<- diff(d1[,i],lag = 1, differences = 1)
#     }else{
#       d2[,i]<- d1[-1,i]
#       result[i,1]<-'Estacionario'
#     }
#   
#       pValores1[i]<-list(suppressWarnings(adf.test(d2[,i])))
#       pValores[i,2]<-pValores1[[i]][["p.value"]]
# 
#     # 2d ----------------------------------------------------------------------
# 
#     if (pValores[i,2] > 0.05){
#       d3[,i]<- diff(d2[,i],lag = 1, differences = 1)
#     }else{
#       d3[,i]<- d2[-1,i]
#       result[i,2]<-'Estacionario'
#     }
#     
#       pValores2[i]<-list(suppressWarnings(adf.test(d3[,i])))
#       pValores[i,3]<-pValores2[[i]][["p.value"]]
# 
#     
#     # 3d ----------------------------------------------------------------------
#     
#     if (pValores[i,3] > 0.05){
#       d4[,i]<- diff(d3[,i],lag = 1, differences = 1)
#     }else{
#       d4[,i]<- d3[-1,i]
#       result[i,3]<-'Estacionario'
#     }
#     
#     pValores3[i]<-list(suppressWarnings(adf.test(d4[,i])))
#     pValores[i,4]<-pValores3[[i]][["p.value"]]
#     
#     # result 3d
#     if (pValores[i,4] > 0.05){
#       #d4[,i]<- diff(d3[,i],lag = 1, differences = 1)
#     }else{
#       #d4[,i]<- d3[-1,i]
#       result[i,4]<-'Estacionario'
#     }
#   
# }

rm(pValores0,pValores1,pValores2,pValores3)

# selecionando as variaveis -----------------------------------------------
Z<-k2
row.names(diffAp)<-row.names(result)<-row.names(pValores)<-colnames(k)


# testar se todas as series sao estacionarias -----------------------------
# dfpValores<-as.data.frame(pValores)
# filter(dfpValores,dfpValores[,2]>0.05)
# rm(dfpValores)
#d5<-d4[,-(grep('LRHUTTTTBEM156S', colnames(d4)))]

#rm(x,y,xStar,i,I,cv,codigos_selecionados,betas)

run{
  
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

}



for (i in 2:ncol(Z)) {
  print(paste0( isSeasonal(Z[,i], test = "combined", freq = 4), " - ", i))
}

for (i in 2:ncol(Z)) {
  if (isSeasonal(Z[,i], test = "combined", freq = 4) == TRUE) {
    plot.ts(Z[,i], )
  }
}

View(codigos_selecionados)

  write.csv(codigos_selecionados, paste0(rootFolder,'_Base_Selecao_Codigos/',folder,'.csv'))
  write.csv(pValores, paste0(rootFolder,folder,'/csv.pValores.csv'))
  write.csv(result, paste0(rootFolder,folder,'/csv.Result_Estacionaridade.csv'))
  write.csv(diffAp, paste0(rootFolder,folder,'/csv.Diff_Aplicadas.csv'))
  save.image(paste0(rootFolder,folder,'/base.RData'))

