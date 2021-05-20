#-----------------------------------------------------
# Read all OZ metadata
folder.list='C:\\BEN\\DATA\\Q_RHN4\\metadata\\'
folder.QJ='C:\\BEN\\DATA\\Q_RHN4\\d_common_format_flow_data\\'
allstations=read.table(paste0(folder.list,'StationList.txt'),header=T)
country=substr(allstations$filename,1,2)

#-----------------------------------------------------
# select subsample
pmvmax=15
#mask=(country=='AU') # ALL OZ
mask=(country=='AU') & allstations$lat >= -35 & allstations$lat <= -25 & allstations$lon >= 147 # EAST OZ
#mask=(country=='AU') & allstations$lat <= -35 & allstations$lon >= 110 # SOUTH OZ

stations=allstations[mask,]
n=NROW(stations)
sname=vector("character",n)
# set season
season=c(9,10,11)
# read QJ for selected stations
DF=data.frame(site=integer(),year=integer(),occ=integer(),amax=numeric(),amean=numeric())
for (i in 1:n){
  message(paste(i,'/',n))
  sname[i] <- strsplit(as.character(stations$filename)[i],'_')[[1]][3]
  QJ=read.table(paste0(folder.QJ,stations$filename[i]),header=T,sep=',')
  # define hydro-year
  Hyear=QJ$Year
  Hyear[QJ$Month<season[1]]=Hyear[QJ$Month<season[1]]-1
  for(m in 1:12){
    if(all(m!=season)){Hyear[QJ$Month==m]=NA}
  }
  years=min(Hyear,na.rm=T):max(Hyear,na.rm=T)
  amax=0*years
  amean=0*years
  nday=amax
  ndata=amax
  for(j in 1:length(years)){
    mask=(Hyear==years[j])
    nday[j]=sum(mask,na.rm=T)
    ndata[j]=sum(mask & !is.na(QJ$Flow),na.rm=T)
    amax[j]=max(QJ$Flow[mask],na.rm=T)
    amean[j]=mean(QJ$Flow[mask],na.rm=T)
    if(amax[j]==-Inf){amax[j]<-NA}
    if(is.nan(amean[j])){amean[j]<-NA}
  }
  pmv=100*(max(nday)-ndata)/max(nday)
  q=quantile(amax[pmv<=pmvmax],0.8)
  occ=amax>q
  occ[pmv>pmvmax]<-NA
  occ=as.integer(occ)
  amax[pmv>pmvmax]<-NA
  amean[pmv>pmvmax]<-NA
  DF=rbind(DF,
           data.frame(site=rep(i,length(years)),year=years,occ=occ,amax=amax,amean=amean))
}

vars=c('amean','amax','occ')
allWs=vector(mode = "list", length = length(vars))
for(k in 1:length(vars)){
  var=vars[k]
  y1=min(DF$year)
  yn=max(DF$year)
  years=y1:yn
  Y=matrix(NA,length(years),n)
  for(i in 1:n){
    mask=(DF$site==i)
    yo=DF$year[mask]
    Y[(yo[1]-y1+1):(yo[1]-y1+length(yo)),i]=DF[mask,var]
  }
  # If needed, remove first and last year if entirely missing
  del=c()
  if(all(is.na(Y[1,]))){
    years=years[-1]
    Y=Y[-1,]
  }
  if(all(is.na(Y[NROW(Y),]))){
    years=years[-NROW(Y)]
    Y=Y[-NROW(Y),]
  }
  W <- as.data.frame(Y)
  names(W) <- paste0('station',1:n)
  W <- cbind(data.frame(year=years),W)
  allWs[[k]] <- W
  #write.table(signif(W),file=paste0('OZ_',var,'.txt'),sep=';',row.names=F,col.names=T,quote=F)
}

springFlow <- allWs[[1]];save(springFlow,file='../data/springFlow.RData')
springMax <- allWs[[2]];save(springMax,file='../data/springMax.RData')
springFlood <- allWs[[3]];save(springFlood,file='../data/springFlood.RData')

OZstations <- data.frame(ID=paste0('station',1:n),OzID=sname,lon=stations$lon,lat=stations$lat,area=stations$area)
save(OZstations,file='../data/OZstations.RData')

elNino <- read.table('nino3-4.csv',sep=';',header=T)
elNino$nino <- (elNino$nino-mean(elNino$nino))/sd(elNino$nino)
save(elNino,file='../data/elNino.RData')

