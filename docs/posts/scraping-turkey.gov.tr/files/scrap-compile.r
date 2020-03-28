#################################################33

# Install Docker
# https://hub.docker.com/editions/community/docker-ce-desktop-windows

####################################################################################

library(RSelenium)
library(rvest)
library(xml2)


system('docker run -d -p 4445:4444 selenium/standalone-chrome')
####################################################################################

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()


website= 'https://www.turkiye.gov.tr'
cities = c('bursa','denizli','diyarbakir','istanbul','kahramanmaras',
           'kocaeli','konya','malatya','sakarya','tekirdag','erzurum')

dates <- seq.Date(Sys.Date() - 3737,Sys.Date()-1, by='days')
dates <- dates[1:(length(dates)-1)]


for(i in 1:length(cities)){
  
  url  = paste0(website,"/",cities[i],'-buyuksehir-belediyesi-vefat-sorgulama')
  
  tables <- vector("list",length(dates))
  
  for(j in 1:length(dates)){
    
    tables[[j]]=list()
    rep = 0
    
    while(length(tables[[j]])==0 & rep<20){
      remDr$navigate(url)
      element<- remDr$findElement(using = 'css selector', "#tarih")
      button <- remDr$findElement(using = 'css selector', ".submitButton") 
      element$clearElement()
      
      element$sendKeysToElement(list(dates[j]))
      button$clickElement()
      
      #remDr$screenshot(display=TRUE)
      
      html <- xml2::read_html(remDr$getPageSource()[[1]])
      tables[[j]] <- html_table(html_nodes(html, "table"))
      
      rep = rep+1 
    }
    
    print(c(j,dates[j]))
    print(tables[[j]])
  }
  
  
  for(k in 1:length(tables)){
    
    if(length(tables[[k]])!=0){
      colnames(tables[[k]][[1]])[1:6] <- c("AdSoyad","BabaAdi","DogumTar","Yasi","Sebeb","Islem")
      tables[[k]][[1]]$OlumTar <- dates[k]
      tables[[k]][[1]]$Sehir   <- cities[i]
    }
  }
  
  tab <- tables[[1]][[1]]
  
  for(k in 2:length(tables)){
    
    if(length(tables[[k]])!=0){
      tab <- rbind(tab,tables[[k]][[1]])
    }
  }
  
  write.csv(tab,paste0(cities[i],".csv"),row.names=FALSE)
  
} 

#################################################################

a1 = read.csv("bursa.csv",header=TRUE)
a2 = read.csv("denizli.csv",header=TRUE)
a3 = read.csv("diyarbakir.csv",header=TRUE)
a4 = read.csv("istanbul.csv",header=TRUE)
a5 = read.csv("kahramanmaras.csv",header=TRUE)
a6 = read.csv("kocaeli.csv",header=TRUE)
a7 = read.csv("konya.csv",header=TRUE)
a8 = read.csv("malatya.csv",header=TRUE)
a9 = read.csv("sakarya.csv",header=TRUE)
a10 = read.csv("tekirdag.csv",header=TRUE)
a11 = read.csv("erzurum.csv",header=TRUE)

data <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)

names(table(data$Yasi))

data[which(data$Yasi=="-"),]$Yasi=NA                       
data[which(data$Yasi=="-1"),]$Yasi=NA
data[which(data$Yasi=="-2"),]$Yasi=NA

data$Yasi <- as.numeric(as.character(data$Yasi))
data[which(data$Yasi>145),]$Yasi=NA


data$OlumTar <- as.character(data$OlumTar)

data$OlumTar <- as.Date(data$OlumTar,format="%Y-%m-%d")
data$OlumTar <- format(data$OlumTar,format="%d/%m/%Y")


data$OlumTar <- as.character(data$OlumTar)


data$Sehir <- as.character(data$Sehir)


rm(list=setdiff(ls(), "data")) 

save.image("data.Rdata")




