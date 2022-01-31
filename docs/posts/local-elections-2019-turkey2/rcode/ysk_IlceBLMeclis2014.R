
require(jsonlite)

##############################################################################################################################

#2014 Ilce Belediye Meclis Uyeligi

secimId        = 36039  # 2014 mahalli idare genel secim
secimturu      = 3      # belediye meclis uyeligi

buyuksehir.kod <- fromJSON(paste0("https://sonuc.ysk.gov.tr/api/getSecimSandikSonucList?secimId=",
                                  secimId,
                                  "&secimTuru=",
                                  secimturu,
                                  "&ilId=&ilceId=&beldeId=&birimId=&muhtarlikId=&cezaeviId=&sandikTuru=&sandikNoIlk=&sandikNoSon=&ulkeId=&disTemsilcilikId=&gumrukId=&yurtIciDisi=1&sandikRumuzIlk=&sandikRumuzSon=&secimCevresiId=&sandikId="))

buyuksehir.kod                <- buyuksehir.kod[,c("il_ID","il_ADI")]
buyuksehir.kod$secimCevresiID <- rep(0,nrow(buyuksehir.kod))

sandik <- vector("list",nrow(buyuksehir.kod))

for(i in 1:nrow(buyuksehir.kod)){
  
  extract.ilce.id <- fromJSON(paste0("https://sonuc.ysk.gov.tr/api/getIlceList?secimId=",secimId,
                                     "&secimTuru=",secimturu,
                                     "&ilId=",buyuksehir.kod[i,1],
                                     "&secimCevresiId=",buyuksehir.kod[i,3],
                                     "&sandikTuru=-1&yurtIciDisi=1"))

   ilce.ids <- extract.ilce.id[,c("ilce_ID","ilce_ADI","secim_CEVRESI_ID")]
   
   root.link <- function(il.id,ilce.id,secimturu,secimCevresiId) {
     return(
       paste0("https://sonuc.ysk.gov.tr/api/getSecimSandikSonucList?secimId=",secimId,
              "&secimTuru=",secimturu,
              "&ilId=",il.id,
              "&ilceId=",ilce.id,
              "&beldeId=&birimId=&muhtarlikId=&cezaeviId=&sandikTuru=&sandikNoIlk=&sandikNoSon=&ulkeId=&disTemsilcilikId=&gumrukId=&yurtIciDisi=1&sandikRumuzIlk=&sandikRumuzSon=&",
              "secimCevresiId=",secimCevresiId,"&sandikId=")
       )
    }
   
   j= 1
   api.link = root.link(il.id     = buyuksehir.kod[i,1],
                        ilce.id   = ilce.ids[j,1],
                        secimturu = secimturu,
                        secimCevresiId = ilce.ids[j,3])
   
   extract.sandik <- fromJSON(api.link)
   
   sonuc <- extract.sandik[,c("il_ID","il_ADI","ilce_ID","ilce_ADI","muhtarlik_ID",
                              "muhtarlik_ADI","sandik_NO",
                              "secmen_SAYISI","oy_KULLANAN_SECMEN_SAYISI",
                              "gecerli_OY_TOPLAMI","gecersiz_OY_TOPLAMI",
                              "itirazsiz_GECERLI_OY_SAYISI","itirazli_GECERLI_OY_SAYISI",
                              "parti1_ALDIGI_OY","parti2_ALDIGI_OY","parti3_ALDIGI_OY","parti4_ALDIGI_OY",
                              "parti5_ALDIGI_OY","parti6_ALDIGI_OY","parti7_ALDIGI_OY","parti8_ALDIGI_OY",
                              "parti9_ALDIGI_OY", "parti10_ALDIGI_OY","parti11_ALDIGI_OY","parti12_ALDIGI_OY",
                              "parti13_ALDIGI_OY","parti14_ALDIGI_OY","parti15_ALDIGI_OY","parti16_ALDIGI_OY",                   
                              "parti17_ALDIGI_OY","parti18_ALDIGI_OY","parti19_ALDIGI_OY","parti20_ALDIGI_OY",                  
                              "parti21_ALDIGI_OY","parti22_ALDIGI_OY","parti23_ALDIGI_OY","parti24_ALDIGI_OY",                   
                              "parti25_ALDIGI_OY","parti26_ALDIGI_OY","parti27_ALDIGI_OY","parti28_ALDIGI_OY",                   
                              "parti29_ALDIGI_OY","parti30_ALDIGI_OY","bagimsiz_TOPLAM_OY",
                              "ittifak1_ALDIGI_OY","ittifak2_ALDIGI_OY","ittifak3_ALDIGI_OY","ittifak4_ALDIGI_OY","ittifak5_ALDIGI_OY",
                              "son_ISLEM_TARIHI")]
   
   for(j in 2:nrow(extract.ilce.id)){
     
     api.link = root.link(il.id     = buyuksehir.kod[i,1],
                        ilce.id   = ilce.ids[j,1],
                        secimturu = secimturu,
                        secimCevresiId = ilce.ids[j,3])
     
     extract.sandik <- fromJSON(api.link)
     
     temp <- extract.sandik[,c("il_ID","il_ADI","ilce_ID","ilce_ADI","muhtarlik_ID",
                              "muhtarlik_ADI","sandik_NO",
                              "secmen_SAYISI","oy_KULLANAN_SECMEN_SAYISI",
                              "gecerli_OY_TOPLAMI","gecersiz_OY_TOPLAMI",
                              "itirazsiz_GECERLI_OY_SAYISI","itirazli_GECERLI_OY_SAYISI",
                              "parti1_ALDIGI_OY","parti2_ALDIGI_OY","parti3_ALDIGI_OY","parti4_ALDIGI_OY",
                              "parti5_ALDIGI_OY","parti6_ALDIGI_OY","parti7_ALDIGI_OY","parti8_ALDIGI_OY",
                              "parti9_ALDIGI_OY", "parti10_ALDIGI_OY","parti11_ALDIGI_OY","parti12_ALDIGI_OY",
                              "parti13_ALDIGI_OY","parti14_ALDIGI_OY","parti15_ALDIGI_OY","parti16_ALDIGI_OY",                   
                              "parti17_ALDIGI_OY","parti18_ALDIGI_OY","parti19_ALDIGI_OY","parti20_ALDIGI_OY",                  
                              "parti21_ALDIGI_OY","parti22_ALDIGI_OY","parti23_ALDIGI_OY","parti24_ALDIGI_OY",                   
                              "parti25_ALDIGI_OY","parti26_ALDIGI_OY","parti27_ALDIGI_OY","parti28_ALDIGI_OY",                   
                              "parti29_ALDIGI_OY","parti30_ALDIGI_OY","bagimsiz_TOPLAM_OY",
                              "ittifak1_ALDIGI_OY","ittifak2_ALDIGI_OY","ittifak3_ALDIGI_OY","ittifak4_ALDIGI_OY","ittifak5_ALDIGI_OY",
                              "son_ISLEM_TARIHI")]
        
        sonuc <- rbind(sonuc,temp)
        
        cat("sehir = ",i,"    ilce=",j,"\n")
   }
   
  sandik[[i]] <- sonuc 
  
}


sandik.sonuc <- sandik[[1]]
for(u in 2:length(sandik)){
  sandik.sonuc <- rbind(sandik.sonuc,sandik[[u]])
}


parti.kod <- jsonlite::fromJSON("https://sonuc.ysk.gov.tr/api/getSandikSecimSonucBaslikList?secimId=36039&secimCevresiId=0&ilId=0&bagimsiz=0&secimTuru=3&yurtIciDisi=1")
loc = which(colnames(sandik.sonuc)%in%parti.kod$column_NAME)
colnames(sandik.sonuc)[loc] <- parti.kod$ad

sandik.sonuc <- sandik.sonuc[,-grep("parti",colnames(sandik.sonuc))]
sandik.sonuc <- sandik.sonuc[,-grep("ittifak",colnames(sandik.sonuc))]

colnames(sandik.sonuc)[8:37] <- paste0(colnames(sandik.sonuc)[8:37],"_IlceBLMeclis2014")

write.csv(sandik.sonuc,"IlceBLMeclis2014.csv",row.names=FALSE)

 
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                  