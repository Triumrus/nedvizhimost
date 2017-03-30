library("data.table")
library("stringr")
getwd()
setwd("C:/Users/user/Desktop/аренда")
table<- fread("ciaaaaan.csv")
table$rooms<- as.numeric(str_sub(table$Заголовок,1,1))
table$Заголовок <- NULL
info <- table[,.(Адрес,Телефон)]
table$Адрес <- NULL
table$Телефон <- NULL
table$`Дата публикации` <- NULL
table$Категория <- NULL
table$Описание <- NULL
info$`Имя продавца`<- table$`Имя продавца`
table$`Имя продавца` <- NULL

 table[,`:=`(kitchen = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[1])
        ,mebel = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[2])
        ,holodll = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[3])
       ,animals = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[4])
       ,children = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[5])
       ,tv = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[6])
       ,washer = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[7])
       ,balcon = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[8])
       ,lodshia = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[9])
       ,telefon = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[10])
       ,internet = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[11])
       ,conder = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[12])
       ,posuda = str_detect(str_split(table$Параметры,","),unlist(str_split(table$Параметры[which.max(str_count(table$Параметры))],","))[13])
 )]
 table$Параметры <- NULL
 info$URL<- table$URL
 table$URL <- NULL
 table$car <-  str_detect(table$До_метро,"машин")
 table$go<- as.numeric(unlist(lapply(table$До_метро,function(x){unlist(str_split(x,"[:space:]"))[1]})))
 table$До_метро <- NULL
 table$plo_kitchen<- as.numeric(str_extract(table$кухня,"[0-9]+"))
 table$кухня <- NULL
 table$plo_shil<- as.numeric(str_extract(table$жилая,"[0-9]+"))
 table$жилая <- NULL
 table$ремонт<- as.factor(table$ремонт)
 table$Лифт <- NULL
 table$комнат <- NULL
 table$polotolki <- as.numeric( gsub(pattern = ",",replacement = ".",x = str_extract(table$потолки,"[0-9],[0-9]+")) )
 table$потолки <- NULL
 table$etashei<- as.numeric(str_extract(str_extract(table$этажей,"/.[0-9]+"),"[0-9]+"))
 table$этажей <- NULL
 table$балкон <- NULL
 table$`Тип дома` <- NULL
   table[,`:=`(
     LOW = (table$ремонт==levels(table$ремонт)[1]),
     HIGH = (table$ремонт==levels(table$ремонт)[2]),
     MEDIUM = (table$ремонт==levels(table$ремонт)[3]),
     UPPER = (table$ремонт==levels(table$ремонт)[4]),
     NOPE = (table$ремонт==levels(table$ремонт)[5])
   )]
   table$ремонт <- NULL
   metro<- lapply(table$метро,function(x)  {x== levels(as.factor(table$метро))})
     metro <- matrix(unlist(metro),ncol = 152,byrow = T)
     metro<- data.table(metro)
     names(metro) <- as.character(levels(as.factor(table$метро)))
     table<- cbind(table,metro)
     table$метро <- NULL
     str(table)
      rm(metro)
     table<- sapply(table,as.numeric)
     table <- data.table(table)
     
