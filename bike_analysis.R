#czy dzien tygodnia wyplywal na trip duration ?


#łADOWANIE BIBLIOTEK
library("dplyr")
library(ggplot2)
library(lattice)
library(writexl)

#WCZYTYWANIE DANYCH
df_01 <- read.csv("./datasets/JC-201701-citibike-tripdata.csv")
df_04 <- read.csv("./datasets/JC-201704-citibike-tripdata.csv")
df_07 <- read.csv("./datasets/JC-201707-citibike-tripdata.csv")
df_10 <- read.csv("./datasets/JC-201710-citibike-tripdata.csv")
df_02 <- read.csv("./datasets/JC-201702-citibike-tripdata.csv")
df_03 <- read.csv("./datasets/JC-201703-citibike-tripdata.csv")
df_05 <- read.csv("./datasets/JC-201705-citibike-tripdata.csv")
df_06 <- read.csv("./datasets/JC-201706-citibike-tripdata.csv")
df_08 <- read.csv("./datasets/JC-201708 citibike-tripdata.csv")
df_09 <- read.csv("./datasets/JC-201709-citibike-tripdata.csv")
df_11 <- read.csv("./datasets/JC-201711-citibike-tripdata.csv")
df_12 <- read.csv("./datasets/JC-201712-citibike-tripdata.csv")
names(df_02)[c(1,2)] <- c("tripduration", "starttime")
names(df_03)[c(1,2)] <- c("tripduration", "starttime")
names(df_01)[c(1,2)] <- c("tripduration", "starttime")

week <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")

#AD.1* STYCZEN -> DURATION TIME
tmp <- df_01[,c("tripduration", "starttime")]
tmp$DayWeek <- as.Date(a$starttime)
tmp$DayWeek <- strftime(a$DayWeek, "%A")
tmp <- tmp[, c("tripduration", "DayWeek")]
tmp <- tmp[order(a$DayWeek),]

#ZAPISYWANIE DANYCH DO EXCELA
write_xlsx(tmp,"C:\\Users\\Uzytkownik\\Desktop\\ramka2a.xlsx")

#WYKRES 1
ggplot(a, aes(x=DayWeek, y=tripduration)) +  scale_x_discrete(limits = week) +
  geom_boxplot(fill='darkorange3') + coord_cartesian(ylim = c(100, 700))

ggplot(a, aes(x=DayWeek, y=tripduration)) +  scale_x_discrete(limits = week)+
  geom_boxplot(fill='coral3') + coord_cartesian(ylim = c(0, 3000))

# AD.1 DZIEN TYGODNIA VS DURATION TIME
durationVSday <- function(frame){
  frame <- frame[, c("tripduration","starttime")]
  frame$DayWeek <- as.Date(frame$starttime)
  frame$DayWeek <- strftime(frame$DayWeek, "%A")
  result <- frame %>% group_by(DayWeek) %>% summarise(Average = mean(tripduration))
  result <- result[order(-result$Average),]
  result$AverageMinutes <- result$Average/60
  return (result)
}

res1 <- durationVSday(df_01)
res2 <- durationVSday(df_04)
res3 <- durationVSday(df_07)
res4 <- durationVSday(df_10)

result <- merge(res1, res2, by.x = "DayWeek", by.y = "DayWeek")
result <- merge(result, res3, by.x = "DayWeek", by.y = "DayWeek")
names(result)[c(2:7)] <- c("ave1", "avemin1", "ave2", "avemin2", "ave3", "avemin3")
result <- merge(result, res4, by.x = "DayWeek", by.y = "DayWeek")
names(result)[c(8:9)] <- c("ave4", "avemin4")
names(result)
result$average_min <-  (result$avemin1 + result$avemin2 + result$avemin3 + result$avemin4) / 4
result <- result[, c("DayWeek", "average_min")]
result <- result[order(-result$average_min),]

result <- result %>% arrange(sapply(DayWeek, function(y) which(y == week)))

#WYKRES 2
ggplot(result) + scale_x_discrete(limits = week) +
  geom_bar(aes(x = DayWeek, y = average_min), 
           stat = 'identity', fill = 'darkseagreen4') +
  
  ggtitle("Wykres słupkowy dzien tygodnia vs average trip duration [min]") +
  xlab("Dzien Tygodnia") +
  ylab("Average trip duration [min]") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


write_xlsx(result,"C:\\Users\\Uzytkownik\\Desktop\\ramka1.xlsx")

# AD.2 miesiac vs czas podrozy

duration_mean <- function(df){
  mean(df$tripduration,  na.rm = TRUE)
}

dm1 <- duration_mean(df_01)
dm2 <- duration_mean(df_02)
dm3 <- duration_mean(df_03)
dm4 <- duration_mean(df_04)
dm5 <- duration_mean(df_05)
dm6 <- duration_mean(df_06)
dm7 <- duration_mean(df_07)
dm8 <- duration_mean(df_08)
dm9 <- duration_mean(df_09)
dm10 <- duration_mean(df_10)
dm11 <- duration_mean(df_11)
dm12 <- duration_mean(df_12)

res <- c(dm1,dm2,dm3,dm4,dm5,dm6,dm7,dm8,dm9,dm10,dm11,dm12)
res <- data.frame(res)
names(res) <- "mean_duration_time"

miesiace <- c("st", "lut", "ma", "kw", "maj", "cze", "lip", "sierp", "wrz", "paz", "lis", "gr")

#WYKRES 3
res %>% ggplot(aes(x = 1:12, y = mean_duration_time)) + geom_line() + geom_point()
write_xlsx(res,"C:\\Users\\Uzytkownik\\Desktop\\ramka3.xlsx")

