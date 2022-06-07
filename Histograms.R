raw <- read.csv("/Users/arif.hussain/Documents/GitHub/datasciencecoursera/Swyft Work/WBR 2022 - Cohort Raw.csv")

df_raw <- as.data.frame(raw)

library(dplyr)

tapply(df_raw$GMV, df_raw$First_Shipment_Month, median)

long <- df_raw[c("First_Shipment_Month", "GMV")]

g <- ggplot(long, aes(x = GMV, fill = First_Shipment_Month))
g <- g + geom_histogram(colour = "black", binwidth = 1)
g <- g+ facet_grid(. ~ First_Shipment_Month)
g

hist(df_raw[df_raw$First_Shipment_Month == "2021-04-01",]$GMV, col = "blue", breaks = 20)
hist(df_raw[df_raw$First_Shipment_Month == "2021-05-01",]$GMV, col = "blue", breaks = 20)

tapply(df_raw$GMV, df_raw$First_Shipment_Month, summary)





