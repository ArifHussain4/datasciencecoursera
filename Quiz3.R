if(!file.exists("/quiz3")){dir.create("./quiz3")}

url1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(url1, destfile = "./quiz3/housing.csv")

housing_data_q3 <- read.csv("./quiz3/housing.csv")

head(housing_data_q3)

library(dplyr)
    # > 10 acrs --> ACR == 3
    #sold more than 10k agrilculte --> AGS == 6

logic <- housing_data_q3$ACR ==3 & housing_data_q3$AGS == 6
which(logic)
      
      housing_data_q3_1 <- filter(housing_data_q3, ACR == 3, AGS == 6)
      housing_data_q3_1

      
library(jpeg)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", destfile = "./quiz3/pic.jpg")
jpgdata = readJPEG("./quiz3/pic.jpg", native = TRUE)
quantile(jpgdata, probs = c(0.3, 0.8))



url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url2, destfile = "./quiz3/GDP.csv", method="curl")
download.file(url3, destfile = "./quiz3/education.csv", method="curl")


gdp_data <- read.csv("./quiz3/GDP.csv")
education_data <- read.csv("./quiz3/education.csv")

gdp_data_df <- as.data.frame(gdp_data)
education_data_df <- as.data.frame(education_data)

names(gdp_data_df) #X
names(education_data_df) #CountryCode

