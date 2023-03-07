
setwd("~/Desktop/Winter 2023/GSB 530/Group Project")
df <- read_excel("Big_Data_Files.xlsx", sheet = "COVID_Testing")


#Clean
df$Age_60_And_Above <- ifelse(df$Age_60_And_Above == "Yes", 1, 0)
df$Male <- ifelse(df$Sex == "male", 1, 0)
df$Positive <- ifelse(df$Result == "positive", 1, 0)
