library(tidyverse)
library(readxl)

df <- read_excel("aerossol/exemplo_dt_numero_massa.xlsx")
View(df)

df <- df %>% 
  mutate(ln_diametro = log(diametro))

ln_d <- sum(df$amostra1*df$ln_diametro)/sum(df$amostra1)

ln_sigma <- sqrt(sum(df$amostra1*(df$ln_diametro - ln_d)^2)/(sum(df$amostra1)-1))

exp(ln_d)
exp(ln_sigma)


