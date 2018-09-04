o3 <- 0.15
no2 <- 0.20
no3 <- 0
n2o5 <- 0
hno3 <- 0
h2o <- 22.84

passo <- 600

k1 <- 0.0468 / passo
k2 <- 2510 / passo
k3 <- 3.14e-6 / passo
k4 <- 1.92e-6 / passo
  
for(i in 2:1000000) {
  
  o3[i] <- o3[i - 1] - k1 * o3[i-1] * no2[i-1]
  
  no2[i] <- no2[i - 1] - k1 * o3[i-1] * no2[i-1] - 
    k2 * no2[i - 1] * no3[i - 1] +
    k3 * n2o5[i - 1]
  
  no3[i] <- no3[i - 1] + k1 * o3[i-1] * no2[i-1] - 
    k2 * no2[i - 1] * no3[i - 1] +
    k3 * n2o5[i - 1]
  
  n2o5[i] <- n2o5[i - 1] + k2 * no2[i - 1] * no3[i - 1] - 
    k3 * n2o5[i - 1] +
    k4 * n2o5[i - 1] * h2o[i - 1]
  
  hno3[i] <- hno3[i - 1] + k4 * n2o5[i - 1] * h2o[i - 1]
  
  h2o[i] <- h2o[i - 1] - k4 * n2o5[i - 1] * h2o[i - 1]
  
}

library(tidyverse)

df <- tibble(o3, no2, no3, n2o5, hno3, h2o)



df %>% 
  tidyr::gather(composto, valor, -h2o) %>%
  group_by(composto) %>% 
  dplyr::mutate(passo = 1:n()) %>%
  #filter(passo < 10000) %>% 
ggplot() +
  geom_line(aes(x = passo, y = valor, color = composto))

df %>% 
  tidyr::gather(composto, valor, everything()) %>%
  group_by(composto) %>% 
  dplyr::mutate(passo = 1:n()) %>%
  ggplot() +
  geom_line(aes(x = passo, y = valor)) +
  facet_wrap(~composto, scales = "free_y")

ggplot(df) +
  geom_line(aes(x = 1:1000000, y = no3))
