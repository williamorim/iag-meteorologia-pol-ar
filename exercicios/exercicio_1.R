# Carregando pacotes
library(tidyverse)

# caso o pacote tidyverse não esteja instalado, rodar o código abaixo
# pode demorar um pouco, principalmente no Linux
install.packages(tidyverse) 

# Valores no passo 0

o3 <- 0.15
no2 <- 0.20
no3 <- 0
n2o5 <- 0
hno3 <- 0
h2o <- 22.84

# Tamanho do passo

passo <- 1/600 # minuto -> décimo de segundo

# Constantes de reação transformadas para o tamanho do passo

k1 <- 0.0468 * passo
k2 <- 2510 * passo
k3 <- 3.14e-6 * passo
k4 <- 1.92e-6 * passo

# 1 milhão de passos

n_passos <- 1000000

# Iterações 
# (começa do dois porque vetores no R começam com o índice 1, que já tem o passo 0)
  
for(i in 2:n_passos) {
  
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

# Transformando em data.frame
df <- tibble(o3, no2, no3, n2o5, hno3, h2o)

# Fazendo gráficos

# Todas as curvas no mesmo gráfico (tirando a água por causa da escala)
df %>% 
  gather(composto, valor, -h2o) %>%
  group_by(composto) %>% 
  mutate(passo = 1:n()) %>%
ggplot() +
  geom_line(aes(x = passo, y = valor, color = composto))

# Cada curva em um gráfico
df %>% 
  gather(composto, valor, everything()) %>%
  group_by(composto) %>% 
  mutate(passo = 1:n()) %>%
  ggplot() +
  geom_line(aes(x = passo, y = valor)) +
  facet_wrap(~composto, scales = "free_y")
