# constantes
k_sn <- 1
k_ns <- 1
k_cooh <- 0.0000047304 

#co_s  massa de CO hemisfério sul
#co_n  massa de CO hemisfério norte
oh <- 870000 # massa de OH

# vegetação, oceano, queima de biomassa, antropogênicos
e_s <- 50 + 34.6 + 700 + 0 # emissão (solo) no hemisfério sul
e_n <- 100 + 17.3 + 0 + 650 # emissão (solo) no hemisfério norte

d_s = 63.333 # deposição no hemisfério sul
d_n = 126.666 # deposição no hemisfério norte


# oxidação do CH4 (metano), isopreno, hidrocarboneto não-metano (HCNM), acetona
q_s <- 400 + 135 + 0 + 0 # emissão (química) no hemisfério sul
q_n <- 400 + 135 + 140 + 20 # emissão (quúmica) no hemisfério norte

f <- function(x) {
  
  res1 <- k_sn * x[1] - k_ns * x[2] + e_n - d_n + q_n - k_cooh * x[2] * oh
  
  res2 <- k_ns * x[2] - k_sn * x[1] + e_s - d_s + q_s - k_cooh * x[1] * oh
  
  return(c(res1, res2))
  
}

raizes <- multiroot(f, start = c(1, 1))

raizes$root

# Razão de mistura

# CO HS
((raizes$root[1]*(10^12)/28)/18e19)*10^9

# CO HN
((raizes$root[2]*(10^12)/28)/18e19)*10^9
