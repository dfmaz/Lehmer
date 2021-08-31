datos = c(0.143, 0.182, 0.256, 0.26, 0.27,
          0.437, 0.509, 0.611, 0.712, 1.04,
          1.09, 1.15, 1.46, 1.88, 2.08)
n = length(datos)
alpha = 0.05 
B = 10^5

mediana = median(datos)

# Remuestreo
est = function(datos)
  sqrt(n)*(median(datos)-mediana)
estB = replicate(B, est(sample(datos, n, replace = TRUE)))

# IC para la mediana
pto_crit = quantile(estB, c(alpha/2, 1 - alpha/2))
ext_inf = mediana - pto_crit[2]/sqrt(n)
ext_sup = mediana - pto_crit[1]/sqrt(n)