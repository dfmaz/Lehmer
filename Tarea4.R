## Optimización: recocido simulado para el cálculo del mínimo de una 
##               función de una variable en un intervalo [izq, dcho]

# Recocido Simulado
RS = function(n, K, A, t, alpha, f, izq, dcho) 
{
  xi = runif(1, izq, dcho) 
  fi = fmin = f(xi)
  a = 0
  k = 0 
  lsol = fi 
  
  for(i in 1:n)
  {
    while ((a<=A) && (k<=K)) 
    {
      y = xi + runif(1, izq/4, dcho/4) 
      fy = f(y)
      if (fy < fi)
      {
        xi = y
        fi = fy
        a = a+1
        if (fi < fmin)
        {
          xmin = xi
          fmin = fi
        }
      } else if (runif(1) < exp((fi-fy)/t))
      {
        xi = y
        fi = fy
        a = a+1
      }
      k = k+1
    }
    t = t*alpha 
    a = 0
    k = 0
    lsol[i] = fi
  }
  plot(lsol, xlab="i", ylab="f_i", type="l")
  list(x_min=xmin, f_xmin=fmin) # x_min y f(x_min)
}

# Función objetivo
f = function(x)
  3*x^4-16*x^3+18*x^2
plot(f,-1,4)

# Parámetros
n = 700
K = 1
A = 1
t = 20
alpha = 0.99
izq = -1
dcho = 4

# Llamada a la función
RS(n, K, A, t, alpha, f, izq, dcho)