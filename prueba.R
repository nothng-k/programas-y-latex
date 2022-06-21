
graficador<-function(a,b,l,f){
  x <- seq(a, b, length.out = l)
  y <- x
  z <- outer(x, y, f)

  op <- par(bg = "white")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
        ltheta = 120, shade = 0.75, ticktype = "detailed",zlab = f
  ) -> res
}


price_function<-function(p_1,p_2,k=1){
  return(exp((p_2-p_1)*k))
}

price_function<-function(p_1,p_2,k=400){
  return(1-(
    exp(p_1/k)/
     (exp(p_1/k)+exp(p_2/k))
  )
      
     )
}



graficador(3,15,60,"price_function")
dicotomic_function<-function(c_1,c_2,n_1,n_2,N){
  return(
    (
      n_1+c_1*(N-n_1-n_2)
      )/(
        (n_1+n_2)*((c_1+c_2)==0)+N*((c_1+c_2)>0)
        )
    )
}

dicotomic_function<-function(c_1,c_2,n_1,n_2,N){
  return(
    (
      n_1+c_1*(N-n_1-n_2)
    )/(
      (n_1+n_2)*(1 -c_1) * ( 1 - c_2 )+N*((c_1+c_2)>0)
    )
  )
}

dicotomic_function1<-function(c_1,c_2,n_1,n_2,N){
  return(
      c_1 * c_2 * 0.6 +
      c_1 * (1 - c_2) * 0.75 +
      (1 -c_1) * ( 1 - c_2 ) * 0.65+
      c_1 * (1 - c_2) * 0.8
  )
}
dicotomic_function2<-function(c_1,c_2,n_1,n_2,N){
  return(
    c_1 * c_2 * 0.6 +
    c_1 * (1 - c_2) * 0.75 +
    (1 -c_1) * ( 1 - c_2 ) * 0.65+
    c_1 * (1 - c_2) * 0.8
  )
}

price_control<-function(p,k=400,m=1000){
  return(0+
    (p<m)*exp(-p/k)
  )
}

price_control<-function(p,k=400,m=1000){
  return(0+
           (p<m)*exp(-p/k)*((p/-m)^2)
  )
}
    


m_ps<-function(p_1,p_2,c_1=0,c_2=0,n_1=100000000,n_2=50000000,N=500000000,k=0.03){
  return(
    2000*dicotomic_function(c_1,c_2,n_1,n_2,N)*price_function(p_1,p_2,k)*price_control(p_1)
  )
}
graficador(0,2,60,"m_ps")

V_ps<-function(p_1,p_2,c_1=0,c_2=0,n_1=100000000,n_2=50000000,N=500000000,k=400){
  m_ps(p_1,p_2,c_1=c_1,c_2=c_2,k=k,n_1 = n_1,n_2=n_2,N=N)*(p_1-300)
}
graficador(300,1000,60,"V_ps")
k=400
p=200
numeros=numeric(k-p)
for( i in p:k){
  numero=c(0,0)
  for( j in p:k){
    if (numero[1]<V_ps(j,i)){
      numero=c(V_ps(j,i),j)
    }
  }
  numeros[i]=numero
}
numeros
plot(numeros)
abline(1,1, col = 2)
