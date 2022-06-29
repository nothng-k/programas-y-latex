

"""
FUNCION PARA GRAFICAR
"""


graficador<-function(a,b,l,f){
  x <- seq(a, b, length.out = l)
  y <- x
  z <- outer(x, y, f)
  z[z<0]=0
  op <- par(bg = "white")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
        ltheta = 120, shade = 0.75, ticktype = "detailed",zlab = f
  ) -> res
}

"""
FUNCION DE PRECIO
"""
price_function<-function(p_1, p_2, k=200){
  return(1 - (
    exp(p_1 / k ) /
      (exp(p_1 / k ) + exp( p_2 / k ))
    )
  )
}



graficador(3,1000,60,"price_function")

"""
FUNCION DE DECISON
"""

dicotomic_function_ps<-function(c_ps, c_xb){
  return(
    c_ps * c_xb * 0.6 +
      c_ps * (1 - c_xb) * 0.75 +
      (1 -c_ps) * ( 1 - c_xb ) * 0.65+
      c_ps * (1 - c_xb) * 0.8
  )
}
dicotomic_function_xb<-function(c_xb, c_ps){
  return(
    c_ps * c_xb * 0.4 +
      c_ps * (1 - c_xb) * 0.65 +
      (1 -c_ps) * ( 1 - c_xb ) * 0.75+
      c_ps * (1 - c_xb) * 0.8
  )
}
"""
cONTROL DE PRECIO
"""

price_control<-function(p, m=1000){
  return(0+
           ( p < m )*
           ( 
             (
             ( p - m ) / m ) ^ 2 
             ) 
  )
}

"""
FUNCIONES DE VENTAS
"""

ventas_ps<-function(p_ps, p_xb, c_ps=0, c_xb=0, k=400){
  return(
    200000000 * 
      dicotomic_function_ps(c_ps, c_xb ) * 
      price_function(p_ps, p_xb, k) *
      price_control(p_ps)
  )
}

ventas_xb<-function(p_xb, p_ps, c_ps=0, c_xb=0, k=400){
  return(
    200000000*
      dicotomic_function_xb(c_ps, c_xb)*
      price_function(p_xb, p_ps, k)* 
      price_control(p_xb)
  )
}
graficador(1,1000,60,"ventas_ps")

"""
FUNCIONES DE BENEFICIOS
"""

beneficios_ps<-function( p_ps, p_xb, c_ps=0, c_xb=0, k=400 , coste = 100 ){
  ventas_ps(p_ps , p_xb , c_ps=c_ps , c_xb=c_xb , k=k ) *
    ( p_ps -  coste )
}

beneficios_xb<-function( p_xb , p_ps , c_ps=0 , c_xb=0 , k=400 , coste = 70 ){
  ventas_xb( p_xb , p_ps , c_ps=c_ps , c_xb=c_xb , k=k ) * 
    ( p_xb - coste )
}

graficador(1,1000,60,"beneficios_ps")

"""
FUNCIONES DE RESOLUCIÓN
"""

resolucion<- function( coste_ps , coste_xb , c_ps = 0 , c_xb = 0 , x0=0 , y0=0 , N=100 ){
  z = matrix(0,nrow=2,ncol=c+1); z[,1] <- c(x0,y0)
  for(i in 1:N){
    z[1,i+1] <- optim(z[1,i],function(x){-beneficios_ps( x , z[2,i] , c_ps , c_xb , coste = coste_ps )})$par
    z[2,i+1] <- optim(z[2,i],function(y){-beneficios_xb( y , z[1,i+1] , c_ps , c_xb , coste = coste_xb ) })$par
  }
  return(z)
}

resolucion(100,70)

