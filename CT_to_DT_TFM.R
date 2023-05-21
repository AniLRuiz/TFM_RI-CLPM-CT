library(ggplot2)
#library(patchwork)
#library(MASS)
#library(Matrix)

summary(bfitT)
# Obtaining matrices with ct parameters ----------------------
# replace "bfit" with the name of your fitted OpenMx model (not the summary)
A_ct <- bfitT$indiv1$A$values
names(A_ct) <- bfitT$indiv1$A$labels

Q_ct <- bfitT$indiv1$Q$values
names(Q_ct) <- bfitT$indiv1$Q$labels

# Los siguientes parámetros están definidos en una métrica independiente del 
# tiempo en un RI-CLPM. No es necesario reescalarlos
Mu <- bfitT$indiv1$x0$values
rownames(Mu) <- bfitT$indiv1$x0$labels
Mu

Sigma <- bfitT$indiv1$P0$values
names(Sigma) <- bfitT$indiv1$P0$labels
Sigma

# Re-scaling to a specific dt interval -----------------------

dl <- c(.001, seq(0.1, 20, by = 0.1)) # delta with different intervals


CT2DT <- function(A_ct, Q_ct, delta){
  A_ct <- A_ct[1:2,1:2] # We take only the parameters defining the system dynamics
  Q_ct <- Q_ct[1:2,1:2]
  I <- diag(1, dim(A_ct)[1])
  
  # Conversion of A (drift matrix)
  # equation: A(delta)= e^(ct_amat*delta)  (Equation 7, in Voelkle, 2012)
  A_dt <- Matrix::expm(A_ct * delta)
  
  # Conversion of Q (dynamic error)
  # equation: irow (A#-1 *[e^(A#*(delta))-I] row Q) (Equation 11, in Voelkle, 2012)
  A. <- A_ct %x% I + I %x% A_ct # A con almohadilla (del artículo)
  rowQ <- matrix (c (Q_ct), ncol=1)
  I. <- diag (dim(I)[1]*2)
  
  Q_dt <- matrix ((solve (A.)) %*% (expm(A.*delta)-I.) %*% rowQ, ncol=dim(Q_ct)[1])
  
  # get results out
  out <- list(A_dt = A_dt,
              Q_dt = Q_dt,
              delta = delta) }

resultados <- lapply(dl, FUN = function(d) {CT2DT(A_ct, Q_ct, delta = d)})
resultados[[21]]

getDTpars <- function(cD) {
  A_dt <- matrix(as.matrix(cD$A_dt), ncol=1)[,]
  names(A_dt) <- c("a11", "c21", "c12", "a22")
  
  Q_dt <- matrix(as.matrix(cD$Q_dt), ncol=1)[c(1,2,4),]
  names(Q_dt) <- c("derx", "derxy", "dery")
  
  out <- c(intervalo = cD$delta, A_dt, Q_dt)
  }

data <- data.frame(t(sapply(resultados, getDTpars)))

data[21,] # Parameters for a time lag of 1 year (2 semesters)


# Plotting -------------------------------
# Auto-effects or inertia
p1 <- ggplot(data, aes(intervalo)) + 
  geom_line(aes(y = a11, colour = "autorregresivo SDA")) + 
  geom_line(aes(y = a22, colour = "autorregresivo NDC")) +
  scale_x_continuous(breaks= seq(0,40))+
  geom_vline(xintercept = 2, color = "navy")+
  theme_bw() +
  geom_text(aes(x=2, label="\n 1 año", y=0.75), colour="navy", angle =90,
            check_overlap = T) +
  labs(x = "Intervalo (Semestres)", y = "Valor", col = "Parámetros") + 
  scale_color_manual(name = "Parámetros", values = c("autorregresivo SDA" = "gray8", 
                                                     "autorregresivo NDC" = "gray48"))
p1 


p1v2 <- ggplot(data, aes(intervalo)) + 
  geom_line(aes(y = a11)) + 
  geom_line(aes(y = a22)) +
  scale_x_continuous(breaks= seq(0,40))+
  geom_vline(xintercept = 2, color = "navy")+
  theme_bw() +
  geom_text(aes(x=2, label="\n 1 año", y=0.75), colour="navy", angle =90,
            check_overlap = T) +
  labs(x = "Intervalo (Semestres)", y = "Valor") 

p1v2 

# Cross-lagged effects or spillover
p2 <- ggplot(data, aes(intervalo)) + 
  geom_line(aes(y = c12, colour = "efecto cruzado NDC a SDA")) + 
  geom_line(aes(y = c21, colour = "efecto cruzado SDA a NDC")) +
  scale_x_continuous(breaks= seq(0,40))+
  geom_vline(xintercept = 2, color = "navy")+
  theme_bw()  +
  geom_text(aes(x=2, label="\n 1 año", y=-0.05), colour="navy", angle =90,
            check_overlap = T) + 
  labs(x = "Intervalo (Semestres)", y = "Valor", col = "Parámetros") + 
  scale_color_manual(name = "Parámetros", values = c("efecto cruzado NDC a SDA" = "gray8", 
                                                       "efecto cruzado SDA a NDC" = "gray48"))
p2

# Dynamic error / innovation variance
p3<- ggplot(data, aes(intervalo)) + 
  geom_line(aes(y = derx, colour = "innovaciones v SDA")) + 
  geom_line(aes(y = dery, colour = "innovaciones v NDC")) + 
  geom_line(aes(y = derxy, colour = "innovaciones cov")) +
  scale_x_continuous(breaks= seq(0,40, 2))+
  geom_vline(xintercept = 2, color = "navy")+
  theme_bw()+
  geom_text(aes(x=2, label="\n 1 año", y=0.20), colour="navy", angle =90,
            check_overlap = T) +
  labs(x = "Intervalo (Semestres)", y = "Valor", col = "Parámetros") +
  scale_color_manual(name = "Parámetros", values = c("innovaciones v SDA" = "gray8", 
                                                     "innovaciones v NDC" = "gray48",
                                                     "innovaciones cov" = "gold4"))
p3

