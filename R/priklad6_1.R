xTx_inv <- matrix(c(7.75, -6.75, 1.25, -6.75, 6.45, -1.25, 1.25, -1.25, 0.25), nrow = 3, ncol = 3)

# Matica X
# X <- matrix(c(1,3,9,1,1,1,1,2,4,1,4,16), nrow = 4, ncol = 3, byrow = T)

# X <- cbind(rep(1, 4), ucast, ucast^2)

ucast <- c(3,1,2,4)
objav <- c(15,3,8,25) # toto skumame, takze toto je Y

n <- length(objav)

# Vykreslenie
plot(ucast, objav)

# Teraz uz vyuzijeme zabudovanu funkciu R-ka
m1 <- lm(objav ~ 1 + ucast + I(ucast^2))
summary(m1)

# Odhady beta
betaHAT <- m1$coefficients
beta2HAT <- m1$coefficients[3]

# S^2
s2_m1 <- (summary(m1)$sigma)^2

a <- c(0,0,1)

test_stat <- beta2HAT^2 / (( t(a) %*% xTx_inv %*% a ) * s2_m1)
p_val <- 1 - pf(test_stat, 1, 1)

p_val

# Vykreslenie
plot(ucast, objav)
osX <- seq(1, 4, by = 0.01)
osY <- betaHAT[2] * osX + betaHAT[3] * osX^2 + betaHAT[1]
lines(osX, osY, col = 'red')

# b) Akaike

RSS_m1 <- s2_m1*(n-3)
# R2 <- 1 - RSS_m1 / TSS

AIC1 <- log(RSS_m1/n) + 2*3/n # pozor, k = 3

m2 <- lm(objav ~ 1 + ucast)
summary(m2)

betaHAT_sub <- m2$coefficients

# Vykreslenie
plot(ucast, objav)
osX <- seq(1, 4, by = 0.01)
osY <- betaHAT_sub[2] * osX + betaHAT_sub[1]
lines(osX, osY, col = 'red')

s2_m2 <- (summary(m2)$sigma)^2
RSS_m2 <- s2_m2*(n-2)

AIC2 <- log(RSS_m2/n) + 2*2/n # pozor, k = 2

#cim mensie AIC, tym "kvalitnejsi" model
c(AIC1, AIC2)

# Adjusted R^2

TSS <- sum((objav - mean(objav))^2) # rovnake pre oba modely

R2adj_m1 <- 1 - (RSS_m1 / (n - 3)) / (TSS / (n-1))
R2adj_m2 <- 1 - (RSS_m2 / (n - 2)) / (TSS / (n-1)) # pozor na k = 2 v tomto modeli!

c(R2adj_m1, R2adj_m2)


# a) by sme mohli robit aj pomocou RSS:
test_stat <- ((RSS_m2 - RSS_m1) / 1 ) / ( RSS_m1 / (n-3) ) 
