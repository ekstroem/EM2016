dskellam <- function(x, mu1, mu2) {
  return(exp(-(mu1+mu2))*(mu1/mu2)^(x/2)*besselI(2*sqrt(mu1*mu2),nu=x)
         )
}


eta <- 2.25
beta1 <- seq(0.1, eta-0.05, 0.1)



skellam <- rep(0, length(beta1))
counter <- 1
for (i in beta1) {

  # Udregn ssh for at hold 1 vinder
  skellam[counter] <- sum(dskellam(1:12, i, eta-i)) / ( sum(dskellam(seq(-10,10,1), i, eta-i)) - dskellam(0, i, eta-i) )
  counter <- counter+1
}

skellam <- data.frame(beta=beta1, prob=skellam)

FindParameter <- function(prob) {

    sapply(prob, function(i) {
               if (i<.009) {
                   return (.1)
               }
               if (i>.995) {
                   return (eta-.05)
               }
               return(min(skellam$beta[skellam$prob>i]))
           }
           )

}


#plot(beta1, skellam, type="l", ylim=c(0,1),
#     xlab="Lambda A",
#     ylab="Sandsynlighed for at hold A vinder", lwd=2)
