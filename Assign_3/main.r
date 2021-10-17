f <- function(x) dnorm(x)
g <- function(x) dcauchy(x)

arnc <- function(n=100, M=1.52){
    x <- rep(0,n)
    for (i in 1:n){
        repeat{
            y <- rcauchy(1)
            u <- runif(1,0,1)
            if(u < (f(y)/(M*g(y)))) break 
        }
        x[i] <- y 
    }
    
    return(x)
}

arnc(10)
z= arnc(10000)
hist(z)
summary(z)
boxplot(z)

hist(rnorm(10000))
qqplot(rnorm(10000),z)







