ThreewayDME.f <- function(M,X,Z,W,xlab,ylab,lloc,Low,High,level)
{

## A function to generate 3-way marginal effects plots in R with stars for set confidence levels -- 1 discrete, dichotomous modifier
## Written by Joshua Gubler ~ http://joshuagubler.wordpress.com ; based on Stata code from Joel Selway and Brambor, Clark, and Golder
## Last modified: 5 February 2011

## Variables must be in the following order: y = x z w (control variables here) xz xw zw xzw .  The model can include as many control variables as you need.  As long as you get the first three variables in correct order, R will correctly order the interaction terms.
## M = an object of type "lm," "glm," or other estimation -- i.e. the object that contains the regression estimation you seek to plot
## X = the variable whose effect on Y you seek to plot
## Z = the first moderating variable (will be positioned on the X-axis of the plot)
## W = the second moderating variable (the lines on the plot -- this is your discrete, dichotomous modifier)
## xlab = Label for x-axis (in quotes)
## ylab = label for y-axis (in quotes)
## lloc = location of the legend for the plot, use values like "bottomleft"
## Low, High = titles for each of the lines to be put in the legend -- e.g. "Control", "Low" (titles must be in quotes)
## level = to set the confidence level.  Two options (don't put these in quotes): 95, 90.  Stars will show on lines that are significant at the level you set.  If you do not put either option, stars will show on all lines.

## Example: ThreewayDME.f(estimation.lm,ses,edu,nato,"Education levels","Effect of SES on Civil War","bottomleft","Not Member","Member",90) -- here nato is dichtomous 1 == member, 0 == not

S <- summary(M)
N <- c(1:20)

#     ################################################################  #
#       Create 20 equally spaced values in a vector between min         #
#       and max on the Z variable                                       #
#     ################################################################  #

zmin <- rep(min(Z,na.rm=TRUE), 20)
zmax <- rep(max(Z, na.rm=TRUE), 20)
Z <- (((N-1)/(20-1))*(zmax-zmin))+zmin

#     ################################################################  #
#       Generate the values of W for which you want to calculate the    #
#       marginal effect (and standard errors) of X on Y.                #
#     ################################################################  #

W0 <- quantile(as.numeric(W),   0, na.rm=TRUE)
W1 <- quantile(as.numeric(W), 1, na.rm=TRUE)

#     ################################################################  #
#       Grab elements of the coefficient and variance-covariance matrix #
#       that are required to calculate the marginal effect and standard #
#       errors.                                                         #
#     ################################################################  #

H <- head(S$coefficients,4)
T <- tail(S$coefficients,4)
b <- rbind(H,T)
Vcov <- vcov(M)
Vcov <- as.data.frame(Vcov)
Vcov1 <- Vcov[,c(1:4)]
Vcov2 <- Vcov[,-c(3:0-length(Vcov))]
Vcov <- cbind(Vcov1,Vcov2)
Vh <- head(Vcov,4)
Vt <- tail(Vcov,4)
V <- rbind(Vh,Vt)

b1 <- b[2,1]
b2 <- b[3,1]
b3 <- b[4,1]
b4 <- b[5,1]
b5 <- b[6,1]
b6 <- b[7,1]
b7 <- b[8,1]

varb1 <- V[2,2]
varb2 <- V[3,3]
varb3 <- V[4,4]
varb4 <- V[5,5]
varb5 <- V[6,6]
varb6 <- V[7,7]
varb7 <- V[8,8]

covb1b4 <- V[5,2]
covb1b5 <- V[6,2]
covb1b7 <- V[8,2]
covb4b5 <- V[6,5]
covb4b7 <- V[8,5]
covb5b7 <- V[8,6]

#     ################################################################  #
#       We want to calculate the marginal effect of X on Y for all      #
#       Z values of the modifying variable Z. We also want to           #
#       calculate this marginal effect as Z changes for specific values #
#       of the second modifying variable W.  In the code below, we      #
#       calculate the marginal effect of X on Y for values of Z         #
#       when W=0 and W=1.                                               #
#     ################################################################  #

conb0 <- b1+b4*Z+b5*W0+b7*(Z*W0)
conb1 <- b1+b4*Z+b5*W1+b7*(Z*W1)

#     ################################################################  #
#       Calculate the standard errors for the marginal effect of X on Y #
#       for all Z values of the modifying variable Z. Do this for the   #
#       case when W=0 and W=1,                                          #
#     ################################################################  #

conse0 <- sqrt(varb1
               + varb4*(Z^2) + varb5*(W0^2) + varb7*(Z^2)*(W0^2)
               + 2*Z*covb1b4 + 2*W0*covb1b5 + 2*Z*W0*covb1b7 + 2*Z*W0*covb4b5
               + 2*W0*(Z^2)*covb4b7 + 2*(W0^2)*Z*covb5b7)

conse1 <- sqrt(varb1
               + varb4*(Z^2) + varb5*(W1^2) + varb7*(Z^2)*(W1^2)
               + 2*Z*covb1b4 + 2*W1*covb1b5 + 2*Z*W1*covb1b7 + 2*Z*W1*covb4b5
               + 2*W1*(Z^2)*covb4b7 + 2*(W1^2)*Z*covb5b7)              

#     ################################################################  #
#                           Create t statistics                         #
#     ################################################################  #

t0 <- conb0/conse0
t1 <- conb1/conse1

#     ################################################################  #
#       Make a "shadow" variable that is missing if the t score is not  #
#       larger than the critical level of significance that you want.   #
#     ################################################################  #

ci <- NA
ci[level==95] <- 1.96
ci[level==90] <- 1.645

stars.df <- data.frame(consb0=conb0,consb1=conb1,t0=t0,t1=t1)
stars.df$consb0[abs(stars.df$t0)<ci] <- NA
stars.df$consb1[abs(stars.df$t1)<ci] <- NA

#     ################################################################  #
#       Generate a string variable called txt that is designated with  #
#       a star.                                                         #
#     ################################################################  #

txt <- c("*")

#     ################################################################  #
#       Graph the marginal effect of X on Y across the desired range of #
#       the modifying variable Z. Do this for when W=0 and W=1,         #
#     ################################################################  #

plot(c(Z,Z,Z,Z), c(conb0,stars.df$consb0,conb1,stars.df$consb1), type="n",xlab=xlab,ylab=ylab)
lines(Z,conb0,col="blue")
text(x=Z,y=stars.df$consb0,labels=txt)
lines(Z,conb1,col="red")
text(x=Z,y=stars.df$consb1,labels=txt)
legend(lloc,legend=c(Low,High),col=c("blue","red"),lty = c("solid"))
abline(h=0)
}

