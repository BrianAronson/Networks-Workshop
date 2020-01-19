# Note: What if we want to run OLS regression on network data? We
#       would need to control for network autocorrelation

#10) Run a regression model on network data
    #a) Determine dependent variable
        smokes<-g %v% "smokes"
    #b) Determine independent variables
        grade<-g %v% "grade"
        sex<-g %v% "sex"
    #c) Run model without controlling for network autocorrelation
        glm.model<-glm(smokes~grade+sex)
        summary(glm.model)

#11) Use LNAM to run a regression while controlling for network
    lnam.model<-lnam(y=smokes,x=cbind(grade,sex),W1=g)
    summary(lnam.model)