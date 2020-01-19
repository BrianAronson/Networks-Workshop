################################################################################
# ERGM - Exponential Random Graphs
################################################################################

# Note: What micro-level network processes generate our graphs?
#       What factors influence people's preference for making 
#       particular ties? To determine this in a cross-sectional 
#       network, we can use ERGMs.

#12) prep work
    #a) To keep our environment from getting too cluttered, let's 
    #   just keep what we need.
        #i) Identify objects to keep
            keeps<-c("g","indegree","outdegree")
        #ii) Identify all objects in environment
            all.objects<-ls()
        #iii) Convert all.objects to tibble for use with filter
        #     function
            all.objects<-tibble(all.objects)
        #iv) Remove keeps from all.objects
            all.objects<-all.objects %>% filter(!all.objects %in% keeps)
        #v) Remove variables with a name in all.objects and
        #   all.objects
            rm(list=c(all.objects$all.objects,"all.objects"))

#13) Run a very simple ergm
    #a) Run an ergm based only on the degree distribution of g
        mod.rand<-ergm(g~edges)
    #b) Pull more detailed summary stats of our model
        summary(mod.rand)
    #c) Interpret model coefficients
        #i) Pull our parameter estimate from the above summary
        #   function
            mod.sum<-summary(mod.rand)
            coef<-mod.sum$coefficients
            estimate<-coef$Estimate
            estimate
        #ii) Convert our estimate from log odds to a probability
            probability<-exp(estimate)/(1+exp(estimate))
            probability
        #iii) Notice the above is equivalent to our graph density
            gden(g)

#14) Run a slightly more complex ergm
    #a) There are a ton of terms that you can add to an ergm.
    #   Below brings up the list.
        ?`ergm-terms`
    #b) Run an ergm that based on:
        mod1<-ergm(g~edges+             #graph density
                   nodematch("sex")+    #preference for same sex
                   nodematch("grade")+  #preference for same grade
                   nodematch('smokes')  #preference for same smokes
                 )
    #c) View our results
        summary(mod1)
    #d) Run goodness of fit diagnostics
        #i) Estimate goodness of fit for our model
            mod.gof=gof(mod1)
        #ii) Plot results
            plot(mod.gof) #prints a bunch of models; click back
                          #on the model pane to see them

#15) Crazy model with covariates for each sender and receiver
    #a) Run an ergm
        mod2<-ergm(g~edges+
                   nodematch("sex")+
                   nodematch("grade")+
                   nodematch('smokes')+
                   sender+
                   receiver
                 )
    #b) View summary stats
        summary(mod2) #BIC is bad
    #c) View GOF
        mod.gof=gof(mod2) #good GOF but overfit
        plot(mod.gof)

#16) Model with indegree and outdegree covariates
    #a) Create indegree and outdegree covariates
        g %v% "indegree"<-indegree
        g %v% "outdegree"<-outdegree
    #b) Run an ergm
        mod3<-ergm(g~edges+
                   nodematch("sex")+
                   nodematch("grade")+
                   nodematch('smokes')+
                   nodeicov("indegree")+
                   nodeocov("outdegree")
                 )
    #c) View summary stats
        summary(mod3)
    #d) Convenient way to find out more about an ergm term
        search.ergmTerms(name="nodeicov")

#17) Model with reciprocity and transitivity covariate
    #a) Add mutual to model
        mod4<-ergm(g~edges+
                   nodematch("sex")+
                   nodematch("grade")+
                   nodematch('smokes')+
                   nodeicov("indegree")+
                   nodeocov("outdegree")+
                   mutual
                 )
    #b) View summary stats
        summary(mod4)
    #c) NOT RUN: add triangle to model
        # mod5<-ergm(g~edges+
        #              nodematch("sex")+
        #              nodematch("grade")+
        #              nodematch('smokes')+
        #              nodeicov("indegree")+
        #              nodeocov("outdegree")+
        #              mutual+
        #              triangle
        # )

#Note: Adding triangle crashes the model. Why? Because most ties
#      are intransitive. This parameter is too strong; it often
#      causes the model to predict that all ties are transitive. 
#      Below is a sub par work-around.

    #d) Change mcmc iterations
        mod5<-ergm(g~edges+               #graph density
                     nodematch("sex")+    #preference for same sex
                     nodematch("grade")+  #preference for same grade
                     nodematch('smokes')+ #preference for same smokes
                     nodeicov("indegree")+
                     nodeocov("outdegree")+
                     mutual+
                     triangle,
                     control=control.ergm(MCMLE.maxit=2)
        )
        summary(mod5)
    #e) Check mcmc diagnostics
        mcmc.diagnostics(mod5)
    #f) Simulate network based on ergm
        test=simulate(mod5,1)
        plot(test) #fit is poor here; we want values to be randomly 
                   #distributed around the mean;
        
#18) Run ergm with gwesp instead of triangle (this is better)
    #a) Run ergm
        mod6<-ergm(g~edges+               
                     nodematch("sex")+    
                     nodematch("grade")+  
                     nodematch('smokes')+ 
                     nodeicov("indegree")+
                     nodeocov("outdegree")+
                     mutual+
                     gwesp
        )
        summary(mod6)
    #b) Check mcmc diagnostics
        mcmc.diagnostics(mod6) #not great
    #c) Simulate network based on ergm
        test=simulate(mod6,1)
        plot(test)
