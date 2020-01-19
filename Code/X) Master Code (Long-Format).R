#############################OVERVIEW##############################
# Overview: This program teaches models that deal with network
#           simulation, autocorrelation, and diffusion, including
#           the functions:
#               sna: rgnmix, rgraph, rguman, cug.test, qaptest,
#                    netlm, lnam
#               ergm: ergm, mcmc.diagnostics, simulate.ergm
#
# Credits: This lab is created by Brian Aronson for Duke's SN&H
#          workshop on 5/16/19. However, much of the following 
#          code is adapted from tutorials by James Moody, Jeff 
#          Smith, Jake Fisher, the statnet team, Tom Valente, 
#          and George G. Vega Yon
###################################################################

# Notes: 
#   #a) This is a long version of the lab, held in one file. 
#
#   #b) The setwd function below must be edited for the lab to
#       work.
#
#   #c) See "0) Master Code (START HERE)" for a version of this  
#       lab with the below scripts separated into smaller files.


#0) set work directory
      setwd("C:/Users/bda13/Desktop/ERGM Lab/ERGM and Diffusion Lab")

#1) Prep work
    #a) restart workspace
       #i) Deletes all objects in your environment
          rm(list=setdiff(ls(), list("Package_Name",
                                     "Description")))
      #ii) Remove any remnants from memory
          gc()
    #b) Install packages
        #i) Identify necessary packages
            list.of.packages<-c("tidyverse", "statnet","ergm",
                                "ggplot2","sna","EpiModel",
                                "netdiffuseR","networkDynamic")
        #ii) Identify which packages are not installed on pc
            new.packages<-list.of.packages[!(list.of.packages
                          %in% installed.packages()[,"Package"])]
        #iii) Install any packages not already on pc
            if(length(new.packages)) install.packages(new.packages)
    #c) Load necessary packages
        library(tidyverse)
        library(statnet)
        library(ggplot2)
        library(sna)
        library(ergm)
        library(EpiModel)
        library(netdiffuseR)
    #d) Load data
        AHS_Base<-read_csv('ahs_wpvar.csv',col_names = TRUE)

#2) Create an edge list based on AHS's network data
    #a) Create a new dataframe with only the variables we will need
        AHS_adjlist<-AHS_Base %>% select(ego_nid, mfnid_1:mfnid_5,
                                         ffnid_1:ffnid_5, grade,
                                         sex, PSMOKES,commcnt)
    #B) Only keep info where commcnt==1
        AHS_adjlist<-AHS_adjlist %>% filter(commcnt==1)
    #c) Use adjacency list to build an edge list
        AHS_Edges<-AHS_adjlist %>% gather(Alter_Label, Target,
                                          mfnid_1:mfnid_5,
                                          ffnid_1:ffnid_5,
                                          na.rm = TRUE)

# Note: In the above code, the gather function collapses 10
#       columns (mfnid and ffnid) into one. The values of those
#       columns are places in "Target". The column names of
#       those values are now in "Alter_Label. Rows are added
#       to the dataset to reflect this merge.

    #d) Rename our id variable
        AHS_Edges<-AHS_Edges %>% rename(id = `ego_nid`)
    #e) Remove edges with missing Target info
        AHS_Edges<-AHS_Edges %>% filter (Target != 99999)
    #f) Just keep the sender and receiver info
        AHS_Edges<-AHS_Edges %>%select(id, Target)

#3) Convert to graph object and derive some basic node-level
#   statistics
    #a) Convert edgelist to graph object to use statnet functions
        g<-as.network(AHS_Edges)
    #b) Assign attributes to certain vertices (aka "nodes");
    #   sna requires a unique syntax for assigning attributes
        g %v% "grade"<-AHS_adjlist$grade
        g %v% "sex"<-AHS_adjlist$sex
        g %v% "smokes"<-AHS_adjlist$PSMOKES
    #c) Get degree scores
        outdegree<-degree(g, cmode = "outdegree")
        indegree<- degree(g, cmode = "indegree")
    #d) Assign degree scores to vertices
        g %v% "indegree"<-indegree
        g %v% "outdegree"<-outdegree
        g %v% "degree"<-degree(g)
    #e) Plot network and color by attribute
        plot(g, vertex.col="grade")
        plot(g, vertex.col="sex")
        
# Note: How do we determine if our network statistics are
#       statistically significant? What should our baseline
#       expectation be? To do this, we can compare our graph
#       to simulated random graphs.

#4) Derive some network-level statistics
    #a) Network transitivity
        obs_tran=gtrans(g)
        obs_tran
    #b) Distribution of dyads
        dyads=dyad.census(g)
        dyads
    #c) Assign information about our network's degree distributions
    #   to objects
        our_mut<-dyads[1] #number reciprocal ties (mutual ties)
        our_asym<-dyads[2] #number non-reciprical ties (asymmetric ties)
        our_null<-dyads[3] #number of null ties possible ties valued 0)

# Note: Is our transitivity value high or low? Let's compare to
#       uniform random graphs that have similar degree distributions.

#5) Compare our network transitivity to that of a uniform random graph
    #a) Simulate a uniform random graph
        #i) Input determine number of simulations
            nsims=500
        #ii) Initialize an empty vector based on number of times to simulate
            sim_tran=vector(length=nsims)
        #iii) Determine the transitivity in each vector
            for(i in 1:nsims){ #for each simulation
              # Simulate a network based on the degree distribution
              # in our network
                  sim_g=rguman(1,nv=71,mut=our_mut,asym=our_asym,null=our_null)
              # Determine transitivity of that network and assign it
              # to a cell in sim_tran
                  sim_tran[i]=gtrans(sim_g)
            }
    #b) Graph our simulated estimates of transitivity from uniform
    #   random graphs and compare them to our dataset
        #i) Convert simulated results to a tibble for use with ggplot
            dt=tibble(sim_tran)
        #ii) Plot results
            # Assign our simulations to a ggplot object and identify
            # the x axis
                p<-ggplot(dt,aes(x=sim_tran))
            # Make a histogram based on our simulations
                p<-p+geom_histogram()
            # Make a red line on the graph illustrating our observed
            # rate of transitivity
                p<-p+geom_vline(xintercept=obs_tran,
                        color="red")
            # Print our graph to the plots pane
                p

# Note: The above simulations can also be done with igraph's
#       sample_degseq function

# Note: Maybe our transitivity scores are influenced by rates of
#       grade mixing (i.e. transitivity in our random graphs
#       differed from those in our data because our simulations
#       didn't account for homophily within grades. Let's try
#       creating a random graph that accounts for grade mixing

#6) Compare our network transitivity to that of a uniform random
#   graph with a similar rate of grade mixing
    #a) Determine a mixing matrix based on grades (indicates
    #   the rate of ties across and within grades)
        grades=as.character(g %v% "grade")
        grdmix=mixingmatrix(g, "grade")
        grdmix
    #b) Simulate a uniform random graph that replicates mixing
    #   matrix of grades
        #i) input determine number of simulations
            nsims=500
        #ii) Initialize an empty vector based on number of times
        #    to simulate
            sim_tran=vector(length=nsims)
        #iii) Determine the transitivity in each vector
            for(i in 1:nsims){ # for each simulation
              # Simulate a network based on our mixing matrix
                  sim_g<-rgnmix(1,grades,grdmix$matrix,method="exact",return.as.edgelist=TRUE)
              # Determine transitivity of that network and assign
              # it to a cell in sim_tran
                  sim_tran[i]=gtrans(sim_g)
            }
    #c) Graph our simulated estimates of transitivity from uniform
    #   random graphs and compare them to our dataset
        #i) Convert our simulated results to a tibble
            dt=tibble(sim_tran)
        #ii) Plot our results
            # Assign our simulations to a ggplot object and identify
            # the x axis
                p<-ggplot(dt,aes(x=sim_tran))
            # Make a histogram based on our simulations
                p<-p+geom_histogram()
            # Make a red line on the graph illustrating our observed
            # rate of transitivity
                p<-p+geom_vline(xintercept=obs_tran,
                        color="red")
            # Print our graph to the plots pane
                p

# Note 1: Notice we are repeating a lot of syntax? This is usually
#       unnecessary in R. In practice, we could assign a lot of
#       our duplicated steps to functions. R is a functional
#       programming language; once you get familiar with R, try to
#       take full advantage of this.

# Note 2: We could complicate the simulation models to
#       simultaneously cluster by sex, grade, race, etc., but this
#       is getting unwieldy.

# Alternative: An easy way to test whether the graph has more
#       transitivity than we would expect in a random graph given
#       its size, density, and dyad census is the cugtest.

#7) Run cug.test to compare univariate statistics to random models
    cug.test(dat=g,FUN=gtrans,cmode="size")
    cug.test(dat=g,FUN=gtrans,cmode="edges")
    cug.test(dat=g,FUN=gtrans,cmode="dyad.census")
        
# Note: What if we are interested in understanding whether certain
#       node attributes are more homophilous than we'd expect by
#       chance? Again, we must compare to null models. For this,
#       we can use QAP.

#8) Use QAP to determine significance of network correlations
    #a) Create a matrix indicating whether respondents have the
    #   same sex and same grade
        #i) Identify variables of interest
            grade<-g %v% "grade"
            sex<-g %v% "sex"
        #ii) Estimate distances between all values in each vector
            grade.m<-dist(grade)
            sex.m<-dist(sex)
        #iii) Convert distance objects to matrices
            grade.m<-as.matrix(grade.m)
            sex.m<-as.matrix(sex.m)
        #iv) For example, look at first 3 cells in sex vector and
        #    in sex matrix
            sex[1:3]
            sex.m[1:3,1:3] #values are just subtractions of above
    #b) Convert g to adjacency matrix
        adj.mat<-as.matrix.network.adjacency(g)
    #c) Run QAP on grade and sex separately
        #i) Grade
            qap1<-qaptest(list(adj.mat,grade.m),gcor,g1=1,g2=2)
            summary(qap1)
        #ii) Sex
            qap2<-qaptest(list(adj.mat,sex.m),gcor,g1=1,g2=2)
            summary(qap2)
            plot(qap2) #visualize simulated vs real

#9) Run MR-QAP on grade and sex jointly
    qap3<-netlm(adj.mat,list(grade.m,sex.m))
    summary(qap3)
    
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
    
################################################################################
# ERGM - Exponential Random Graphs
################################################################################

# Note: What micro-level network processes generate our graphs?
#       What factors influence people's preference for making
#       particular ties? To determine this in a cross-sectional
#       network, we can use ERGMs.

#12) Prep work
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

#13) run a very simple ergm
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
    
######Diffusion Models#####

###EpiModel###
#Note: These examples are adapted from the statnet team's EpiModel
#      tutorial for Sunbelt 2015. http://statnet.github.io/sb/t1.html
        
#19) Simulate a network with EpiModel
    #a) Create 500 nodes with no edges
        nw <- network.initialize(n = 500, directed = FALSE)
    #b) Create a formula to simulate whether nodes become linked
    #    based on density and concurrency
        formation <- ~edges + concurrent
    #c) Specify the number of edges and concurrent ties to approach
    #   in the simulated network
        target.stats <- c(175, 110)
    #d) Specify how many simulated steps ties should exist before
    #   they dissolve
        coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 50)
    #e) estimate a tergm model based on the above specifications
        est <- netest(nw, formation, target.stats, coef.diss)
    #f) Get diagnostics of the above model by running 10 simulations,
    #   each that run for 1000 steps.
        dx <- netdx(est, nsims = 10, nsteps = 1000,
                    nwstats.formula = ~edges + meandeg+
                    degree(0:1) + concurrent)
    #g) Print our results
        dx

    #Note: We want our simulated means to be similar to our
    #      target means for tie formation and dissolution.

    #h) Print our diagnostics over time.
        plot(dx, mean.smooth = TRUE, mean.lwd = 3,
             plots.joined = FALSE)

    #Note: We want our simulated statistics to be relatively
    #      in line with our target statistics throughout the
    #      1000 simulated steps, as is the case here. Since
    #      the networks are good, let's simulate a disease

#20) Use the above network for simulating epidemics with EpiModel
    #a) Set model parameters for disease diffusion and recovery
        param <- param.net(
          inf.prob = 0.4,  #transmission probability per act
          act.rate = 2,    #number acts per partnership per unit of time
          rec.rate = 0.01  #recovery rate
        )
        init <- init.net(i.num = 10) #number initially infected
        control <- control.net(
          type = "SIS",    #type of disease (SIS = people can get infected multiple times)
          nsims = 5,       #number of simulations
          nsteps = 500     #steps in simulations
        )
    #b) Simulate disease diffusion over our network (takes time)
        sim <- netsim(est, param, init, control)
    #c) Print diagnostics
        #i) Number susceptible and infected over time
            plot(sim)
        #ii) Network plot of disease at t=1 and t=500
            par(mar = c(0,0,0,0), mfrow = c(1, 2))
            plot(sim, type = "network", col.status = TRUE, at = 1, sims = 1)
            plot(sim, type = "network", col.status = TRUE, at = 500, sims = 1)
            dev.off()


###netdiffuseR###
#Note: This example is adapted from Valente and Vega Yon's
#      tutorial for Sunbelt 2018. https://usccana.github.io/netdiffuser-sunbelt2018/sim.html

#21) Simulate patterns of rumor diffusion with netdiffuser
    #a) Simulate rumor diffusion based on the following:
        diffnet_rumor <- rdiffnet(
          n = 500,                                 # number nodes
          t = 5,                                   # number periods
          seed.graph = "small-world",              # type of graph
          rgraph.args = list(p = .3),              # rewire probability
          seed.p.adopt = .05,                      # percent network that are seeds
          threshold.dist = function(i) 1L,         # threshold for spread
          exposure.args = list(normalized = FALSE) # measure exposure as a count
        )
    #b) Create table of exposure/adopters over time
        summary(diffnet_rumor)
    #c) Visualize the network at t=1,t=3,and t=5
        plot_diffnet(diffnet_rumor, slices = c(1, 3, 5))
    #d) Plot cumulative adoption over time
        plot_adopters(diffnet_rumor, what = "cumadopt")

        