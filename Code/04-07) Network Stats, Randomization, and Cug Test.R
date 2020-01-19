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
#        uniform random graphs that have similar degree distributions.

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
