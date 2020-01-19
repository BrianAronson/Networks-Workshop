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
    #e) Estimate a tergm model based on the above specifications
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

