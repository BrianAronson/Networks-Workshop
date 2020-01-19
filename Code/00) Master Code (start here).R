#############################OVERVIEW##############################
# Overview: This program teaches models that deal with network
#           simulation, autocorrelation, and diffusion. Of note, 
#           it includes the functions:
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
devtools::install_github("USCCANA/netdiffuseR")
# Notes: 
#    a) This lab requires that all of the following scripts be
#       saved in the same directory. It is recommended that these
#       all be run in succession due to dependencies across files.
#
#    b) The setwd function below must be edited for the lab to
#       work. 
#
#    c) Run the file.edit functions to open each below script,  
#       or run the source functions to run those scripts without 
#       opening them. 
#
#    d) See "X) Master Code (Long-Format)" for a version of this 
#       lab with all of the below scripts included in one long file.


#0) set work directory
      setwd("C:/Users/admin/Downloads/ergm lab")
      
#1-3) Prep work: Load libraries and format data
      file.edit("01-03) Prep Work.R")
      #source("01-03) Prep Work.R")

#4-7) Network Stats, Randomization, and Cug Test: Significance
#     testing of network statistics    
      file.edit("04-07) Network Stats, Randomization, and Cug Test.R")
      #source("04-07) Network Stats, Randomization, and Cug Test.R")
    
#8-9) QAP and MR-QAP: Significance testing of network 
#     correlations
      file.edit("08-09) QAP and MR-QAP.R")
      #source("08-09) QAP and MR-QAP.R")
    
#10-11) LNAM: Network autocorrelation regression model
      file.edit("10-11) LNAM.R")
      #source("10-11) LNAM.R")

#12-18) ERGM: Exponential random graph models
      file.edit("12-18) ERGM.R")
      #source("12-18) ERGM.R")
      
#19-21) Diffusion models
      file.edit("19-21) Diffusion.R")