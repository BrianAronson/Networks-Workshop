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