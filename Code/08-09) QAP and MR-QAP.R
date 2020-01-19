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
