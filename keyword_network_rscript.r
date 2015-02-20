#####################		NETWORK ANALYSIS OF KEYWORDS IN 169 SUSTAINABLE DEVELOPMENT GOAL TARGETS		############################
# 217 challenges
# 64 subjects
# 58 instruments

### 150220 - starting work on secondpass keyword mapping

#install.packages("reshape")
#install.packages("igraph")
library(reshape);library(igraph)
	
	
	
wd<-"G:/Documents/ScienceProjects/SDG_SystemsThinking/SDG_SystemsThinking_Analysis/SDG_SystemsThinking_Analysis"
fig.wd<-"G:/Documents/ScienceProjects/SDG_SystemsThinking/Rfigures"

workspace<-"zerodraft_keyword_ntwk.RData"

setwd(wd)
#load(workspace)


### analysis type
analysis.comb<-"Challenge"

dframe<-read.csv("Prelim_mapping_key_150220d_oneColumn.csv",header=TRUE,stringsAsFactors =TRUE,na.strings="NA")

if(analysis.comb == "Challenge"){
  
dframe.sub <- dframe[ which( dframe[ ,"Type"] == analysis.comb ), ]
    
}



#dframe.melt.sub<-dframe.melt[which(dframe.melt[,"type"]%in%sub.type.vec),]
#dframe.melt.sub<-dframe.melt.sub[complete.cases(dframe.melt.sub),]


###############			CREATE MATRIX - GOALS AS COLUMNS - VALUES AS ROWS

comb.mat<-matrix(0,nrow=length(unique(dframe.sub[,"Keyword"])),ncol=length(unique(dframe.sub[,"GoalTopic"])))
colnames(comb.mat)<-unique(dframe.sub[,"GoalTopic"])
rownames(comb.mat)<-unique(dframe.sub[,"Keyword"])
comb.df<-as.data.frame(comb.mat)

for(i in 1:ncol(comb.df)){
dframe.sub.i<-dframe.sub[which(dframe.sub[,"GoalTopic"]==names(comb.df)[i]),]
comb.df[sort(unique(match(dframe.sub.i[,"Keyword"],rownames(comb.mat)))),i]<-c(t(table(match(dframe.sub.i[,"Keyword"],rownames(comb.mat)))))
}

###############			PERFORM NETWORK ANALYSIS	

bg=graph.incidence(as.matrix(comb.df))

shapes=c(rep("circle",nrow(comb.df)),rep("square",ncol(comb.df)))
labeldistances=c(rep(0,nrow(comb.df)),rep(0,ncol(comb.df)))
nodecolors=c(rep("white",nrow(comb.df)),rep("grey50",ncol(comb.df)))
labelcexs=c(rep(0.5,nrow(comb.df)),rep(2,ncol(comb.df)))
labelcolors=c(rep("grey50",nrow(comb.df)),rep("black",ncol(comb.df)))


pr=bipartite.projection(bg)
#get.adjacency(pr$proj1,sparse=FALSE,attr="weight") ### get the matrix











setwd(fig.wd)
pdf(paste(analysis.comb,"_",Sys.Date(),".pdf",sep=""),width=8,height=8)
#plot.igraph(bg,vertex.shape=shapes,vertex.label.degree=pi/2,vertex.label.dist=labeldistances,vertex.color=nodecolors,vertex.label.cex=labelcexs,vertex.label.color=labelcolors)#V(bg)$type)
#plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="grey70",vertex.label=V(pr$proj1)$name)
plot(pr$proj2,edge.width=(E(pr$proj2)$weight^2)/10,edge.color="grey70",vertex.label=V(pr$proj2)$name)
#plot(pr$proj2,edge.width=E(pr$proj2)$weight^2,vertex.frame.color="red",edge.color="grey70",vertex.label=V(pr$proj2)$name)


####################	FURTHER PLOTTING OPTIONS

#igraph.options(plot.layout=layout.fruchterman.reingold(en.network,niter=10000),vertex.label.color="black")
#igraph.options(plot.layout=layout.lgl,vertex.label.color="black")
#igraph.options(plot.layout=layout.sphere(en.network),vertex.label.color="black")
#igraph.options(plot.layout=layout.kamada.kawai,vertex.label.color="black")
#igraph.options(plot.layout=layout.drl,vertex.label.color="black")
#palette(rainbow(15))


####################  SUMMARY SCORES
par(mfrow=c(2,2),mar=c(14,2,2,2))


#### CENTRALITY

## authority centrality
authority.vec.vals<-sort(authority.score (pr$proj2, scale = TRUE, weights=NULL, options = igraph.arpack.default)$vector,decreasing=TRUE)
authority.title<-"Kleinberg's centrality scores"

x<-barplot(authority.vec.vals,xaxt="n",main= paste(authority.title,sep=""))
#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
labs <- paste(names(authority.vec.vals))
text(cex=1, x=x-.25, y=-0.01, labs, adj=1,xpd=TRUE, srt=90)



#Eigenvector centrality 
evcent.vals<-sort(evcent(pr$proj2, directed = FALSE, scale = TRUE, weights = NULL,
                         options = igraph.arpack.default)$vector,decreasing=TRUE) 

evcent.title<-"Eigenvector Centrality"

x<-barplot(evcent.vals,xaxt="n",main= paste(evcent.title,sep=""))
#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
labs <- paste(names(evcent.vals))
text(cex=1, x=x-.25, y=-0.1, labs, adj=1,xpd=TRUE, srt=90)



#Bonacich alpha centrality 
alpha.vals<-sort(alpha.centrality(pr$proj2, nodes=V(pr$proj2), alpha=1, loops=FALSE,
                 exo=1, weights=NULL, tol=1e-7, sparse=TRUE),decreasing=TRUE)
alpha.title<-"Bonacich a-centrality"

x<-barplot(alpha.vals,xaxt="n",main= paste(alpha.title,sep=""))
#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
labs <- paste(names(alpha.vals))
text(cex=1, x=x-.25, y=-0.01, labs, adj=1,xpd=TRUE, srt=90)


#Bonacich power centrality 
bonpow.vals<-sort(bonpow(pr$proj2, nodes=V(pr$proj2), loops=FALSE, exponent=1,
       rescale=FALSE, tol=1e-7, sparse=TRUE),decreasing=TRUE)

bonpow.title<-"Bonacich Power Centrality"

x<-barplot(bonpow.vals,xaxt="n",main= paste(bonpow.title,sep=""),ylim=c(0,-1.2))
#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
labs <- paste(names(bonpow.vals))
text(cex=1, x=x-.25, y=0.1, labs, adj=1,xpd=TRUE, srt=90)




#vertex betweenness
betweenness.vals<-sort(betweenness(pr$proj2, v=V(pr$proj2), directed = TRUE, weights = NULL,
                                   nobigint = TRUE, normalized = FALSE),decreasing=TRUE)

betweenness.title<-"Betweeness centrality scores"

x<-barplot(betweenness.vals,xaxt="n",main= paste(betweenness.title,sep=""))
#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
labs <- paste(names(betweenness.vals))
text(cex=1, x=x-.25, y=-0.01, labs, adj=1,xpd=TRUE, srt=90)





dev.off()


### end of figure
        
        


#################
setwd(wd)
save.image("zerodraft_keyword_ntwk.RData")

