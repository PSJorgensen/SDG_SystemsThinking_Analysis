#####################		NETWORK ANALYSIS OF KEYWORDS IN 169 SUSTAINABLE DEVELOPMENT GOAL TARGETS		############################
# 217 challenges
# 64 subjects
# 58 instruments



#install.packages("reshape")
#install.packages("igraph")
library(reshape);library(igraph)
	
	
	
wd<-"D:/SDG_leveragepoints"
fig.wd<-"D:/SDG_leveragepoints/Rfigures"

workspace<-"zerodraft_keyword_ntwk.RData"

setwd(wd)
#load(workspace)



dframe<-read.csv("SDG_targets_import.csv",header=TRUE,stringsAsFactors =TRUE,na.strings="NA")

dframe.melt<-melt(dframe[,c(
	"Goal","Gtopic","Target","Timeline1","Timeline2","Challenge1","Challenge2","Challenge3","Challenge4","Challenge5","Challenge6","Challenge7",
	"Challenge8","Challenge9","Challenge10","Challenge11","Challenge12","Challenge13","Subject1","Subject2","Subject3","Subject4","Subject5","Subject6","Solution1","Solution2","Solution3","Solution4","Solution5","Solution6",
	"Condition1","Condition2","Condition3","Condition4","Condition5","Condition6","Condition7",
	"Condition8","Condition9","Condition10","Condition11","Condition12"
	)],id.vars=c("Goal","Gtopic","Target"),variable_name="type")

#sub.type.vec<-c("Challenge1","Challenge2","Challenge3","Challenge4","Challenge5","Challenge6","Challenge7",
#	"Challenge8","Challenge9")#"Timeline1","Timeline2","Subject1","Subject2","Subject3","Subject4","Subject5","Subject6","Solution1","Solution2","Solution3","Solution4","Solution5"

#sub.type.vec<-names(dframe)[grep("Challenge",names(dframe))]	
#sub.type.vec<-names(dframe)[grep("Subject",names(dframe))]	
sub.type.vec<-names(dframe)[grep("Solution",names(dframe))]	
#sub.type.vec<-names(dframe)[grep("Timeline",names(dframe))]	
#sub.type.vec<-names(dframe)[grep("Condition",names(dframe))]	
	
	
dframe.melt.sub<-dframe.melt[which(dframe.melt[,"type"]%in%sub.type.vec),]
dframe.melt.sub<-dframe.melt.sub[complete.cases(dframe.melt.sub),]


###############			CREATE MATRIX - GOALS AS COLUMNS - VALUES AS ROWS

comb.mat<-matrix(0,nrow=length(unique(dframe.melt.sub[,"value"])),ncol=length(unique(dframe.melt.sub[,"Gtopic"])))
colnames(comb.mat)<-unique(dframe.melt.sub[,"Gtopic"])
rownames(comb.mat)<-unique(dframe.melt.sub[,"value"])
comb.df<-as.data.frame(comb.mat)

for(i in 1:ncol(comb.df)){
dframe.melt.sub.i<-dframe.melt.sub[which(dframe.melt.sub[,"Gtopic"]==names(comb.df)[i]),]
comb.df[sort(unique(match(dframe.melt.sub.i[,"value"],rownames(comb.mat)))),i]<-c(t(table(match(dframe.melt.sub.i[,"value"],rownames(comb.mat)))))
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
pdf("solution_plots_141219.pdf",width=20,height=20)
plot.igraph(bg,vertex.shape=shapes,vertex.label.degree=pi/2,vertex.label.dist=labeldistances,vertex.color=nodecolors,vertex.label.cex=labelcexs,vertex.label.color=labelcolors)#V(bg)$type)
plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="grey70",vertex.label=V(pr$proj1)$name)
plot(pr$proj2,edge.width=E(pr$proj2)$weight^2,edge.color="grey70",vertex.label=V(pr$proj2)$name)
#plot(pr$proj2,edge.width=E(pr$proj2)$weight^2,vertex.frame.color="red",edge.color="grey70",vertex.label=V(pr$proj2)$name)
dev.off()


####################	FURTHER PLOTTING OPTIONS

igraph.options(plot.layout=layout.fruchterman.reingold(en.network,niter=10000),vertex.label.color="black")
#igraph.options(plot.layout=layout.lgl,vertex.label.color="black")
#igraph.options(plot.layout=layout.sphere(en.network),vertex.label.color="black")
#igraph.options(plot.layout=layout.kamada.kawai,vertex.label.color="black")
#igraph.options(plot.layout=layout.drl,vertex.label.color="black")
palette(rainbow(15))


#################
setwd(wd)
save.image("zerodraft_keyword_ntwk.RData")

