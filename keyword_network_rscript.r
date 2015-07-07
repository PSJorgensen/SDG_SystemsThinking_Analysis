#####################		NETWORK ANALYSIS OF KEYWORDS IN 169 SUSTAINABLE DEVELOPMENT GOAL TARGETS		############################
# 217 challenges
# 64 subjects
# 58 instruments


### 150220 - starting work on secondpass keyword mapping

#install.packages("reshape")
#install.packages("igraph")
library(reshape);library(igraph);library(RColorBrewer)
	


	
wd<-"D:/ScienceProjects/SDG_SystemsThinking_Analysis-master/SDG_SystemsThinking_Analysis-master"
fig.wd<-"D:/ScienceProjects/SDG_SystemsThinking_Analysis-master/SDG_SystemsThinking_Analysis-master/Rfigures"

workspace<-"zerodraft_keyword_ntwk.RData"

setwd(wd)
#load(workspace)


	### analysis type
	analysis.comb<-"HumanSubject" #"HumanSubject","""Challenge","Subject","ChallengeSubjectInstrument"
	#analysis.comb<-"ChallengeSubjectInstrument"
	vertices.is<-"Goals" # "Targets","Goals"

	setwd(wd)
	dframe<-read.csv("Prelim_mapping_key_150220d_oneColumn.csv",header=TRUE,stringsAsFactors =TRUE,na.strings="NA")


	{

	if(analysis.comb == "ChallengeSubjectInstrument"){
	  
	dframe.sub <- dframe[ which( dframe[ ,"Type"] %in% c("Challenge","Subject","Instrument")), ]
		
	}


	if(analysis.comb %in% c("Challenge","Subject")) {
	  
	dframe.sub <- dframe[ which( dframe[ ,"Type"] == analysis.comb ), ]
		
	}

	if(analysis.comb %in% c("HumanSubject")) {
	  
	dframe.sub <- dframe[ which( dframe[ ,"Type"] == "Subject" &  is.na(dframe[ ,"NonHumanSubject"])==TRUE), ]
		
	}


	#dframe.melt.sub<-dframe.melt[which(dframe.melt[,"type"]%in%sub.type.vec),]
	#dframe.melt.sub<-dframe.melt.sub[complete.cases(dframe.melt.sub),]

	dframe.sub<-dframe.sub[which(duplicated(dframe.sub[,c("Keyword","Targets")])==FALSE),]

	###############			CREATE MATRIX - GOALS AS COLUMNS - VALUES AS ROWS


	if(vertices.is!="Targets"){
	comb.mat<-matrix(0,nrow=length(unique(dframe.sub[,"Keyword"])),ncol=length(unique(dframe.sub[,"Goal"])))
	colnames(comb.mat)<-unique(dframe.sub[,"Goal"])
	rownames(comb.mat)<-unique(dframe.sub[,"Keyword"])
	comb.df<-as.data.frame(comb.mat)

	for(i in 1:ncol(comb.df)){
	dframe.sub.i<-dframe.sub[which(dframe.sub[,"Goal"]==names(comb.df)[i]),]
	comb.df[sort(unique(match(dframe.sub.i[,"Keyword"],rownames(comb.mat)))),i]<-c(t(table(match(dframe.sub.i[,"Keyword"],rownames(comb.mat)))))
	}

	}

	if(vertices.is=="Targets"){

	comb.mat<-matrix(0,nrow=length(unique(dframe.sub[,"Keyword"])),ncol=length(unique(dframe.sub[,"Targets"])))
	colnames(comb.mat)<-unique(dframe.sub[,"Targets"])
	rownames(comb.mat)<-unique(dframe.sub[,"Keyword"])
	comb.df<-as.data.frame(comb.mat)

	for(i in 1:ncol(comb.df)){
	dframe.sub.i<-dframe.sub[which(dframe.sub[,"Targets"]==names(comb.df)[i]),]
	comb.df[sort(unique(match(dframe.sub.i[,"Keyword"],rownames(comb.mat)))),i]<-c(t(table(match(dframe.sub.i[,"Keyword"],rownames(comb.mat)))))
	}

	}


	#which(rowSums(comb.df)>1)
	#which(colSums(comb.df)>1)
	#comb.df[which(rowSums(comb.df>2)),]


	###############			PERFORM NETWORK ANALYSIS	

	bg=graph.incidence(as.matrix(comb.df))

	shapes=c(rep("circle",nrow(comb.df)),rep("square",ncol(comb.df)))
	labeldistances=c(rep(0,nrow(comb.df)),rep(0,ncol(comb.df)))
	nodecolors=c(rep("white",nrow(comb.df)),rep("grey50",ncol(comb.df)))
	labelcexs=c(rep(0.5,nrow(comb.df)),rep(2,ncol(comb.df)))
	labelcolors=c(rep("grey50",nrow(comb.df)),rep("black",ncol(comb.df)))


	pr=bipartite.projection(bg)
	#get.adjacency(pr$proj1,sparse=FALSE,attr="weight") ### get the matrix







	if(vertices.is=="Targets"){
	figure.width <-12
	figure.height <-8
	}

	if(vertices.is!="Targets"){
	figure.width <-12
	figure.height <-8
	}

	setwd(fig.wd)

	pdf(paste(vertices.is,"_",analysis.comb,"_",Sys.Date(),".pdf",sep=""),width=figure.width,height=figure.height)

	if(vertices.is!="Targets"){

	g<-pr$proj2
	com <- walktrap.community(g)$membership
	g <- set.graph.attribute(g, "layout", layout.fruchterman.reingold(g,niter=10000))

	#plot.igraph(bg,vertex.shape=shapes,vertex.label.degree=pi/2,vertex.label.dist=labeldistances,vertex.color=nodecolors,vertex.label.cex=labelcexs,vertex.label.color=labelcolors)#V(bg)$type)
	#plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="grey70",vertex.label=V(pr$proj1)$name)
	plot(g,edge.width=(E(pr$proj2)$weight^2)/5,
	edge.color="grey70",vertex.label=V(pr$proj2)$name)
	#plot(pr$proj2,edge.width=E(pr$proj2)$weight^2,vertex.frame.color="red",edge.color="grey70",vertex.label=V(pr$proj2)$name)

	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),

		######################################



	plot(g,edge.width=(E(g)$weight^2)/5,vertex.color=1:16,
	edge.color="grey70",vertex.label.dist=0,vertex.label.color="black",
	main="Qualitative color by goal"
	)
	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),


		
	#### A PRIORI GROUPS -  SOCIAL(3,4,5,16) - ECON(1,8,9,10) - RES(2,7,11,12) - ENV/RESOURCE(6,13,14,15)
	### GOALS AS LABELS - DIVERGING GOALS AS COLORS
	palette(brewer.pal(4, "RdBu"))

	goal.color.df<-data.frame("Goal"=1:16,"Color"=c(2,3,1,1,1,4,3,2,2,2,3,3,4,4,4,1))
	vertex.color.df<-data.frame("Seq"=1:length(V(g)$name),"Goal"=V(g)$name)
	vertex.color.df<-merge(vertex.color.df,goal.color.df,by="Goal",all.x=TRUE)
	#vertex.color.df<-vertex.color.df[apply(vertex.color.df,1,function(x,y=x["Target"],z=V(g)$name) match(y,z)),]
	vertex.color.df<-vertex.color.df[order(vertex.color.df$Seq),]

	plot(g,edge.width=(E(g)$weight^2)/5,vertex.color=vertex.color.df$Color,
	edge.color="grey70",vertex.label=V(g)$name,vertex.label.dist=0,vertex.label.color="black",
	main="Color by goal group"
	)

	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),
		
	
	### GOALS AS LABELS - DIVERGING GOALS AS COLORS
	palette(rep(brewer.pal(8, "RdBu"),each=2))

	plot(g,edge.width=(E(g)$weight^2)/5,vertex.color=V(g),
	edge.color="grey70",vertex.label=V(g)$name,vertex.label.dist=0,vertex.label.color="black",
	main="Color by goal number"
	)

	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),
	
	### COMMUNITY COLORING - GOALS AS LABELS
	palette(brewer.pal(12, "Paired"))

	plot(g,edge.width=(E(g)$weight^2)/5,vertex.color=walktrap.community(g)$membership,#edge.betweenness.community(g)$membership,
	edge.color="grey70",vertex.label=V(g)$name,vertex.label.dist=0,vertex.label.color="black",vertex.label.cex=0.7,
	main="Color by walktrap sub-network"
	)

	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),

	}


	if(vertices.is=="Targets"){

	### SETTING THE COLOR PALETTE
	#palette(rainbow(length(unique(walktrap.community(pr$proj2)$membership)),start=0.6,end=1/8))
	#palette(heat.colors(length(unique(walktrap.community(pr$proj2)$membership))))


	### GOALS AS LABELS - QUALITATIVE GOALS AS COLORS
	palette(brewer.pal(length(unique(do.call(rbind,strsplit(V(pr$proj2)$name,"_"))[,1]))
	, "Paired"))

	g<-pr$proj2
	com <- walktrap.community(g)$membership
	g <- set.graph.attribute(g, "layout", layout.fruchterman.reingold(g,niter=10000))


	plot(g,vertex.size=5.5,edge.width=(E(g)$weight^2)/10,vertex.color=do.call(rbind,strsplit(V(g)$name,"_"))[,1],
	edge.color="grey70",vertex.label=do.call(rbind,strsplit(V(g)$name,"_"))[,1],vertex.label.dist=0,vertex.label.color="black",vertex.label.cex=0.7,
	main="Qualitative color by goal"
	)
	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),


	#### A PRIORI GROUPS -  SOCIAL(3,4,5,16) - ECON(1,8,9,10) - RES(2,7,11,12) - ENV/RESOURCE(6,13,14,15)
	### GOALS AS LABELS - DIVERGING GOALS AS COLORS
	palette(brewer.pal(4, "RdBu"))

	goal.color.df<-data.frame("Goal"=1:16,"Color"=c(2,3,1,1,1,4,3,2,2,2,3,3,4,4,4,1))
	vertex.color.df<-data.frame("Seq"=1:length(V(g)$name),"Goal"=do.call(rbind,strsplit(V(g)$name,"_"))[,1],"Target"=V(g)$name)
	vertex.color.df<-merge(vertex.color.df,goal.color.df,by="Goal",all.x=TRUE)
	#vertex.color.df<-vertex.color.df[apply(vertex.color.df,1,function(x,y=x["Target"],z=V(g)$name) match(y,z)),]
	vertex.color.df<-vertex.color.df[order(vertex.color.df$Seq),]

	plot(g,vertex.size=5.5,edge.width=(E(g)$weight^2)/10,vertex.color=vertex.color.df$Color,
	edge.color="grey70",vertex.label=do.call(rbind,strsplit(V(g)$name,"_"))[,1],vertex.label.dist=0,vertex.label.color="black",vertex.label.cex=0.7,
	main="Color by goal group"
	)

	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),
		
		
	### GOALS AS LABELS - DIVERGING GOALS AS COLORS
	palette(rep(brewer.pal(8, "RdBu"),each=2))

	plot(g,vertex.size=5.5,edge.width=(E(g)$weight^2)/10,vertex.color=do.call(rbind,strsplit(V(g)$name,"_"))[,1],
	edge.color="grey70",vertex.label=do.call(rbind,strsplit(V(g)$name,"_"))[,1],vertex.label.dist=0,vertex.label.color="black",vertex.label.cex=0.7,
	main="Sequential color by goal"
	)
	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),

	### COMMUNITY COLORING - GOALS AS LABELS
	palette(brewer.pal(12, "Paired"))

	plot(g,vertex.size=5.5,edge.width=(E(g)$weight^2)/10,vertex.color=walktrap.community(g)$membership,
	edge.color="grey70",vertex.label=do.call(rbind,strsplit(V(g)$name,"_"))[,1],vertex.label.dist=0,vertex.label.color="black",vertex.label.cex=0.7,
	main="Color by sub-network"
	)
	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),

		
	### COMMUNITY COLORING - TARGETS AS LABELS
	palette(brewer.pal(12, "Paired"))

	plot(g,vertex.size=5.5,edge.width=(E(g)$weight^2)/10,vertex.color=walktrap.community(g)$membership,	#edge.betweenness.community(g)$membership,
	edge.color="grey70",vertex.label=gsub("_",".",V(g)$name,fixed=TRUE),vertex.label.dist=0,vertex.label.color="black",vertex.label.cex=0.5,
	main="Color by sub-network"
	)
	label.mat<-as.matrix(unique(dframe.sub[,c("Goal","GoalTopic")],bycol=TRUE))
	legend("topleft",
		,paste(label.mat[,1],". ",label.mat[,2],sep=""),
		ncol=1,bty="n")
		#c(unique(do.call(rbind,strsplit(V(g)$name,"_"))[,1]),unique(dframe.sub[,"GoalTopic"])),
		
		
	}

	####################	FURTHER PLOTTING OPTIONS

	#igraph.options(plot.layout=layout.fruchterman.reingold(en.network,niter=10000),vertex.label.color="black")
	#igraph.options(plot.layout=layout.lgl,vertex.label.color="black")
	#igraph.options(plot.layout=layout.sphere(en.network),vertex.label.color="black")
	#igraph.options(plot.layout=layout.kamada.kawai,vertex.label.color="black")
	#igraph.options(plot.layout=layout.drl,vertex.label.color="black")
	#palette(rainbow(15))

	par(mfrow=c(1,1),mar=c(4,8,4,4),oma=c(0,0,0,0))

	####################  SUMMARY SCORES 
	if(vertices.is=="Goals"){

	####		METRICS BASED ON ADJACENCY MATRIX
	g.adj.df<-as.data.frame(as.matrix(get.adjacency(g)))
	g.adj.df[,c("Goal")]<-NA

	g.adj.df[,c("Goal")]<-rownames(g.adj.df)

	goal.df<-data.frame("Goal"=V(g)$name,"Edges.Nb"=NA)

	goal.df[,"Edges.Nb"] <- c(rowSums(as.matrix(get.adjacency(g))))


	keyword.df<-data.frame("keyword"=rownames(comb.df),
		"NumberOfGoals"=apply(comb.df,1,function(x,y=names(comb.df)) length(unique(c(as.numeric(y)*x)))-1)
		)

	###	PLOT KEYWORD NUMBER GOALS AND TARGETS
	keyword.df<-keyword.df[order(keyword.df[,"NumberOfGoals"],decreasing=FALSE),]
	x<-barplot(keyword.df[which(keyword.df[,"NumberOfGoals"]>3),"NumberOfGoals"],yaxt="n",main= "NUMBER OF KEYWORD GOALS",horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- keyword.df[which(keyword.df[,"NumberOfGoals"]>3),"keyword"]
	text(cex=1, y=x-.25, x=-0.3, labs, adj=1,xpd=TRUE, srt=0)


	
	}
	if(vertices.is=="Targets"){


	####		METRICS BASED ON ADJACENCY MATRIX
	g.adj.df<-as.data.frame(as.matrix(get.adjacency(g)))
	g.adj.df[,c("Target","Goal")]<-NA

	g.adj.df[,c("Goal")]<-do.call(rbind,strsplit(rownames(g.adj.df),"_"))[,1]
	g.adj.df[,c("Target")]<-rownames(g.adj.df)

	target.df<-data.frame("target"=V(g)$name,"Edges.Nb"=NA,"Edges.TransGoalNb"=NA,"Edges.NumberOfGoals"=NA,"Edges.OwnGoalNb"=NA)

	target.df[,"Edges.Nb"] <- c(rowSums(as.matrix(get.adjacency(g))))
	target.df[,"Edges.NumberOfGoals"] <- apply(as.data.frame(as.matrix(get.adjacency(g))),1,function(x) length(unique(do.call(rbind,strsplit(names(which(x==1)),"_"))[,1])))
	target.df[,"Edges.OwnGoalNb"] <- apply(g.adj.df,1,function(x,y=x["Goal"],z=x["Target"]) length(grep(y,do.call(rbind,strsplit(names(which(x==1)),"_"))[,1])))
	target.df[,"Edges.TransGoalNb"] <- target.df[,"Edges.Nb"]-target.df[,"Edges.OwnGoalNb"]
	target.df[,"Edges.TransGoalProp"] <- target.df[,"Edges.TransGoalNb"]/target.df[,"Edges.Nb"]

	####		METRICS BASED ON KEYWORD INCIDENCES

	comb.df.target<-comb.df
	comb.df.goal<-comb.df
	names(comb.df.goal)<-do.call(rbind,strsplit(names(comb.df.goal),"_"))[,1]

	keyword.df<-data.frame("keyword"=rownames(comb.df.goal),
		"NumberOfGoals"=apply(comb.df.goal,1,function(x,y=names(comb.df.goal)) length(unique(c(as.numeric(y)*x)))-1),
		"NumberOfTargets"=apply(comb.df.target,1,function(x,y=names(comb.df.target)) length(which(x>0)))
		)


	###	SETTING BARPLOT DIMENSIONS


	if(vertices.is == "Targets"){par(mfrow=c(1,2),mar=c(4,14,2,4))}

	###	PLOT KEYWORD NUMBER GOALS AND TARGETS
	keyword.df<-keyword.df[order(keyword.df[,"NumberOfGoals"],decreasing=FALSE),]
	x<-barplot(keyword.df[which(keyword.df[,"NumberOfGoals"]>3),"NumberOfGoals"],yaxt="n",main= "NUMBER OF KEYWORD GOALS",horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- keyword.df[which(keyword.df[,"NumberOfGoals"]>3),"keyword"]
	text(cex=1, y=x-.25, x=-0.3, labs, adj=1,xpd=TRUE, srt=0)

	###	PLOT KEYWORD NUMBER GOALS AND TARGETSX
	keyword.df<-keyword.df[order(keyword.df[,"NumberOfTargets"],decreasing=FALSE),]
	x<-barplot(keyword.df[which(keyword.df[,"NumberOfTargets"]>5),"NumberOfTargets"],yaxt="n",main= "NUMBER OF KEYWORD TARGETS",horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- keyword.df[which(keyword.df[,"NumberOfTargets"]>5),"keyword"]
	text(cex=1, y=x-0.25, x=-0.3, labs, adj=1,xpd=TRUE, srt=0)
}


	#if(vertices.is != "Targets"){par(mfrow=c(2,2),mar=c(14,2,2,2))}
	#if(vertices.is == "Targets"){par(mfrow=c(1,1),mar=c(14,2,2,2))}

	#### CENTRALITY

	if(vertices.is == "Targets"){par(mfrow=c(1,1),mar=c(4,4,4,4))


	###	PLOT NUMBER OF EDGES
	target.df<-target.df[order(target.df[,"Edges.Nb"],decreasing=TRUE),]
	x<-barplot(target.df[30:1,"Edges.Nb"],yaxt="n",main= "NUMBER OF EDGES",horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- target.df[30:1,"target"]
	text(cex=1, y=x-.25, x=-0.01, labs, adj=1,xpd=TRUE, srt=0)

	

	## authority centrality
	authority.vec.vals<-sort(authority.score (pr$proj2, scale = TRUE, weights=NULL, options = igraph.arpack.default)$vector,decreasing=FALSE)
	authority.title<-"Kleinberg's centrality scores"

	x<-barplot(authority.vec.vals[(length(authority.vec.vals)-30):length(authority.vec.vals)],yaxt="n",main= paste(authority.title,sep=""),horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- paste(names(authority.vec.vals))[(length(authority.vec.vals)-30):length(authority.vec.vals)]
	text(cex=1, y=x-.25, x=-0.01, labs, adj=1,xpd=TRUE, srt=0)


	#vertex betweenness
	betweenness.vals<-sort(betweenness(pr$proj2, v=V(pr$proj2), directed = TRUE, weights = NULL,
									   nobigint = TRUE, normalized = FALSE),decreasing=FALSE)

	betweenness.title<-"Betweenness centrality scores"
	x<-barplot(betweenness.vals[(length(betweenness.vals)-30):length(betweenness.vals)],yaxt="n",
		main= paste(betweenness.title,sep=""),horiz=TRUE)
	labs <- paste(names(betweenness.vals))[(length(betweenness.vals)-30):length(betweenness.vals)]
	text(cex=1, y=x-.25, x=-0.01, labs, adj=1,xpd=TRUE, srt=0)


	#Eigenvector centrality 
	evcent.vals<-sort(evcent(pr$proj2, directed = FALSE, scale = TRUE, weights = NULL,
							 options = igraph.arpack.default)$vector,decreasing=TRUE) 
	evcent.title<-"Eigenvector Centrality"

	x<-barplot(evcent.vals[30:1],yaxt="n",main= paste(evcent.title,sep=""),horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- paste(names(evcent.vals))[30:1]
	text(cex=1, y=x-.25, x=-0.01, labs, adj=1,xpd=TRUE, srt=0)


	if(vertices.is == "Targets"){par(mfrow=c(1,2),mar=c(4,4,2,4))

	###	PLOT PROPORTION OF TRANSGOAL EDGES
	target.df<-target.df[order(target.df[,"Edges.TransGoalProp"],decreasing=TRUE),]
	x<-barplot(target.df[30:1,"Edges.TransGoalProp"],yaxt="n",main= "PROPORTION OF TRANSGOAL EDGES",horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- target.df[30:1,"target"]
	text(cex=1, y=x-.25, x=-0.01, labs, adj=1,xpd=TRUE, srt=0)

	###	PLOT NUMBER OF TRANSGOAL EDGES
	target.df<-target.df[order(target.df[,"Edges.TransGoalNb"],decreasing=TRUE),]
	x<-barplot(target.df[30:1,"Edges.TransGoalNb"],yaxt="n",main= "NUMBER OF TRANSGOAL EDGES",horiz=TRUE)
	#barplot(authority.vec.vals,xaxt="n",main=paste(authority.title))
	labs <- target.df[30:1,"target"]
	text(cex=1, y=x-.25, x=-0.5, labs, adj=1,xpd=TRUE, srt=0)

	}
	}
	
	}



	dev.off()


	### end of figure       


#################
setwd(wd)
save.image("zerodraft_keyword_ntwk.RData")


#######################			MORE CENTRALITY SCORES




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







# plotting a simple ring graph, all default parameters, except the layout
g <- graph.ring(10)
g$layout <- layout.circle
plot(g)
tkplot(g)
rglplot(g)

# plotting a random graph, set the parameters in the command arguments
g <- barabasi.game(100)
plot(g, layout=layout.fruchterman.reingold, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

# plot a random graph, different color for each component
g <- erdos.renyi.game(100, 1/100)
comps <- clusters(g)$membership
colbar <- rainbow(max(comps)+1)
V(g)$color <- colbar[comps+1]
plot(g, layout=layout.fruchterman.reingold, vertex.size=5, vertex.label=NA)

# plot communities in a graph
g <- graph.full(5) %du% graph.full(5) %du% graph.full(5)
g <- add.edges(g, c(1,6, 1,11, 6,11))
com <- spinglass.community(g, spins=5)
V(g)$color <- com$membership+1
g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g))
plot(g, vertex.label.dist=1.5)

# draw a bunch of trees, fix layout
igraph.options(plot.layout=layout.reingold.tilford)
plot(graph.tree(20, 2))
plot(graph.tree(50, 3), vertex.size=3, vertex.label=NA)
tkplot(graph.tree(50, 2, mode="undirected"), vertex.size=10,
vertex.color="green")

