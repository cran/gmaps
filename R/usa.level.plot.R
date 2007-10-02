USALevelPlot<-function(
	states,
	levels,
	col.fun=grey,
	alaska='alaska'%in%tolower(states),
	hawaii='hawaii'%in%tolower(states),
	normalize=TRUE,
	name=NULL,
	vp=NULL,
	gp=NULL,
	asp=1.4,
	...
){
	#validity checks
	if(!require(grid))stop("grid package is required!")
	if(!require(maps))stop("maps package is required!")
	if(!require(mapdata, quietly = TRUE))message("Higher resolution could be obtained if the mapdata package were installed.")

	#Contiguous USA
	if(!normalize && (min(levels)<0 || max(levels)>1))stop("Levels out of range, use normalize=TRUE")
	lnorm<-if(normalize) (levels-min(levels))/diff(range(levels)) else levels 
	col<-col.fun(lnorm)
	map<-mapGrob("state",regions=states,name="mapUSAContiguous",fill.col=col,gp=gp,vp=vp,asp=asp,...)
	
	#Alaska
	if(alaska ){
		if(length(grep("alaska",tolower(states)))==0)stop("alaska=TRUE option specified but Alaska data could not be found.")
		alaskavp<-viewport(x=unit(0,"npc"),y=unit(0,"npc"),width=unit(.3,"snpc"),height=unit(.3,"snpc"),
			name="alaskavp",just=c("left","bottom"))
		alaskafill<-col[grep("alaska",tolower(states))]
		alaskamap<-mapGrob(if(require(mapdata, quietly = TRUE)) "worldHires" else "world" ,"USA:Alaska",exact=F, 
			name="mapUSAAlaska",xlim=c(-178,-130.01306),ylim=c(50,72),
			fill.col=alaskafill,gp=gp,
			vp=vpStack(map$childrenvp,alaskavp),asp=1)
		map<-addGrob(map,alaskamap)
		}

	#Hawaii
	if(hawaii){
		if(length(grep("hawaii",tolower(states)))==0)stop("hawaii=TRUE option specified but Hawaii data could not be found.")
		hawaiivp<-viewport(x=unit(.3,"snpc"),y=unit(0,"snpc"),width=unit(.3,"snpc"),height=unit(.3,"snpc"),
			name="hawaiivp",just=c("left","bottom"))
		hawaiifill<-col[grep("hawaii",tolower(states))]
		hawaiimap<-mapGrob(if(require(mapdata, quietly = TRUE)) "worldHires" else "world" ,"Hawaii",exact=F, 
			name="mapUSAHawaii",xlim=c(-161.2,-154.7),ylim=c(18.9,23.2),
			fill.col=hawaiifill,gp=gp,
			vp=vpStack(map$childrenvp,hawaiivp),asp=1)
		map<-addGrob(map,hawaiimap)
		}

	#plot results
	map
}
grid.USALevelPlot<-function(...){
	map<-USALevelPlot(...)
	grid.draw(map)
	invisible(map)
}

#examples/test code
#data(state)
#p<-rbeta(50,5,2)
#map1<-grid.USALevelPlot(state.name,p,normalize=F)
#grid.newpage()
#map2<-grid.USALevelPlot(state.name,p,normalize=T,alaska=T,hawaii=T,col.fun=function(x)hsv(0,x))
#childNames(map2)
