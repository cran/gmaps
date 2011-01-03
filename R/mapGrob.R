#mapGrob functions
#Andrew Redd
#Version 0.0.2

#TODO
#


#support function for helping grid graphics
convert.coordinates.for.grid<-function(cod,regions=".",...){
	x=cod$x
	y=cod$y
	id<-numeric(0)
	id.marker<-1
	for(i in 1:length(x)){
		if (is.na(x[i])){
			id[i]<-id.marker
			id.marker<-id.marker+1
		}else{
			id[i]<-id.marker
		}
	}
	region.id<-rep(NA,length(id))
	if(identical(regions,".")){
		region.id<-id
		region.names<-cod$names
	} else if(length(regions)==1){
		region.names<-regions
		region.id<-rep(1,length(id))
	} else {
		region.names<-regions
		regions<-tolower(regions)
		region.map<-1:length(regions)
		names(region.map)<-regions
		for(r in regions){
			name.matches<-grep(r,cod$names)
			region.id[id %in% name.matches]<-region.map[r]
		}
	}
	
	list(x=x,y=y,range=cod$range,names=cod$names,id=id,region.id=region.id,region.names=region.names)
}

makeMapViewports<-function(coord,asp=1){
	xrange<-coord$range[1:2]
	yrange<-coord$range[3:4]
	vpStack(
		viewport(name="maplayout",layout=grid.layout(1,1,
			widths=diff(xrange),
			heights=diff(yrange)*asp,
			respect=TRUE)),
		viewport(name="mapvp",layout.pos.row=1,layout.pos.col=1,
			xscale=xrange,
			yscale=yrange))
}
makeMapPolygons<-function(coord,fill.col=NULL,gp=NULL){
	n<-length(coord$region.names)
	if(!is.null(fill.col))fill.cols<-rep(fill.col,n)
	polygons<-vector("list",n)
	for(i in 1:n){
		polygons[[i]]<-if(any(coord$region.id==i))polygonGrob(
			name=paste("mappolygon",coord$region.names[i],sep=":"),
			unit(coord$x[coord$region.id==i],"native"),
			unit(coord$y[coord$region.id==i],"native"),
			gp=if(is.null(fill.col)) gp else gpar(fill=fill.col[i],gp),
			vp=vpPath("maplayout","mapvp")) else NULL
	}
	do.call("gList",polygons)
}
mapGrob<-function(
	database = "world", 
	regions=".",
	exact=F,
	xlim=NULL,
	ylim=NULL,
	name=NULL,
	fill.col=NA,#superceeds gp$fill if specified
	gp=NULL,
	vp=NULL,
	asp=1,
	...) 
{
	#validity checks
	if(!require(maps))stop("maps package is required")
	if(!require(grid))stop("grid graphics is required")

	#Get Coordinates for plotting regions
	c1<-map(database=database,regions=regions,exact=exact,fill=T,xlim=xlim,ylim=ylim,plot=F,...)
	coord<-convert.coordinates.for.grid(c1, regions)
    
	# Aspect Ratio 
	if(missing(asp)){
		xrange <- range(coord$x, na.rm = TRUE)  
		yrange <- range(coord$y, na.rm = TRUE)
    aspect <- 1/cos((mean(yrange) * pi)/180)
	} else aspect<-asp
	#Grob generation
	#mapvp<-viewport(height=unit(1,"npc"),width=unit(1,"npc"),xscale=coord$range[1:2],yscale=coord$range[3:4],name="map")
	#map<-gTree(,vp=mapvp)
	mapvp<-makeMapViewports(coord,asp=aspect)
	mappolys<-makeMapPolygons(coord,fill.col=fill.col,gp=gp)
	gTree(coord=coord,name=name,gp=gp,vp=vp,
		childrenvp=mapvp,
		children=mappolys,
		cl="mapGrob")
}

grid.mapGrob<-function(...){
	map<-mapGrob(...)
	grid.draw(map)
	invisible(map)
}
validDetails.mapGrob<-function(x){
	x
}


