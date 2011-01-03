#Gradient Legends
#by Andrew Redd
#C 2007

#TODO
#1. make width Calculation
#2. make magrin calculation more presice for longer tick marks
#3. Add Valid Details function

gradientLegendGrob<-function(
	at=seq(0,1,length=5), 	#where to place tick marks 
	axis.min=min(at),		#to specify minimun
	axis.max=max(at),		#to specify maximum
	labels=as.character(at),#specify labels for tick marks
	col.fun=grey,		#function to specify gradient
	delta=.01,			#defines level of precicion of gradient 
	vertical=F,			#controls orientation
	reverse=F,			#controls which side to put the axis on
	name=NULL,
	gp=NULL,
	vp=NULL,
	...
){	
	#generate Rectangles
	if(delta<=0||delta>=1)stop("delta must be between 0 and 1.")
	if(length(at)!=length(labels))stop("labels and at must be of the same length")
	z<-(seq(0,1,by=delta)-delta/2)[-1]
	y<-if(vertical) unit(z,"npc") else unit(0.5,"npc")
	x<-if(!vertical)unit(z,"npc") else unit(0.5,"npc")
	height<-if(vertical)  delta else unit(1.5,"char")
	width	<-if(!vertical) delta else unit(1.5,"char")
	just<-if(vertical)
		if(reverse) c("right","center") else c("left","center") else
	 	if(reverse) c("center","top") else c("center","bottom") 
	cols<-col.fun(z)
	gp1<-do.call(gpar,append(list(col=NA,fill=cols),gp))
	#make Gradient
	legvp<-viewport(name="legvp",width=unit(1,"npc")-unit(1,"char"),height=unit(1,"npc")-unit(1,"char"))
	#make Gradient Border
	rectangles<-rectGrob(name="gradient",x=x,y=y,height=height,width=width,just=just,gp=gp1,vp=legvp)
	border<-if(vertical)rectGrob(name="gradBorder",x=x,width=width,just=just,vp=legvp)
			else	  rectGrob(name="gradBorder",y=y,height=height,just=just,vp=legvp)
	#Make Ticks
	ticks<-if(is.numeric(at)){
		w<- (at-axis.min)/(axis.max-axis.min)
		x0<-if(!vertical)  unit(w,"npc") else unit(.5,"npc")
		x1<-if(!vertical)  unit(w,"npc") else unit(.5,"npc")+if(reverse) unit(.4,"lines") else unit(-.4,"lines")
		y0<-if(vertical) unit(w,"npc") else unit(.5,"npc")
		y1<-if(vertical) unit(w,"npc") else unit(.5,"npc")+if(reverse) unit(.4,"lines") else unit(-.4,"lines")
		segmentsGrob(x0=x0,x1=x1,y0=y0,y1=y1,vp=legvp,name="ticks")
	} else NULL
	#Make Labels
	axis.labels<-if(all(!is.na(labels))){
		w<- (at-axis.min)/(axis.max-axis.min)
		x<-if(!vertical) unit(w,"npc") else if(reverse) unit(.5,"npc")+unit(.5,"lines") else unit(.5,"npc")-unit(.5,"lines")
		y<-if(vertical)  unit(w,"npc") else if(reverse) unit(.5,"npc")+unit(.5,"lines") else unit(.5,"npc")-unit(.5,"lines")
		just<-if(vertical) if(reverse)c("left","center") else c("right","center") else
					 if(reverse)c("center","bottom") else c("center","top")
		textGrob(labels,x=x,y=y,just=just,name="labels",vp=legvp)
	} else NULL
	gTree(at=at,axis.min=axis.min,axis.max=axis.max,col.fun=col.fun,delta=delta,vertical=vertical,reverse=reverse,
		childrenvp=legvp,
		children=gList(rectangles,border,ticks,axis.labels),
		name=name,gp=gp,vp=vp,
		cl="gradientLegendGrob")
}
grid.gradientLegendGrob<-function(...){
	grad<-gradientLegendGrob(...)
	grid.draw(grad)
	invisible(grad)
}
widthDetails.gradientLegendGrob<-function(x){
	if(x$vertical) unit(1,"null") else{
		convertUnit(unit(0.1,"lines"),"inches","x")+widthDetails(x$children[[2]])+widthDetails(x$children[[3]])+widthDetails(x$children[[4]])
	}
}
heightDetails.gradientLegendGrob<-function(x){
	if(!x$vertical) unit(1,"null") else{
		convertUnit(unit(0.1,"lines"),"inches","x")+widthDetails(x$children[[2]])+widthDetails(x$children[[3]])+widthDetails(x$children[[4]])
	}
}


#grid.gradientLegendGrob()
#grid.gradientLegendGrob(vertical=T)
#g0<-grid.gradientLegendGrob(vertical=T,reverse=T)
#grid.gradientLegendGrob(1:5,col.fun=hsv,delta=1/1000)
#rb<-function(p)rgb(p,0,1-p)
#g1<-grid.gradientLegendGrob(1:10,col.fun=rb)
#widthDetails(g1)
#widthDetails(g0)