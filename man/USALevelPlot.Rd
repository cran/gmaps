\name{USALevelPlot}
\alias{USALevelPlot}
\alias{grid.USALevelPlot}

\title{USA Level Plot}
\description{
USALevelPlot produces a mapGrob object for a map of the United States where the .
}

\usage{
USALevelPlot(
	states,
	levels,
	col.fun=grey,
	alaska='alaska'\%in\%tolower(states),
	hawaii='hawaii'\%in\%tolower(states),
	normalize=TRUE,
	name=NULL,
	vp=NULL,
	gp=NULL,
	asp=1.4,
	...)
	grid.USALevelPlot(...)
}
\arguments{
	\item{states}{List of the full names of the states.}
	\item{levels}{The response variable to use to color code the states}
	\item{col.fun}{A color function that takes the response variable and returns a color code.}
	\item{alaska}{include an inset of alaska?}
	\item{hawaii}{include an inset of Hawaii?}
	\item{normalize}{Does the response variable need to be transformed to between 0 and 1.  If False and levels is outside of [0,1]  may cause errors depending on the col.fun.}
	\item{name}{the name of the object.}
	\item{vp}{Viewport for the object.  used to embed within other objects.}
	\item{gp}{grid graphics parameters, ie. a \link[grid]{gpar} object.}
	\item{asp}{aspect ratio for the map.}
	\item{...}{other parameters passed on.}
}
\seealso{
	\link[=colorfunctions]{Color Functions}, \code{\link{mapGrob}}, \code{\link[maps]{map}}
}
\examples{
#maps of Arrests by state for arrests per 100,000 for Murder
data(USArrests)
attach(USArrests)
grid.newpage()
grid.frame(name="map")
grid.pack("map",USALevelPlot(states=rownames(USArrests),levels=Murder,col.fun=reds),height=unit(1,'null'))
grid.pack("map",gradientLegendGrob(at=quantile(Murder),col.fun=reds),side="bottom",height=unit(.2,'npc'))
detach(USArrests)
}
\keyword{hplot}