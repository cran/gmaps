\name{mapGrob}
\alias{mapGrob}
\alias{grid.mapGrob}
\alias{validDetails.mapGrob}


\title{Map grid graphics objects}
\description{
mapGrob is the representation of a map object in the grid graphics system.
}

\usage{
mapGrob(
	database = "world", 
	regions=".",
	exact=F,
	xlim=NULL,
	ylim=NULL,
	name=NULL,
	fill.col=NA,
	gp=NULL,
	vp=NULL,
	asp=1,
	...) 
grid.mapGrob(...)
validDetails.mapGrob(x)
}
\arguments{
	\item{database}{Any of the valid maps databases.}
	\item{regions}{A vector of the regions to be included in the map.}
	\item{exact}{A logical specifying if regions should be matched exactly or partially.}
	\item{xlim}{x range on the map objecct, ussually correspondes to longitude.}
	\item{ylim}{y range on the map objecct, ussually correspondes to latitude.}
	\item{name}{the name of the object.}
	\item{fill.col}{color to use to fill the map with. Superceeds \code{gp\$fill} if specified.}
	\item{gp}{grid graphics parameters, ie. a \link[grid]{gpar} object.}
	\item{vp}{Viewport for the object.  used to embed within other objects.}
	\item{asp}{aspect ratio for the map.}
	\item{...}{other parameters passed onto the map function.}
	\item{x}{A mapGrob object}
}
\seealso{
	\code{\link[grid]{grid}}, \code{\link[maps]{maps}}
}
\examples{
#The four corners area of the United States colored.
grid.mapGrob("state",c("utah","new mexico","arizona","colorado"),fill.col=rainbow(4))
}
\keyword{dplot}