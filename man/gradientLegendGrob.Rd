\name{gradientLegendGrob}
\alias{gradientLegendGrob}
\alias{grid.gradientLegendGrob}
\alias{widthDetails.gradientLegendGrob}
\alias{heightDetails.gradientLegendGrob}

\title{Gradient Legend Grobs}
\description{
gradientLegendGrob creates a gradient legend grid graphics object.
}

\usage{
gradientLegendGrob<-function(
	at=seq(0,1,length=5),
	axis.min=min(at),
	axis.max=max(at),
	labels=as.character(at),
	col.fun=grey,
	delta=.01,
	vertical=F,			
	reverse=F,
	name=NULL,
	gp=NULL,
	vp=NULL,
	...) 
grid.gradientLegendGrob(...)
widthDetails.gradientLegendGrob(x)
heightDetails.gradientLegendGrob(x)
}
\arguments{
	\item{at}{where to place tick marks.}
	\item{axis.min}{To specify the axis minimun.}
	\item{axis.max}{To specify the axis maximum.}
	\item{labels}{specifies specific labels for tick marks.}
	\item{col.fun}{function to specify gradient.}
	\item{delta}{defines level of precision of gradient, smaller is a finer gradient}
	\item{vertical}{controls orientation False implies a horizontal orientation.}
	\item{reverse}{controls which side to put the axis on.  If true puts the axis on the opposite of the default which depends on the argument for vertical.}
	\item{name}{the name of the object.}
	\item{gp}{grid graphics parameters, ie. a \link[grid]{gpar} object.}
	\item{vp}{Viewport for the object.  used to embed within other objects.}
	\item{...}{other parameters passed on, mostly just ignored.}
	\item{x}{A GradientLegendGrob object}
}
\seealso{
	\code{\link{USALevelPlot}}, \code{\link{mapGrob}}, \link[=colorfunctions]{Color Functions}
}
\keyword{dplot}
\keyword{color}
