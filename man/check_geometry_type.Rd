% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_geometry_type}
\alias{check_geometry_type}
\title{Check Geometry Type of an sf Object}
\usage{
check_geometry_type(sf_object, geom_category)
}
\arguments{
\item{sf_object}{An 'sf' object, which represents spatial features.}

\item{geom_category}{A string indicating the expected geometry category.
Accepts 'line' for LINESTRING and MULTILINESTRING geometries, or 'point'
for POINT and MULTIPOINT geometries.}
}
\value{
Does not return a value but halts execution with an error message if
the 'sf_object''s geometry does not match the expected category.
}
\description{
Validates whether the geometry of an 'sf' object matches the specified
category. This function is designed to ensure that spatial operations are
performed on the appropriate geometry types, specifically 'line' and 'point'
geometries.
}
\keyword{internal}
