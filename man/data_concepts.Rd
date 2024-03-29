% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concept-utils.R
\name{new_cncpt}
\alias{new_cncpt}
\alias{is_cncpt}
\alias{init_cncpt}
\alias{init_cncpt.num_cncpt}
\alias{init_cncpt.unt_cncpt}
\alias{init_cncpt.fct_cncpt}
\alias{init_cncpt.cncpt}
\alias{init_cncpt.rec_cncpt}
\alias{new_concept}
\alias{concept}
\alias{is_concept}
\alias{as_concept}
\title{Data Concepts}
\usage{
new_cncpt(
  name,
  items,
  description = name,
  omopid = NA_integer_,
  category = NA_character_,
  aggregate = NULL,
  ...,
  target = "ts_tbl",
  class = "num_cncpt"
)

is_cncpt(x)

init_cncpt(x, ...)

\method{init_cncpt}{num_cncpt}(x, unit = NULL, min = NULL, max = NULL, ...)

\method{init_cncpt}{unt_cncpt}(x, unit = NULL, min = NULL, max = NULL, ...)

\method{init_cncpt}{fct_cncpt}(x, levels, ...)

\method{init_cncpt}{cncpt}(x, ...)

\method{init_cncpt}{rec_cncpt}(
  x,
  callback = paste0("rename_data_var('", x[["name"]], "')"),
  interval = NULL,
  ...
)

new_concept(x)

concept(...)

is_concept(x)

as_concept(x)
}
\arguments{
\item{name}{The name of the concept}

\item{items}{Zero or more \code{itm} objects}

\item{description}{String-valued concept description}

\item{omopid}{OMOP identifier}

\item{category}{String-valued category}

\item{aggregate}{NULL or a string denoting a function used to aggregate per
id and if applicable per time step}

\item{...}{Further specification of the \code{cncpt} object (passed to
\code{\link[=init_cncpt]{init_cncpt()}})}

\item{target}{The target object yielded by loading}

\item{class}{\code{NULL} or a string-valued sub-class name used for customizing
concept behavior}

\item{x}{Object to query/dispatch on}

\item{unit}{A string, specifying the measurement unit of the concept (can
be \code{NULL})}

\item{min, max}{Scalar valued; defines a range of plausible values for a
numeric concept}

\item{levels}{A vector of possible values a categorical concept may take on}

\item{callback}{Name of a function to be called on the returned data used
for data cleanup operations}

\item{interval}{Time interval used for data loading; if NULL, the respective
interval passed as argument to \code{\link[=load_concepts]{load_concepts()}} is taken}
}
\value{
Constructors and coercion functions return \code{cncpt} and \code{concept}
objects, while inheritance tester functions return logical flags.
}
\description{
Concept objects are used in \code{ricu} as a way to specify how a clinical
concept, such as heart rate can be loaded from a data source and are mainly
consumed by \code{\link[=load_concepts]{load_concepts()}}. Several functions are available for
constructing \code{concept} (and related auxiliary) objects either from code or
by parsing a JSON formatted concept dictionary using \code{\link[=load_dictionary]{load_dictionary()}}.
}
\details{
In order to allow for a large degree of flexibility (and extensibility),
which is much needed owing to considerable heterogeneity presented by
different data sources, several nested S3 classes are involved in
representing a concept. An outline of this hierarchy can be described as
\itemize{
\item \code{concept}: contains many \code{cncpt} objects (of potentially differing
sub-types), each comprising of some meta-data and an \code{item} object
\item \code{item}: contains many \code{itm} objects (of potentially differing
sub-types), each encoding how to retrieve a data item.
}

The design choice for wrapping a vector of \code{cncpt} objects with a container
class \code{concept} is motivated by the requirement of having several different
sub-types of \code{cncpt} objects (all inheriting from the parent type \code{cncpt}),
while retaining control over how this homogeneous w.r.t. parent type, but
heterogeneous w.r.t. sub-type vector of objects behaves in terms of S3
generic functions.

Each individual \code{cncpt} object contains the following information: a string-
valued name, an \code{\link[=new_itm]{item}} vector containing \code{\link[=new_itm]{itm}}
objects, a string-valued description (can be missing), a string-valued
category designation (can be missing), a character vector-valued
specification for an aggregation function and a target class specification
(e.g. \code{\link[=id_tbl]{id_tbl}} or \code{\link[=id_tbl]{ts_tbl}}). Additionally, a sub-
class to \code{cncpt} has to be specified, each representing a different
data-scenario and holding further class-specific information. The following
sub-classes to \code{cncpt} are available:
\itemize{
\item \code{num_cncpt}: The most widely used concept type is indented for concepts
representing numerical measurements. Additional information that can be
specified includes a string-valued unit specification, alongside a
plausible range which can be used during data loading.
\item \code{fct_cncpt}: In case of categorical concepts, such as \code{sex}, a set of
factor levels can be specified, against which the loaded data is checked.
\item \code{lgl_cncpt}: A special case of \code{fct_cncpt}, this allows only for logical
values (\code{TRUE}, \code{FALSE} and \code{NA}).
\item \code{rec_cncpt}: More involved concepts, such as a \link[=sofa_score]{SOFA score}
can pull in other concepts. Recursive concepts can build on other
recursive concepts up to arbitrary recursion depth. Owing to the more
complicated nature of such concepts, a \code{callback} function can be
specified which is used in data loading for concept-specific post-
processing steps.
\item \code{unt_cncpt}: A recent (experimental) addition which inherits from
\code{num_cncpt} but instead of manual unit conversion, leverages
}

Class instantiation is organized in the same fashion as for
\code{\link[=new_itm]{item}} objects: \code{concept()} maps vector-valued arguments
to \code{new_cncpt()}, which internally calls the S3 generic function
\code{init_cncpt()}, while \code{new_concept()} instantiates a \code{concept} object from
a list of \code{cncpt} objects (created by calls to \code{new_cncpt()}). Coercion is
only possible from \code{list} and \code{cncpt}, by calling \code{as_concept()} and
inheritance can be checked using \code{is_concept()} or \code{is_cncpt()}.
}
\examples{
if (require(mimic.demo)) {
gluc <- concept("glu",
  item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L))),
  description = "glucose", category = "chemistry",
  unit = "mg/dL", min = 0, max = 1000
)

is_concept(gluc)

identical(gluc, load_dictionary("mimic_demo", "glu"))

gl1 <- new_cncpt("glu",
  item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L))),
  description = "glucose"
)

is_cncpt(gl1)
is_concept(gl1)

conc <- concept(c("glu", "lact"),
  list(
    item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L))),
    item("mimic_demo", "labevents", "itemid", 50813L)
  ),
  description = c("glucose", "lactate")
)

conc

identical(as_concept(gl1), conc[1L])
}

}
