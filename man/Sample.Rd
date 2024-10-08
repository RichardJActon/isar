% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sample.R
\name{Sample}
\alias{Sample}
\title{R6 class for sample}
\description{
Represents a sample material in an experimental graph.
}
\examples{

## ------------------------------------------------
## Method `Sample$to_list`
## ------------------------------------------------

Sample$new()
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{A name/reference for the sample material.}

\item{\code{factor_values}}{A list of \link{FactorValue}s used to qualify the material in terms of study factors/design.}

\item{\code{study_factor_references}}{a \link{StudyFactorReferences} object}

\item{\code{characteristics}}{A list of Characteristics used to qualify the material properties.}

\item{\code{derives_from}}{A link to the source material that the sample is derived from.}

\item{\code{comments}}{Comments associated with instances of this class.}

\item{\code{ontology_source_references}}{an \link{OntologySourceReferences} object}

\item{\code{category_references}}{an \link{CharacteristicCategoryReferences} object}

\item{\code{unit_references}}{an \link{UnitReferences} object}

\item{\code{sources}}{list of available \link{Source}s}

\item{\code{@id}}{identifier}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Sample-new}{\code{Sample$new()}}
\item \href{#method-Sample-check_factor_values}{\code{Sample$check_factor_values()}}
\item \href{#method-Sample-set_factor_values}{\code{Sample$set_factor_values()}}
\item \href{#method-Sample-to_list}{\code{Sample$to_list()}}
\item \href{#method-Sample-to_table}{\code{Sample$to_table()}}
\item \href{#method-Sample-check_source}{\code{Sample$check_source()}}
\item \href{#method-Sample-set_source}{\code{Sample$set_source()}}
\item \href{#method-Sample-update_characteristics}{\code{Sample$update_characteristics()}}
\item \href{#method-Sample-from_list}{\code{Sample$from_list()}}
\item \href{#method-Sample-print}{\code{Sample$print()}}
\item \href{#method-Sample-clone}{\code{Sample$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-new"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$new(
  name = character(),
  factor_values = NULL,
  study_factor_references = NULL,
  characteristics = NULL,
  derives_from = NULL,
  comments = NULL,
  ontology_source_references = NULL,
  category_references = NULL,
  unit_references = NULL,
  sources = NULL,
  `@id` = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{A name/reference for the sample material.}

\item{\code{factor_values}}{A list of \link{FactorValue} objects used to qualify the material in terms of study factors/design.}

\item{\code{study_factor_references}}{a \link{StudyFactorReferences} object}

\item{\code{characteristics}}{A list of Characteristics used to qualify the material properties.}

\item{\code{derives_from}}{A link to the source material that the sample is derived from.}

\item{\code{comments}}{Comments associated with instances of this class.}

\item{\code{ontology_source_references}}{an \link{OntologySourceReferences} object}

\item{\code{category_references}}{an \link{CharacteristicCategoryReferences} object}

\item{\code{unit_references}}{an \link{UnitReferences} object}

\item{\code{sources}}{list of available \link{Source}s}

\item{\code{@id}}{identifier}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Create a new instance of sample
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-check_factor_values"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-check_factor_values}{}}}
\subsection{Method \code{check_factor_values()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$check_factor_values(factor_values)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{factor_values}}{factor values to be used in a \link{Sample} object
A list of \link{FactorValue} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
validates the factor_values field is a list of \link{FactorValue} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-set_factor_values"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-set_factor_values}{}}}
\subsection{Method \code{set_factor_values()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$set_factor_values(factor_values)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{factor_values}}{factor values used in the sample
A list of \link{FactorValue} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Sets the factor values used in the sample
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-to_list"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-to_list}{}}}
\subsection{Method \code{to_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$to_list(ld = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ld}}{logical json-ld}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
generate an R list representation translatable to JSON
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{Sample$new()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-to_table"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-to_table}{}}}
\subsection{Method \code{to_table()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$to_table()}\if{html}{\out{</div>}}
}

\subsection{Details}{
generate a tabular representation of a sample object
}

\subsection{Returns}{
a Tibble
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-check_source"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-check_source}{}}}
\subsection{Method \code{check_source()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$check_source(source)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{source}}{The name of a source to check is listed}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Checks that source is listed in the provided list of sources
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-set_source"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-set_source}{}}}
\subsection{Method \code{set_source()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$set_source(source)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{source}}{The name of a source to set is listed}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Sets a source if that source is listed in the provided list of sources
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-update_characteristics"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-update_characteristics}{}}}
\subsection{Method \code{update_characteristics()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$update_characteristics()}\if{html}{\out{</div>}}
}

\subsection{Details}{
update characteristic categories with any new categories added to
the reference.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-from_list"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-from_list}{}}}
\subsection{Method \code{from_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$from_list(lst, recursive = TRUE, json = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lst}}{a list serialization of a \link{Sample} factor object}

\item{\code{recursive}}{call to_list methods of any objects within this object (default TRUE)}

\item{\code{json}}{json  (default TRUE)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Make \link{sample} from list
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-print"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$print()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Pretty Prints \link{Sample} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sample-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Sample-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sample$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
