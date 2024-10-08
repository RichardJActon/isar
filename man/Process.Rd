% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Process.R
\name{Process}
\alias{Process}
\title{R6 object for Process}
\description{
R6 object for Process

R6 object for Process
}
\details{
Process nodes represent the application of a protocol to some input material
(e.g. a \link{Source}) to produce some output (e.g.a \link{Sample}).
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{If relevant, a unique name for the process to disambiguate it from other processes.}

\item{\code{executes_protocol}}{A reference to the \link{Protocol} that this process executes.}

\item{\code{date}}{A date formatted as an ISO8601 string corresponding to when the process event occurred.}

\item{\code{performer}}{The name of the person or organisation that carried out the process.}

\item{\code{parameter_values}}{A list of \link{ParameterValue}s relevant to the executing protocol.}

\item{\code{inputs}}{A list of input materials, possibly \link{Source}s, \link{Sample}s, \link{Material}s, \link{DataFile}s}

\item{\code{outputs}}{A list of output materials, possibly \link{Sample}s, \link{Material}s, \link{DataFile}s}

\item{\code{comments}}{Comments associated with instances of this class.}

\item{\code{@id}}{identifier}

\item{\code{protocols}}{list of available \link{Protocol}s}

\item{\code{sources}}{list of available \link{Source}s}

\item{\code{samples}}{list of available \link{Sample}s}

\item{\code{next_process}}{id of the next process in the sequence}

\item{\code{previous_process}}{id of the previous process in the sequence}

\item{\code{materials}}{a list of \link{Material} objects}

\item{\code{data_files}}{as list of \link{DataFile} objects}

\item{\code{ontology_source_references}}{ontology_source_references \link{OntologySource}s to be referenced by \link{OntologyAnnotation}s used in this ISA descriptor.}

\item{\code{unit_references}}{A list of units used as a \link{UnitReferences} objects}

\item{\code{protocol_parameters}}{A list of \link{ProtocolParameter} objects}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Process-new}{\code{Process$new()}}
\item \href{#method-Process-check_name}{\code{Process$check_name()}}
\item \href{#method-Process-set_name}{\code{Process$set_name()}}
\item \href{#method-Process-check_performer}{\code{Process$check_performer()}}
\item \href{#method-Process-set_date}{\code{Process$set_date()}}
\item \href{#method-Process-set_performer}{\code{Process$set_performer()}}
\item \href{#method-Process-check_comments}{\code{Process$check_comments()}}
\item \href{#method-Process-set_comments}{\code{Process$set_comments()}}
\item \href{#method-Process-add_comment}{\code{Process$add_comment()}}
\item \href{#method-Process-to_table}{\code{Process$to_table()}}
\item \href{#method-Process-to_list}{\code{Process$to_list()}}
\item \href{#method-Process-from_list}{\code{Process$from_list()}}
\item \href{#method-Process-add_process_order}{\code{Process$add_process_order()}}
\item \href{#method-Process-print}{\code{Process$print()}}
\item \href{#method-Process-clone}{\code{Process$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-new"></a>}}
\if{latex}{\out{\hypertarget{method-Process-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$new(
  name = character(),
  executes_protocol = NULL,
  date = NULL,
  performer = NULL,
  parameter_values = NULL,
  inputs = NULL,
  outputs = NULL,
  comments = NULL,
  `@id` = NULL,
  protocols = NULL,
  sources = NULL,
  samples = NULL,
  next_process = NULL,
  previous_process = NULL,
  materials = NULL,
  data_files = NULL,
  ontology_source_references = NULL,
  unit_references = NULL,
  protocol_parameters = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{If relevant, a unique name for the process to disambiguate it from other processes.}

\item{\code{executes_protocol}}{A reference to the Protocol that this process executes.}

\item{\code{date}}{A date formatted as an ISO8601 string corresponding to when the process event occurred.}

\item{\code{performer}}{The name of the person or organisation that carried out the process.}

\item{\code{parameter_values}}{A list of \link{ParameterValue}s relevant to the executing protocol.}

\item{\code{inputs}}{A list of input materials, possibly \link{Source}s, \link{Sample}s, \link{Material}s, \link{DataFile}s}

\item{\code{outputs}}{A list of output materials, possibly \link{Sample}s, \link{Material}s, \link{DataFile}s}

\item{\code{comments}}{Comments associated with instances of this class.}

\item{\code{@id}}{identifier}

\item{\code{protocols}}{list of available \link{Protocol}s}

\item{\code{sources}}{list of available \link{Source}s}

\item{\code{samples}}{list of available \link{Sample}s}

\item{\code{next_process}}{id of the next process in the sequence}

\item{\code{previous_process}}{id of the previous process in the sequence}

\item{\code{materials}}{a list of \link{Material} objects}

\item{\code{data_files}}{as list of \link{DataFile} objects}

\item{\code{ontology_source_references}}{ontology_source_references \link{OntologySource}s to be referenced by \link{OntologyAnnotation}s used in this ISA descriptor.}

\item{\code{unit_references}}{A list of units used as a \link{UnitReferences} objects}

\item{\code{protocol_parameters}}{A list of \link{ProtocolParameter} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Create a new \link{Process}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-check_name"></a>}}
\if{latex}{\out{\hypertarget{method-Process-check_name}{}}}
\subsection{Method \code{check_name()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$check_name(name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{of the process}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Check the the name has a non-zero length
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-set_name"></a>}}
\if{latex}{\out{\hypertarget{method-Process-set_name}{}}}
\subsection{Method \code{set_name()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$set_name(name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{of the process}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Set the name of the \link{Process}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-check_performer"></a>}}
\if{latex}{\out{\hypertarget{method-Process-check_performer}{}}}
\subsection{Method \code{check_performer()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$check_performer(performer)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{performer}}{a list of \link{Person} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
check performer is a list of \link{Person} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-set_date"></a>}}
\if{latex}{\out{\hypertarget{method-Process-set_date}{}}}
\subsection{Method \code{set_date()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$set_date(date, null.ok = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{date}}{a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd}

\item{\code{null.ok}}{accept NULL dates (boolean) default = FALSE}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Set date to a Date object
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-set_performer"></a>}}
\if{latex}{\out{\hypertarget{method-Process-set_performer}{}}}
\subsection{Method \code{set_performer()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$set_performer(performer)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{performer}}{a list of \link{Person} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
set performer if performer is a list of \link{Person} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-check_comments"></a>}}
\if{latex}{\out{\hypertarget{method-Process-check_comments}{}}}
\subsection{Method \code{check_comments()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$check_comments(comments)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comments}}{comments}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
checks if comments are a named list of character vectors
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-set_comments"></a>}}
\if{latex}{\out{\hypertarget{method-Process-set_comments}{}}}
\subsection{Method \code{set_comments()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$set_comments(comments)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comments}}{a list of comments}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Sets comments if they are in a valid format
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-add_comment"></a>}}
\if{latex}{\out{\hypertarget{method-Process-add_comment}{}}}
\subsection{Method \code{add_comment()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$add_comment(comment)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comment}}{a list of comments}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Add comment if it is in a valid format
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-to_table"></a>}}
\if{latex}{\out{\hypertarget{method-Process-to_table}{}}}
\subsection{Method \code{to_table()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$to_table()}\if{html}{\out{</div>}}
}

\subsection{Details}{
generate a tabular representation of a process object
}

\subsection{Returns}{
a Tibble
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-to_list"></a>}}
\if{latex}{\out{\hypertarget{method-Process-to_list}{}}}
\subsection{Method \code{to_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$to_list(ld = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ld}}{linked data (default FALSE)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
An R list representation of a \link{Process} object
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-from_list"></a>}}
\if{latex}{\out{\hypertarget{method-Process-from_list}{}}}
\subsection{Method \code{from_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$from_list(lst, recursive = TRUE, json = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lst}}{an Process object serialized to a list}

\item{\code{recursive}}{call to_list methods of any objects within this object (default TRUE)}

\item{\code{json}}{json  (default TRUE)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Make \link{Process} object from list
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-add_process_order"></a>}}
\if{latex}{\out{\hypertarget{method-Process-add_process_order}{}}}
\subsection{Method \code{add_process_order()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$add_process_order(lst, available_processes)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lst}}{list of processes with identifiers if next and previous
process ids}

\item{\code{available_processes}}{list of available \link{Process} objects from
process sqeuence}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
infer the order of processes in process sequence from the next and
previous process values.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-print"></a>}}
\if{latex}{\out{\hypertarget{method-Process-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$print()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Pretty Prints \link{Process} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Process-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Process-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Process$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
