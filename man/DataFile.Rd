% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataFile.R
\name{DataFile}
\alias{DataFile}
\title{Represents a data file in an experimental graph.}
\description{
Represents a data file in an experimental graph.

Represents a data file in an experimental graph.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{filename}}{A name/reference for the data file.}

\item{\code{file_path}}{the path to the file}

\item{\code{check_file_exists}}{check that the file exists}

\item{\code{compute_hash}}{should the hash of the file be computed?}

\item{\code{hash_algo}}{hashing algorithm to use (default md5)}

\item{\code{label}}{The data file type, as indicated by a label such as 'Array Data File' or 'Raw Data File'}

\item{\code{generated_from}}{Reference to Sample(s) the DataFile is generated from}

\item{\code{comments}}{Comments associated with instances of this class.}

\item{\code{type}}{type}

\item{\code{@id}}{identifier}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DataFile-new}{\code{DataFile$new()}}
\item \href{#method-DataFile-check_filename}{\code{DataFile$check_filename()}}
\item \href{#method-DataFile-set_filename}{\code{DataFile$set_filename()}}
\item \href{#method-DataFile-check_file_path}{\code{DataFile$check_file_path()}}
\item \href{#method-DataFile-set_file_path}{\code{DataFile$set_file_path()}}
\item \href{#method-DataFile-check_hash_algo}{\code{DataFile$check_hash_algo()}}
\item \href{#method-DataFile-check_generated_from}{\code{DataFile$check_generated_from()}}
\item \href{#method-DataFile-set_generated_from}{\code{DataFile$set_generated_from()}}
\item \href{#method-DataFile-add_generated_from}{\code{DataFile$add_generated_from()}}
\item \href{#method-DataFile-check_comments}{\code{DataFile$check_comments()}}
\item \href{#method-DataFile-set_comments}{\code{DataFile$set_comments()}}
\item \href{#method-DataFile-add_comment}{\code{DataFile$add_comment()}}
\item \href{#method-DataFile-validate_file}{\code{DataFile$validate_file()}}
\item \href{#method-DataFile-to_list}{\code{DataFile$to_list()}}
\item \href{#method-DataFile-to_table}{\code{DataFile$to_table()}}
\item \href{#method-DataFile-from_list}{\code{DataFile$from_list()}}
\item \href{#method-DataFile-print}{\code{DataFile$print()}}
\item \href{#method-DataFile-get_hash}{\code{DataFile$get_hash()}}
\item \href{#method-DataFile-get_id}{\code{DataFile$get_id()}}
\item \href{#method-DataFile-clone}{\code{DataFile$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-new"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$new(
  filename = character(),
  file_path = character(),
  check_file_exists = FALSE,
  compute_hash = FALSE,
  hash_algo = "md5",
  label = NULL,
  generated_from = NULL,
  comments = NULL,
  type = NULL,
  `@id` = character()
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{filename}}{A name/reference for the data file.}

\item{\code{file_path}}{the path to the file}

\item{\code{check_file_exists}}{check that the file exists}

\item{\code{compute_hash}}{should the hash of the file be computed?}

\item{\code{hash_algo}}{hashing algorithm to use (default md5)}

\item{\code{label}}{The data file type, as indicated by a label such as 'Array Data File' or 'Raw Data File'}

\item{\code{generated_from}}{Reference to Sample(s) the DataFile is generated from}

\item{\code{comments}}{Comments associated with instances of this class.}

\item{\code{type}}{type}

\item{\code{@id}}{identifier}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Make a new DataFile object
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-check_filename"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-check_filename}{}}}
\subsection{Method \code{check_filename()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$check_filename(filename)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{filename}}{The filename of the \link{DataFile}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Check if the filename of the \link{DataFile} is a string
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-set_filename"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-set_filename}{}}}
\subsection{Method \code{set_filename()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$set_filename(filename)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{filename}}{The filename of the \link{DataFile}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
set the filename of the \link{DataFile} if valid
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-check_file_path"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-check_file_path}{}}}
\subsection{Method \code{check_file_path()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$check_file_path(file_path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_path}}{The file_path of the \link{DataFile}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Check if the file_path of the \link{DataFile} is a string
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-set_file_path"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-set_file_path}{}}}
\subsection{Method \code{set_file_path()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$set_file_path(file_path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_path}}{The file_path of the \link{DataFile}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
set the file_path of the \link{DataFile} if valid
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-check_hash_algo"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-check_hash_algo}{}}}
\subsection{Method \code{check_hash_algo()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$check_hash_algo()}\if{html}{\out{</div>}}
}

\subsection{Details}{
checks that hash algo is valid hash algorithm name
}

\subsection{Returns}{
logical
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-check_generated_from"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-check_generated_from}{}}}
\subsection{Method \code{check_generated_from()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$check_generated_from(generated_from)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{generated_from}}{a list of \link{Sample} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Check that generated_from is a list of \link{Sample} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-set_generated_from"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-set_generated_from}{}}}
\subsection{Method \code{set_generated_from()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$set_generated_from(generated_from)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{generated_from}}{a list of \link{Sample} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Set generated_from if input is valid
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-add_generated_from"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-add_generated_from}{}}}
\subsection{Method \code{add_generated_from()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$add_generated_from(generated_from)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{generated_from}}{a list of \link{Sample} objects}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Add generated_from if input is valid
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-check_comments"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-check_comments}{}}}
\subsection{Method \code{check_comments()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$check_comments(comments)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-DataFile-set_comments"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-set_comments}{}}}
\subsection{Method \code{set_comments()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$set_comments(comments)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-DataFile-add_comment"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-add_comment}{}}}
\subsection{Method \code{add_comment()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$add_comment(comment)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-DataFile-validate_file"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-validate_file}{}}}
\subsection{Method \code{validate_file()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$validate_file()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Checks that the hash of the file at filepath matches the stored
hash of the file. Returns TRUE if they match and FALSE if not.

validate_file
}

\subsection{Returns}{
logical
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-to_list"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-to_list}{}}}
\subsection{Method \code{to_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$to_list(ld = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ld}}{linked data (default FALSE)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
An R list representation of a \link{DataFile} object
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-to_table"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-to_table}{}}}
\subsection{Method \code{to_table()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$to_table()}\if{html}{\out{</div>}}
}

\subsection{Details}{
serialise DataFile object to a tabular format (tibble)
}

\subsection{Returns}{
a tibble
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-from_list"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-from_list}{}}}
\subsection{Method \code{from_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$from_list(lst, json = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lst}}{an Characteristic object serialized to a list}

\item{\code{json}}{json  (default TRUE)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Make \link{DataFile} object from list
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-print"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$print()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Pretty prints \link{DataFile} objects
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-get_hash"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-get_hash}{}}}
\subsection{Method \code{get_hash()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$get_hash()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Get the hash of the file
}

\subsection{Returns}{
a hash
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-get_id"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-get_id}{}}}
\subsection{Method \code{get_id()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$get_id()}\if{html}{\out{</div>}}
}

\subsection{Details}{
get the id of the \link{DataFile} object
}

\subsection{Returns}{
a string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataFile-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DataFile-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataFile$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
