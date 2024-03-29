% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config-class.R
\name{load_src_cfg}
\alias{load_src_cfg}
\title{Load configuration for a data source}
\usage{
load_src_cfg(src = NULL, name = "data-sources", cfg_dirs = NULL)
}
\arguments{
\item{src}{(Optional) name(s) of data sources used for subsetting}

\item{name}{String valued name of a config file which will be looked up in
the default config directors}

\item{cfg_dirs}{Additional directory/ies to look for configuration files}
}
\value{
A list of data source configurations as \code{src_cfg} objects.
}
\description{
For a data source to be accessible by \code{ricu}, a configuration object
inheriting from the S3 class \code{src_cfg} is required. Such objects can be
generated from JSON based configuration files, using \code{load_src_cfg()}.
Information encoded by this configuration object includes available ID
systems (mainly for use in \code{\link[=change_id]{change_id()}}, default column names per table
for columns with special meaning (such as index column, value columns, unit
columns, etc.), as well as a specification used for initial setup of the
dataset which includes file names and column names alongside their data
types.
}
\details{
Configuration files are looked for as files \code{name} with added suffix
\code{.json} starting with the directory (or directories) supplied as \code{cfg_dirs}
argument, followed by the directory specified by the environment variable
\code{RICU_CONFIG_PATH}, and finally in \code{extdata/config} of the package install
directory. If files with matching names are found in multiple places they
are concatenated such that in cases of name clashes. the earlier hits take
precedent over the later ones. The following JSON code blocks show excerpts
of the config file available at

\if{html}{\out{<div class="sourceCode">}}\preformatted{system.file("extdata", "config", "data-sources.json", package = "ricu")
}\if{html}{\out{</div>}}

A data source configuration entry in a config file starts with a name,
followed by optional entries \code{class_prefix} and further (variable)
key-value pairs, such as an URL. For more information on \code{class_prefix},
please refer to the end of this section. Further entries include \code{id_cfg}
and \code{tables} which are explained in more detail below. As outline, this
gives for the data source \code{mimic_demo}, the following JSON object:

\if{html}{\out{<div class="sourceCode">}}\preformatted{\{
  "name": "mimic_demo",
  "class_prefix": ["mimic_demo", "mimic"],
  "url": "https://physionet.org/files/mimiciii-demo/1.4",
  "id_cfg": \{
    ...
  \},
  "tables": \{
    ...
  \}
\}
}\if{html}{\out{</div>}}

The \code{id_cfg} entry is used to specify the available ID systems for a data
source and how they relate to each other. An ID system within the context
of \code{ricu} is a patient identifier of which typically several are present in
a data set. In MIMIC-III, for example, three ID systems are available:
patient IDs (\code{subject_id}), hospital admission IDs (\code{hadm_id}) and ICU stay
IDs (\code{icustay_id}). Furthermore there is a one-to-many relationship between
\code{subject_id} and \code{hadm_id}, as well as between \code{hadm_id} and \code{icustay_id}.
Required for defining an ID system are a name, a \code{position} entry which
orders the ID systems by their cardinality, a \code{table} entry, alongside
column specifications \code{id}, \code{start} and \code{end}, which define how the IDs
themselves, combined with start and end times can be loaded from a table.
This gives the following specification for the ICU stay ID system in
MIMIC-III:

\if{html}{\out{<div class="sourceCode">}}\preformatted{\{
  "icustay": \{
    "id": "icustay_id",
    "position": 3,
    "start": "intime",
    "end": "outtime",
    "table": "icustays"
  \}
\}
}\if{html}{\out{</div>}}

Tables are defined by a name and entries \code{files}, \code{defaults}, and \code{cols},
as well as optional entries \code{num_rows} and \code{partitioning}. As \code{files} entry,
a character vector of file names is expected. For all of MIMIC-III a single
\code{.csv} file corresponds to a table, but for example for HiRID, some tables
are distributed in partitions. The \code{defaults} entry consists of key-value
pairs, identifying columns in a table with special meaning, such as the
default index column or the set of all columns that represent timestamps.
This gives as an example for a table entry for the \code{chartevents} table in
MIMIC-III a JSON object like:

\if{html}{\out{<div class="sourceCode">}}\preformatted{\{
  "chartevents": \{
    "files": "CHARTEVENTS.csv.gz",
    "defaults": \{
      "index_var": "charttime",
      "val_var": "valuenum",
      "unit_var": "valueuom",
      "time_vars": ["charttime", "storetime"]
    \},
    "num_rows": 330712483,
    "cols": \{
      ...
    \},
    "partitioning": \{
      "col": "itemid",
      "breaks": [127, 210, 425, 549, 643, 741, 1483, 3458, 3695, 8440,
                 8553, 220274, 223921, 224085, 224859, 227629]
    \}
  \}
\}
}\if{html}{\out{</div>}}

The optional \code{num_rows} entry is used when importing data (see
\code{\link[=import_src]{import_src()}}) as a sanity check, which is not performed if this entry is
missing from the data source configuration. The remaining table entry,
\code{partitioning}, is optional in the sense that if it is missing, the table
is not partitioned and if it is present, the table will be partitioned
accordingly when being imported (see \code{\link[=import_src]{import_src()}}). In order to specify a
partitioning, two entries are required, \code{col} and \code{breaks}, where the former
denotes a column and the latter a numeric vector which is used to construct
intervals according to which \code{col} is binned. As such, currently \code{col} is
required to be of numeric type. A \code{partitioning} entry as in the example
above will assign rows corresponding to \code{idemid} 1 through 126 to partition
1, 127 through 209 to partition 2 and so on up to partition 17.

Column specifications consist of a \code{name} and a \code{spec} entry alongside a
name which determines the column name that will be used by \code{ricu}. The
\code{spec} entry is expected to be the name of a column specification function
of the \code{readr} package (see \code{\link[readr:cols]{readr::cols()}}) and all further entries in a
\code{cols} object are used as arguments to the \code{readr} column specification.
For the \code{admissions} table of MIMIC-III the columns \code{hadm_id} and
\code{admittime} are represented by:

\if{html}{\out{<div class="sourceCode">}}\preformatted{\{
  ...,
  "hadm_id": \{
    "name": "HADM_ID",
    "spec": "col_integer"
  \},
  "admittime": \{
    "name": "ADMITTIME",
    "spec": "col_datetime",
    "format": "\%Y-\%m-\%d \%H:\%M:\%S"
  \},
  ...
\}
}\if{html}{\out{</div>}}

Internally, a \code{src_cfg} object consist of further S3 classes, which are
instantiated when loading a JSON source configuration file. Functions for
creating and manipulating \code{src_cfg} and related objects are marked
\code{internal} but a brief overview is given here nevertheless:
\itemize{
\item \code{src_cfg}: wraps objects \code{id_cfg}, \code{col_cfg} and optionally \code{tbl_cfg}
\item \code{id_cfg}: contains information in ID systems and is created from \code{id_cfg}
entries in config files
\item \code{col_cfg}: contains column default settings represented by \code{defaults}
entries in table configuration blocks
\item \code{tbl_cfg}: used when importing data and therefore encompasses information
in \code{files}, \code{num_rows} and \code{cols} entries of table configuration blocks
}

A \code{src_cfg} can be instantiated without corresponding \code{tbl_cfg} but
consequently cannot be used for data import (see \code{\link[=import_src]{import_src()}}). In that
sense, table config entries \code{files} and \code{cols} are optional as well with
the restriction that the data source has to be already available in \code{.fst}
format

An example for such a slimmed down config file is available at

\if{html}{\out{<div class="sourceCode">}}\preformatted{system.file("extdata", "config", "demo-sources.json", package = "ricu")
}\if{html}{\out{</div>}}

The \code{class_prefix} entry in a data source configuration is used create sub-
classes to \code{src_cfg}, \code{id_cfg}, \code{col_cfg} and \code{tbl_cfg} classes and passed
on to constructors of \code{src_env} (\code{\link[=new_src_env]{new_src_env()}}) and \code{src_tbl}
\code{\link[=new_src_tbl]{new_src_tbl()}} objects. As an example, for the above \code{class_prefix} value
of \code{c("mimic_demo", "mimic")}, the corresponding \code{src_cfg} will be assigned
classes \code{c("mimic_demo_cfg", "mimic_cfg", "src_cfg")} and consequently the
\code{src_tbl} objects will inherit from \code{"mimic_demo_tbl"}, \code{"mimic_tbl"} and
\code{ "src_tbl"}. This can be used to adapt the behavior of involved S3 generic
function to specifics of the different data sources. An example for this is
how \code{\link[=load_difftime]{load_difftime()}} uses theses sub-classes to smoothen out different
time-stamp representations. Furthermore, such a design was chosen with
extensibility in mind. Currently, \code{\link[=download_src]{download_src()}} is designed around data
sources hosted on PhysioNet, but in order to include a dataset external to
PhysioNet, the \code{download_src()} generic can simply be extended for the new
class.
}
\examples{
cfg <- load_src_cfg("mimic_demo")
str(cfg, max.level = 1L)
cfg <- cfg[["mimic_demo"]]
str(cfg, max.level = 1L)

cols <- as_col_cfg(cfg)
index_var(head(cols))
time_vars(head(cols))

as_id_cfg(cfg)

}
