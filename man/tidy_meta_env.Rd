\name{tidy_meta_env}
\alias{tidy_meta_env}
\title{Remove Orphaned Meta objects}
\usage{
  tidy_meta_env(meta_env = "meta", key = "mi_key",
    data_env = .GlobalEnv)
}
\arguments{
  \item{meta_env}{character name of the meta environment,
  defaults to 'meta'.}

  \item{key}{character name of the key attribute, defaults
  to mi_key.}

  \item{data_env}{environment that holds the data, defaults
  to .GlobalEnv.}
}
\description{
  tidy_meta_env deletes meta objects if there is no
  corresponding data object. This function is useful when R
  Objects are deleted and meta information was not deleted.
}
\author{
  Matthias Bannert
}

