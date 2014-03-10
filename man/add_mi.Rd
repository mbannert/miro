\name{add_mi}
\alias{add_mi}
\title{Add meta information to an R object}
\usage{
  add_mi(ts_name, ts_env, key = NA_character_,
    meta_env_name = "meta", srcname = NA_character_,
    legacy_key = NA_character_, cmnt = NA_character_,
    restrict = NA_character_, overwrite = FALSE)
}
\arguments{
  \item{ts_name}{name of a time_series object}

  \item{ts_env}{name of an environment that stores time
  series objects}

  \item{key}{character key, defaults to NA_character_ which
  leads to using the name of the original object}

  \item{meta_env}{character name of the meta environment,
  defaults to "meta"}

  \item{srcname}{character name of the data source}

  \item{legacy_key}{character name of a key that was used
  in another system}

  \item{cmnt}{character comment}

  \item{restrict}{character description of restrictions to
  the data}

  \item{overwrite}{boolean defaults to FALSE}
}
\description{
  This function adds a meta information object to the meta
  environment. If the meta environment does not exist an
  environment is created. Also the original data object is
  linked to the meta information by an
}
\author{
  Matthias Bannert
}

