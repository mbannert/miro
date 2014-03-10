\name{.add_mi}
\alias{.add_mi}
\title{Helper Function to add meta information}
\usage{
  .add_mi(x, meta_env_name = "meta",
    srcname = NA_character_, legacy_key = NA_character_,
    cmnt = NA_character_, restrict = NA_character_,
    overwrite = FALSE)
}
\arguments{
  \item{x}{data object}

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
  This functions adds meta information to the meta
  environment. If the meta environment does not exist, a
  meta environment is created. This function not be used as
  standalone because it creates meta objects without
  checking whether a corresponding data object exists. The
  function .add_mi adds fixed meta information as well as a
  container for localized meta information by using the
  miro's reference classes.
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{add_mit}}
}

