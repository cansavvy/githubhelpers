#' Accept an invite
#'
#' Given an a user name, accept
#'
#' @param user THe username to accept the invites from
#' @param git_pat If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. If none is supplied, then this will attempt to
#' grab from a git pat set in the environment with usethis::create_github_token().
#' Authorization handled by \link[cow]{get_git_auth}
#' @param verbose TRUE/FALSE do you want more progress messages?
#'
#' @return a data frame with the repository's release information: tag_name and tag_date.
#' NAs are returned in these columns if there are no releases.
#'
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @export
#'
#' @examples \dontrun{
#'
#' accept_all_invites("jhudsl-robot")
#' }
accept_all_invites <- function(user,
                               git_pat = NULL,
                               verbose = TRUE) {

  # Try to get credentials other way
  auth_arg <- get_git_auth(git_pat = git_pat, quiet = TRUE)

  git_pat <- try(auth_arg$password, silent = TRUE)

  invites_url <- "https://api.github.com/user/repository_invitations"
  invites_url <- gsub("\\{user\\}", user, invites_url)

  # Github api get
  response <- httr::GET(
    invites_url,
    httr::add_headers(Authorization = paste0("token ", git_pat)),
    httr::accept_json()
  )

  if (httr::http_error(response)) {
    stop(paste0("url: ", invites_url, " failed"))
  }

  content <- unlist(httr::content(response))
  urls <- grep("^https://api.github.com/user/repository_invitations/", content, value = TRUE)

  responses <- sapply(urls, function(url) {
    response <- httr::PATCH(
      url,
      httr::add_headers(Authorization = paste0("token ", git_pat)),
      httr::accept_json()
    )
    if (httr::http_error(response)) {
      stop(paste0("url: ", invites_url, " failed"))
    }
  })

  return(releases)
}
