
#' Create a new R project with standard package structure
#'
#' Scaffolds a new R project following best practices for reproducible and
#' shareable research. Builds on [usethis::create_package()] to generate the
#' core skeleton (DESCRIPTION, NAMESPACE, R/, man/) and then sets up the
#' recommended optional directories, testing infrastructure, a license, and
#' optionally a git repository.
#'
#' The resulting layout follows the structure described in
#' \href{https://r-pkgs.org}{R Packages (2e)}:
#' \itemize{
#'   \item \strong{R/} — reusable functions
#'   \item \strong{man/} — roxygen2-generated documentation
#'   \item \strong{data/} — processed datasets
#'   \item \strong{inst/extdata/} — raw data and external resources
#'   \item \strong{tests/testthat/} — unit tests (when \code{use_tests = TRUE})
#'   \item \strong{vignettes/} — long-form analysis documents (when \code{use_vignette} is provided)
#' }
#'
#' @param path Character string. Path where the new project directory will be
#'   created. The final component of the path is used as the package name.
#' @param title Character string. One-line project title in Title Case. Written
#'   to the \code{Title} field of DESCRIPTION.
#' @param author Character string. Author full name.
#' @param email Character string. Author email address.
#' @param description Character string. One-paragraph description of the project.
#'   Written to the \code{Description} field of DESCRIPTION.
#' @param license Character string. License to apply. One of \code{"MIT"},
#'   \code{"GPL-3"}, or \code{"CC0"}. Defaults to \code{"MIT"}.
#' @param use_git Logical. Whether to initialize a git repository and make an
#'   initial commit. Defaults to \code{TRUE}. Requires an interactive session
#'   or a configured git identity.
#' @param use_tests Logical. Whether to set up \pkg{testthat} testing
#'   infrastructure. Defaults to \code{TRUE}.
#' @param use_vignette Character string or \code{NULL}. If provided, creates a
#'   vignette with this name (e.g. \code{"my-analysis"}). Defaults to
#'   \code{NULL} (no vignette created).
#' @param open Logical. Whether to activate the new project in the current
#'   RStudio session. Defaults to \code{interactive()}.
#'
#' @return Called for side effects. Invisibly returns the normalized project
#'   path.
#'
#' @export
new_project <- function(path,
                        title,
                        author,
                        email,
                        description = NULL,
                        license = c("MIT", "GPL-3", "CC0"),
                        use_git = TRUE,
                        use_tests = TRUE,
                        use_vignette = NULL,
                        open = interactive()) {

  # validate inputs
  assert_single_string(path)
  assert_single_string(title)
  assert_single_string(author)
  assert_single_string(email)
  if (!is.null(description)) assert_single_string(description)
  license <- match.arg(license)
  assert_single_logical(use_git)
  assert_single_logical(use_tests)
  if (!is.null(use_vignette)) assert_single_string(use_vignette)
  assert_single_logical(open)

  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("Package 'usethis' is required. Install it with: install.packages('usethis')",
         call. = FALSE)
  }

  # build DESCRIPTION fields
  fields <- list(
    Title = title,
    `Authors@R` = sprintf(
      'person("%s", email = "%s", role = c("aut", "cre"))',
      author, email
    ),
    Description = if (!is.null(description)) {
      description
    } else {
      "What the project does (one paragraph)."
    }
  )

  # create the core package skeleton; defer opening so we can configure first
  usethis::create_package(path, fields = fields, open = FALSE)

  # all subsequent usethis calls are scoped to the new project
  usethis::with_project(path, {

    # recommended optional directories
    dir.create("data", showWarnings = FALSE)
    dir.create("data/meta", showWarnings = FALSE)
    dir.create("data/raw", showWarnings = FALSE)
    dir.create("data/derived", showWarnings = FALSE)
    dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)

    # .gitignore
    writeLines(c(
      "# MAC files",
      ".DS_Store",
      "",
      "# History files",
      ".Rhistory",
      ".Rapp.history",
      ".Rproj.user",
      ".RData",
      "",
      "# Data files I'm generating",
      "sandbox/",
      "inst/doc",
      "todo.txt",
      "*.Rproj",
      "",
      "# Input and result files too large for GH",
      "figures/",
      "# Private Data ",
      "data/raw/",
      "data/derived/",
      "data/sim/",
      "data/meta/",
      "",
      "# Pipeline logs and state",
      "logs/",
      ".pipeline_state"
    ), ".gitignore")

    # license
    switch(license,
      "MIT"   = usethis::use_mit_license(author),
      "GPL-3" = usethis::use_gpl_license(3),
      "CC0"   = usethis::use_cc0_license()
    )

    # testthat infrastructure
    if (use_tests) {
      usethis::use_testthat()
    }

    # vignette skeleton
    if (!is.null(use_vignette)) {
      usethis::use_vignette(use_vignette, title = title)
    }

    # git last so that all generated files are included in the initial commit
    if (use_git) {
      usethis::use_git()
    }

  })

  if (open) {
    usethis::proj_activate(path)
  }

  invisible(normalizePath(path))
}
