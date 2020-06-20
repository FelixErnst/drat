##' This helper function create a new repository, optionally creates and checks
##' out a git branch (default: \code{gh-pages}) and fills it with the required 
##' paths for \sQuote{source} and \sQuote{binary} packages.
##'
##' This function is still undergoing development and polish and may
##' change in subsequent versions.
##' 
##' @section Options:
##'
##' Set using \code{\link{options}}
##'
##' \describe{
##'   \item{\code{dratBranch}}{The git branch to store packages on. Defaults to \code{gh-pages}}
##' }
##'
##' @title Intialize a git repo for drat
##' @param name A character variable with the name the new repository,
##' the default is \dQuote{drat}.
##' @param basepath A character variable with path to the directory in
##' which the new repository is to be created. The default value is
##' \dQuote{"~/git"}.
##' @param type type(s) of packages the repo should be initialized for.
##' @param version the version(s) of the packages the repo should be initialized
##' for. Only used if \code{type} contains a \sQuote{binary} type. (default:
##' \code{version = getRversion()})
##' @param initGit \code{TRUE} or \code{FALSE}: Should a git repo be 
##' initialized? (default: \code{initGit = FALSE})
##' @return The function is invoked for its side-effects and only
##' returns \code{NULL} invisibly.
##' @author Dirk Eddelbuettel
initRepo <- function(name = "drat", basepath = "~/git", 
                     type = c("source", "binary", "mac.binary", "mac.binary.el-capitan",
                              "mac.binary.mavericks", "win.binary", "both"),
                     version = getRversion(), initGit = FALSE) {
    # input check
    type <- .norm_type(type)
    version <- package_version(version)
    if (!dir.exists(basepath)) stop("Directory '", basepath, "' does not exist.", call.=FALSE)
    dir <- file.path(basepath, name)
    if (dir.exists(dir)) stop("Directory '", dir, "' already exists.", call.=FALSE)
    #
    
    # create repo dir
    dir.create(dir)
    
    if(initGit){
        ## check for the optional git2r package
        haspkg <- requireNamespace("git2r", quietly=TRUE)
        
        branch <- getOption("dratBranch", "gh-pages")
        if (haspkg) {
            repo <- git2r::init(dir)
            writeLines("## Drat Repo", file.path(dir, "README.md"))
            git2r::add(repo, "README.md")
            cmt <- git2r::commit(repo, "Initial Commit")
            
            git2r::checkout(repo, branch, create=TRUE)
        } else {
            warning("Git init skipped as git2r package is missing.",
                    call. = FALSE)
        }
    }
    
    # get directories
    dirs <- lapply(version, .get_type_directories, dir = dir, type = type)
    dirs <- unique(unlist(dirs))
    # create directories
    lapply(dirs, dir.create, recursive = TRUE)
    invisible(NULL)
}

.get_type_directories <- function(version, dir, type){
    # subset to types for mac.binary
    type <- .subset_mac_binary_type(type, version)
    # get dirs
    contrib.url2(dir,type = type, version = version)
}

.subset_mac_binary_type <- function(type, version){
    if(version >= package_version("3.2") && 
       version <= package_version("3.3")){
        type <- type[!grepl("mac.binary",type) | type == "mac.binary.mavericks"]
    } else if(version >= package_version("3.4") && 
              version <= package_version("3.6")){
        type <- type[!grepl("mac.binary",type) | type == "mac.binary.el-capitan"]
    } else if(version > package_version("3.6")){
        type <- type[!grepl("mac.binary",type) | type == "mac.binary"]
    } else {
        type <- type[!grepl("mac.binary",type)]
    }
    type
}