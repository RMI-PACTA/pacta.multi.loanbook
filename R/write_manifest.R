write_manifest <- function(config, path, prior_input_paths = NULL) {
  timestamp <-
    as.character(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%OS6",
        tz = "UTC"
      )
    )
  
  if (length(prior_input_paths) > 0) {
    names(prior_input_paths) <- basename(prior_input_paths)
    
    get_timestamp <- function(path) {
      yaml::read_yaml(file = file.path(path, "manifest.yml"))$run_info$timestamp
    }
    
    previous_manifests <-
      lapply(
        X = prior_input_paths,
        FUN = function(x) {
          list(
            dir = dirname(x),
            timestamp = get_timestamp(x)
          )
        }
      )
  } else {
    previous_manifests <- list()
  }
  
  manifest <-
    list(
      run_info = list(
        timestamp = timestamp
      ),
      previous_manifests = previous_manifests,
      config = config
    )
  
  
  yaml::write_yaml(
    x = manifest,
    file = path
  )
}
