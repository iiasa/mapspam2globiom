# Function to run mapspam in gams
run_gams_adm_level <- function(ac, param, verbose = T){
  model <- system.file("gams", glue::glue("max_score.gms"), package = "mapspam2globiom", mustWork = TRUE)
  input <- file.path(param$spam_path,
      glue::glue("processed_data/intermediate_output/{ac}/input_{param$res}_{param$year}_{ac}_{param$iso3c}.gdx"))

  output <- file.path(param$spam_path,
      glue::glue("processed_data/intermediate_output/{ac}/output_{param$res}_{param$year}_{ac}_{param$iso3c}.gdx"))

  lst <- file.path(param$spam_path,
      glue::glue("processed_data/intermediate_output/{ac}/lst_{param$model}_{param$res}_{param$year}_{ac}_{param$iso3c}.lst"))


  logf <- file.path(param$spam_path,
      glue::glue("processed_data/intermediate_output/{ac}/model_log_{param$model}_{param$res}_{param$year}_{ac}_{param$iso3c}.log"))

  gams_system_call <- glue::glue("gams.exe {model} --gdx_input={input} --gdx_output={output} lf={logf} o={lst} logOption 4")

  gams_system_call <- gsub("/", "\\\\", gams_system_call) # change forward- into backslash
  cat("\nRunning ",  param$model, "model for ", ac)
  cmd_output = system(gams_system_call, intern = TRUE)
  if (verbose) {
    message((paste(cmd_output, collapse = "\n")))
  }
  cat("\nFinished running ",  param$model, "model for ", ac, "\n")
}
