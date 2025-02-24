url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"
aquatics_targets <- read_csv(url, show_col_types = FALSE) #download data from NEON for CRAM and BARC sites
aquatics_targets |>     #graph the two sites through time
  filter(site_id %in% aquatics_focal_sites) |> 
  ggplot(aes(x = datetime, y = observation)) +
  geom_point() +
  facet_grid(variable~site_id, scales = "free_y") +
  theme_bw()
