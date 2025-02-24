library(tidyverse)
url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"
aquatics_targets <- read_csv(url, show_col_types = FALSE) #download data from NEON for all aquatic (?seems to be missing a couple?) sites
aquatics_targets |>     #graph the two sites through time
  filter(site_id == 'BLWA') |>  #pull data from BLWA site
  ggplot(aes(x = datetime, y = observation)) +
  geom_point(size = .5) +
  facet_grid(variable~site_id, scales = "free_y") +
  theme_bw()
