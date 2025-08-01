# Read influence data
csvs <- list.files(file.path(banc.path,"data/influence/frankenbrain_v1.6/"),
                    pattern="csv",
                    full.names = TRUE)
csvs <- c(csvs[grepl("olfactory",csvs)])
distance.df <- data.frame()
for(csv in csvs){
  data <- readr::read_csv(csv, col_types = readr:::cols(.default = col_character()))
  n.seeds <- sum(data$is_seed=="True",na.rm=TRUE)
  data <- data[,c(2,4)]
  colnames(data) <- c("id","influence") # influence_norm_unsigned_forward_steady_state
  if(all(data$influence=="0")){
    warning("No values for: ", csv)
    next
  }
  data$seed <- gsub(".*from_|\\.csv*|_influence.*","",basename(csv))
  data$influence <- as.numeric(data$influence)
  data$influence_norm <- data$influence/n.seeds
  distance.df <- rbind(distance.df,data)
}

# Normalise influence scores
distance.df <- distance.df %>%
  dplyr::mutate(influence_original = influence,
                influence_norm_original = influence) %>%
  calculate_influence_norms %>%
  dplyr::ungroup()

# Read influence data
pkls <- list.files(file.path(banc.path,"data/cascade/frankenbrain_v1.6/"),
                   pattern=".pkl",
                   full.names = TRUE)
cascade.df <- data.frame()
for(pkl in pkls){
  if(!grepl("olfactory",pkl)){
    next
  }
  data <- py_load_object(pkl)
  data_r <- as.data.frame(data)
  colnames(data_r) <- c("id","distance")
  data_r$seed <- gsub("_to_.*|\\.pkl*","",basename(pkl))
  cascade.df <- rbind(cascade.df,data_r)
}
cascade.df$distance <- as.numeric(cascade.df$distance)
distance.df <- dplyr::left_join(distance.df,cascade.df,by=c("id","seed"))

# Read infection data
feathers <- list.files(file.path(banc.path,"data/schlegel_2024_fafb_neuron_ranks"),
                       pattern=".csv",
                       full.names = TRUE)
infection.df <- data.frame()
for(feather in feathers){
  if(!grepl("olfactory",feather)){
    next
  }
  data <- read_csv(feather, col_types = banc.col.types)
  data_r <- as.data.frame(data)
  data_r$seed <- gsub("_to_.*|.*v30409-|-10000|\\.csv*|.*230409-","",basename(feather))
  infection.df <- rbind(infection.df,data_r)
}
infection.df$layer_mean <- as.numeric(infection.df$layer_mean)
infection.df <- infection.df %>%
  dplyr::filter(!grepl("all",seed)) %>%
  dplyr::rename(id=root_id) %>%
  dplyr::select(id, seed, layer_mean)
distance.df <- dplyr::left_join(distance.df,infection.df,by=c("id","seed"))

# Apply direct connectivity
source("R/startup/franken-edgelist.R")
direct.conns <- franken.edgelist.simple %>%
  dplyr::filter(pre_cell_function=="olfactory",
                pre_super_class=="sensory") %>%
  dplyr::distinct(id=post, norm, count) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(count = sum(count,na.rm=TRUE),
                   norm = sum(norm,na.rm=TRUE))
distance.df <- distance.df %>%
  dplyr::left_join(direct.conns,
                   by = "id") %>%
  dplyr::mutate(count = ifelse(is.na(count),0,count),
                norm = ifelse(is.na(norm),0,norm))

# Format
distance.meta <- distance.df %>%
  dplyr::left_join(franken.meta %>%
                     dplyr::select(id, side, cluster,
                                   region, super_class, hemilineage, 
                                   cell_function, nerve, cell_class, sez_class, cell_sub_class, 
                                   cell_type, composite_cell_type, top_nt) %>%
                     dplyr::distinct(id, .keep_all = TRUE),
                   by = "id") %>%
  dplyr::filter(super_class!="glia", 
                !grepl("afferent|sensory",super_class),
                !is.na(super_class)) %>%
  dplyr::mutate(super_class = case_when(
    super_class == "brain_central_other" & !is.na(sez_class) ~ "sez",
    TRUE ~ super_class
  )) %>%
  dplyr::mutate(cell_type = ifelse(grepl("KCg-s",cell_type),"KCg-s",cell_type)) %>%
  dplyr::mutate(cell_sub_class = ifelse(is.na(cell_sub_class),gsub("_.*","",cell_type),cell_sub_class))

# Move to google buckets

