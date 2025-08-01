#######################################
### COLLATE FRANKEN BRAIN INFLUENCE ###
#######################################
source("R/startup/banc-startup.R")

# Master folder
influence.master <- file.path(banc.dropxbox.influence.save.path,"frankenbrain_v1.6_run2/")
seed.groups <- list.files(influence.master, recursive = FALSE, full.names = TRUE)
seed.groups <- seed.groups[!grepl("Seed_00|Seed_07",seed.groups)]
seed.sets <- list.files(seed.groups, recursive = FALSE, full.names = TRUE)

# Create .sql file
table_name <- "frankenbrain_v_1_6_2_influence"
dir.create(banc.dropxbox.influence.save.path, showWarnings = FALSE)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropxbox.influence.save.path,"influence_v_1_6_2.sqlite"))
DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s;", table_name))
dbDisconnect(con)

# Iterate over seed groups
for(sg in seed.groups){
  message("Working on: ", basename(sg))
  influence.list <- list()
  
  # process2
  process2 <- !grepl("_00$|_09$|_07$",sg)
  
  # Set up error bar
  seed.files <- list.files(sg, recursive = FALSE, full.names = TRUE, pattern = "\\.csv")
  pb <- progress_bar$new(total = length(seed.files), format = "[:bar] :percent ETA: :eta")
  if(!length(seed.files)){
    next
  }
  
  # Iterate over seed groups
  for(csv in seed.files){
    
    # Read all data
    data <- readr::read_csv(csv, 
                            progress = FALSE,
                            col_types = inf.col.types)
    data <- data[,-1]
    colnames(data) <- c("id","is_seed","influence")
    n.seeds <- sum(data$is_seed, na.rm = TRUE)
    
    # Skip if no influence
    if(all(data$influence=="0")){
      warning("No values for: ", csv)
      pb$tick()
      next
    }
    
    # Apply seed label
    data$seed <- gsub(".*from_|\\.csv*|_influence.*","",basename(csv))
    data$level <- tolower(gsub(".*from_|\\.csv*|_influence.*","",basename(sg)))
    
    # Perform scaling operations
    data$influence <- as.numeric(data$influence)
    data$influence_log <- log10(data$influence)
    data$influence_norm <- as.numeric(data$influence)/n.seeds
    data$influence_norm_log <- log10(data$influence_norm)
    data$influence_mad <- as.vector((data$influence - median(data$influence, na.rm = TRUE))/stats::mad(data$influence, na.rm = TRUE))
    data$influence_zscore <- as.vector(as.vector(scale(data$influence, center = TRUE, scale = TRUE)))
    data <- data %>%
      dplyr::mutate(influence_mad2 = NA,
                    influence_zscore2 = NA,
                    influence_mad3 = NA,
                    influence_zscore3 = NA)
    
    if(!process2){
      # Write to sql data base
      con <- DBI::dbConnect(RSQLite::SQLite(),
                            file.path(banc.dropxbox.influence.save.path,"influence_v_1_6_2.sqlite"))
      DBI::dbWriteTable(con,
                        name = table_name,
                        value = data,
                        overwrite = FALSE,
                        append = TRUE)
      dbDisconnect(con)
    }else{
      # Add to our list
      influence.list[[length(influence.list)+1]] <- data
    }
    
    # Update progress bar
    pb$tick()
    
  }
  
  if(process2){
    # Bind
    influence.data <- do.call(rbind,influence.list)
    if(!norw(influence.data)){
      next
    }
    
    # Scaling type 2
    influence.data <- influence.data %>%
      dplyr::group_by(id, level) %>%
      dplyr::mutate(influence_mad2 = as.vector((influence - median(influence, na.rm = TRUE))/stats::mad(influence, na.rm = TRUE)),
                    influence_zscore2 = as.vector(scale(influence, center = TRUE, scale = TRUE)),
                    influence_mad3 = as.vector((influence_mad - median(influence_mad, na.rm = TRUE))/stats::mad(influence_mad, na.rm = TRUE)),
                    influence_zscore3 = as.vector(scale(influence_zscore, center = TRUE, scale = TRUE))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(influence = signif(influence,6),
                    influence_log = signif(influence_log,6),
                    influence_norm = signif(influence_norm,6),
                    influence_norm_log = signif(influence_norm_log,6),
                    influence_mad = signif(influence_mad,6),
                    influence_mad2 = signif(influence_mad2,6),
                    influence_mad3 = signif(influence_mad3,6),
                    influence_zscore = signif(influence_zscore,6),
                    influence_zscore2 = signif(influence_zscore2,6),
                    influence_zscore3 = signif(influence_zscore3,6)
      )
    
    # Write to sql data base
    con <- DBI::dbConnect(RSQLite::SQLite(),
                          file.path(banc.dropxbox.influence.save.path,"influence_v_1_6_2.sqlite"))
    DBI::dbWriteTable(con,
                      name = table_name,
                      value = influence.data,
                      overwrite = FALSE,
                      append = TRUE)
    dbDisconnect(con) 
  }
}

# # Move data to data1 on O2
# sync_files <- function(src, dest, 
#                        remote.source = FALSE,
#                        move.old = FALSE, 
#                        extensions = c("png","csv")) {
#   
#   if(remote.source){
#     # Change permissions on source files if remote
#     system(paste(c("ssh $(whoami)@transfer.rc.hms.harvard.edu find", src, "-type", "f", "-name", "*.png", "-o", "-name", "*.csv", "-exec", "chmod", "644", "{}", "+"),collapse=" "))
#     system(paste0(c("ssh $(whoami)@transfer.rc.hms.harvard.edu find", src, "-type", "d", "-exec", "chmod", "755", "{}", "+"),collapse=" "))
#   }else{
#     # Change permissions on source files
#     system2("find", c(src, "-type", "f", "-name", "*.png", "-o", "-name", "*.csv", "-exec", "chmod", "644", "{}", "+"))
#     system2("find", c(src, "-type", "d", "-exec", "chmod", "755", "{}", "+")) 
#   }
#   
#   # Construct the rsync command
#   include_patterns <- paste(paste0("--include='*.", extensions, "'"), collapse = " ")
#   rsync_cmd <- paste(
#     "ssh $(whoami)@transfer.rc.hms.harvard.edu rsync",
#     "-arlptv --no-g --recursive",
#     "--include='*/'",
#     include_patterns,
#     "--exclude='*/done/*'",
#     "--exclude='*/old*'",
#     "--exclude='old*'",
#     "--exclude='*/express/*'",
#     "--exclude='*'",
#     "--prune-empty-dirs",
#     ifelse(move.old,"--backup",""),
#     ifelse(move.old,sprintf("--backup-dir='%s/%s%s/'",dest,"old_",Sys.Date()),""),
#     "--delete",
#     shQuote(paste0(src,"/")),
#     shQuote(paste0(dest,"/"))
#   )
#   cat("command: ",rsync_cmd)
#   
#   # Run rsync using system()
#   result <- system(rsync_cmd, intern = TRUE)
# 
#   # report
#   cat(result)
#   cat("Synced files from", src, "to", dest, "\n")
#   cat(paste(result, collapse = "\n"), "\n")
# }
# cat("Synchronising matching files between O2 and the fileserve \n")
# A <- '/n/data1/hms/neurobio/wilson/banc/'
# B <- '/n/files/Neurobio/wilsonlab/banc/'
# sync_files(path(A, "influence/"), path(B, "influence/"), extensions = c("sqlite"))
# 
# # Define the remote name
# remote_name <- "hms"
# local_file <- file.path(banc.dropxbox.influence.save.path,"influence_v_1_6_2.sqlite")
# system(paste("rclone copy", local_file, paste0(remote_name, ":", "neuroanat/influence/")))




