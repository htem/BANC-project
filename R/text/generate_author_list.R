# script to read author list spreadsheet and output author list, affiliations,
#  FlyWire Consortium

library(googlesheets4)
library(officer)
library(dplyr)
library(stringr)
library(flextable)

banc_author_url <- "https://docs.google.com/spreadsheets/d/1AWz6JdOPpIUZicQITz7aJvLlsNcSy0N66drHZ4ioJsg/edit?usp=sharing"
author_sheet_name <- "authors"
consortium_sheet_name <- "FlyWire Consortium"

author_sheet <- read_sheet(banc_author_url, sheet = author_sheet_name)
consortium_sheet <- read_sheet(banc_author_url, sheet = consortium_sheet_name)

# Check if Order column is a complete sequence from 1 to nrow
check_order_sequence <- function(df) {
  n_rows <- nrow(df)
  expected_sequence <- 1:n_rows
  actual_order <- sort(df$Order[!is.na(df$Order)])
  
  # Check if they match
  if (identical(actual_order, expected_sequence)) {
    cat("✓ Order column is correct: contains integers 1 to", n_rows, "without duplicates or missing numbers\n")
    return(TRUE)
  } else {
    cat("✗ Order column has issues:\n")
    
    # Check for missing values
    if (any(is.na(df$Order))) {
      missing_count <- sum(is.na(df$Order))
      cat("  - Contains", missing_count, "missing values (NA)\n")
    }
    
    # Check for duplicates
    duplicates <- df$Order[duplicated(df$Order) & !is.na(df$Order)]
    if (length(duplicates) > 0) {
      cat("  - Contains duplicates:", paste(duplicates, collapse = ", "), "\n")
    }
    
    # Check for values outside expected range
    out_of_range <- actual_order[actual_order < 1 | actual_order > n_rows]
    if (length(out_of_range) > 0) {
      cat("  - Contains values outside range 1 to", n_rows, ":", paste(out_of_range, collapse = ", "), "\n")
    }
    
    # Check for missing numbers in sequence
    missing_numbers <- setdiff(expected_sequence, actual_order)
    if (length(missing_numbers) > 0) {
      cat("  - Missing numbers:", paste(missing_numbers, collapse = ", "), "\n")
    }
    
    # Check for extra numbers
    extra_numbers <- setdiff(actual_order, expected_sequence)
    if (length(extra_numbers) > 0) {
      cat("  - Extra numbers:", paste(extra_numbers, collapse = ", "), "\n")
    }
    
    return(FALSE)
  }
}

# Run the check
check_order_sequence(author_sheet)


# Function to format middle initial
format_middle_initial <- function(middle_initial) {
  if (is.na(middle_initial) || middle_initial == "") {
    return("")
  }
  
  # Check if it's all uppercase letters
  if (str_detect(middle_initial, "^[A-Z]+$")) {
    if (nchar(middle_initial) == 1) {
      return(paste0(" ", middle_initial, "."))
    } else {
      # Split each letter and add periods
      letters <- str_split(middle_initial, "")[[1]]
      return(paste0(" ", paste(letters, collapse = "."), "."))
    }
  } else {
    # Mix of upper and lowercase - print as is
    return(paste0(" ", middle_initial))
  }
}

# Function to create formatted author list with actual superscripts
create_formatted_author_list <- function(author_sheet) {
  
  # Order authors by Order column, starting from 1
  authors_ordered <- author_sheet %>%
    filter(!is.na(Order)) %>%
    arrange(Order)
  
  # Split affiliations by semicolon and create affiliation mapping
  affiliation_list <- c()
  author_affiliations <- list()
  
  for (i in seq_len(nrow(authors_ordered))) {
    # Split affiliations by semicolon
    affiliations <- str_split(authors_ordered$Affiliation[i], ";")[[1]]
    affiliations <- str_trim(affiliations)  # Remove leading/trailing whitespace
    
    # Get affiliation numbers for this author
    affil_numbers <- c()
    for (affil in affiliations) {
      if (!affil %in% affiliation_list) {
        affiliation_list <- c(affiliation_list, affil)
      }
      affil_numbers <- c(affil_numbers, which(affiliation_list == affil))
    }
    
    # Sort affiliation numbers in increasing order
    affil_numbers <- sort(affil_numbers)
    author_affiliations[[i]] <- affil_numbers
  }
  
  # Create the formatted text using as_paragraph with as_sup for superscripts
  author_parts <- list()
  
  for (i in seq_len(nrow(authors_ordered))) {
    # Format author name
    first_name <- authors_ordered$`First name`[i]
    last_name <- authors_ordered$`Last name`[i]
    middle_initial <- format_middle_initial(authors_ordered$`Middle initial`[i])
    
    full_name <- paste0(first_name, middle_initial, " ", last_name)
    
    # Add author name
    author_parts <- append(author_parts, list(as_chunk(full_name)))
    
    # Add superscript affiliation numbers
    affil_numbers <- author_affiliations[[i]]
    superscript_text <- paste(affil_numbers, collapse = ",")
    author_parts <- append(author_parts, list(as_sup(superscript_text)))
    
    # Add comma and space after author (except for last author)
    if (i < nrow(authors_ordered)) {
      author_parts <- append(author_parts, list(as_chunk(", ")))
    }
  }
  
  # Create the paragraph with all parts
  author_paragraph <- do.call(as_paragraph, author_parts)
  
  # Create data frame for authors flextable
  content_df <- data.frame(
    Authors = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Create flextable and add the formatted paragraph
  authors_ft <- flextable(content_df) %>%
    delete_part(part = "header") %>%  # Remove header
    compose(i = 1, j = 1, value = author_paragraph) %>%
    width(width = 6.5) %>%  # Set width
    align(align = "left") %>%  # Left align
    fontsize(size = 11) %>%  # Set font size to 11
    font(fontname = "Arial") %>%  # Set font to Arial
    line_spacing(space = 1) %>%  # Single spacing
    border_remove()  # Remove all borders
  
  # Create affiliations as a single paragraph with line breaks
  affil_parts <- list()
  
  for (i in seq_along(affiliation_list)) {
    # Add superscript number
    affil_parts <- append(affil_parts, list(as_sup(as.character(i))))
    # Add the affiliation text
    affil_parts <- append(affil_parts, list(as_chunk(affiliation_list[i])))
    
    # Add line break after each affiliation (except the last one)
    if (i < length(affiliation_list)) {
      affil_parts <- append(affil_parts, list(as_chunk("\n")))
    }
  }
  
  # Create the affiliations paragraph
  affiliations_paragraph <- do.call(as_paragraph, affil_parts)
  
  # Create affiliations flextable
  affil_content_df <- data.frame(
    Affiliations = NA_character_,
    stringsAsFactors = FALSE
  )
  
  affiliations_ft <- flextable(affil_content_df) %>%
    delete_part(part = "header") %>%  # Remove header
    compose(i = 1, j = 1, value = affiliations_paragraph) %>%
    width(width = 6.5) %>%
    align(align = "left") %>%
    fontsize(size = 11) %>%  # Set font size to 11
    font(fontname = "Arial") %>%  # Set font to Arial
    line_spacing(space = 1) %>%  # Single spacing
    border_remove()
  
  return(list(
    authors_table = authors_ft, 
    affiliations_table = affiliations_ft, 
    affiliations = affiliation_list
  ))
}

# Generate the formatted author list
result <- create_formatted_author_list(author_sheet)

# Create document with single spacing throughout
doc <- read_docx() %>%
  body_add_flextable(result$authors_table) %>%
  body_add_par("") %>%
  body_add_flextable(result$affiliations_table)



# Save the document
output_filepath <- "/Users/hyang/HMS Dropbox/Helen Yang/BANCanalysis/formatted_author_list.docx"
print(doc, target = output_filepath)




# # Function to create formatted consortium list
# create_formatted_consortium_list <- function(consortium_sheet) {
#   
#   # Split affiliations by semicolon and create affiliation mapping
#   affiliation_list <- c()
#   member_affiliations <- list()
#   
#   for (i in seq_len(nrow(consortium_sheet))) {
#     # Split affiliations by semicolon
#     affiliations <- str_split(consortium_sheet$Affiliation[i], ";")[[1]]
#     affiliations <- str_trim(affiliations)  # Remove leading/trailing whitespace
#     
#     # Get affiliation numbers for this member
#     affil_numbers <- c()
#     for (affil in affiliations) {
#       if (!affil %in% affiliation_list) {
#         affiliation_list <- c(affiliation_list, affil)
#       }
#       affil_numbers <- c(affil_numbers, which(affiliation_list == affil))
#     }
#     
#     # Sort affiliation numbers in increasing order
#     affil_numbers <- sort(affil_numbers)
#     member_affiliations[[i]] <- affil_numbers
#   }
#   
#   # Create the formatted text using as_paragraph with as_sup for superscripts
#   member_parts <- list()
#   
#   for (i in seq_len(nrow(consortium_sheet))) {
#     # Use the Name column directly
#     member_name <- consortium_sheet$Name[i]
#     
#     # Add member name
#     member_parts <- append(member_parts, list(as_chunk(member_name)))
#     
#     # Add superscript affiliation numbers
#     affil_numbers <- member_affiliations[[i]]
#     superscript_text <- paste(affil_numbers, collapse = ",")
#     member_parts <- append(member_parts, list(as_sup(superscript_text)))
#     
#     # Add comma and space after member (except for last member)
#     if (i < nrow(consortium_sheet)) {
#       member_parts <- append(member_parts, list(as_chunk(", ")))
#     }
#   }
#   
#   # Create the paragraph with all parts
#   member_paragraph <- do.call(as_paragraph, member_parts)
#   
#   # Create data frame for members flextable
#   content_df <- data.frame(
#     Members = NA_character_,
#     stringsAsFactors = FALSE
#   )
#   
#   # Create flextable and add the formatted paragraph
#   members_ft <- flextable(content_df) %>%
#     delete_part(part = "header") %>%  # Remove header
#     compose(i = 1, j = 1, value = member_paragraph) %>%
#     width(width = 6.5) %>%  # Set width
#     align(align = "left") %>%  # Left align
#     fontsize(size = 11) %>%  # Set font size to 11
#     font(fontname = "Arial") %>%  # Set font to Arial
#     line_spacing(space = 1) %>%  # Single spacing
#     border_remove()  # Remove all borders
#   
#   # Create affiliations as a single paragraph with line breaks
#   affil_parts <- list()
#   
#   for (i in seq_along(affiliation_list)) {
#     # Add superscript number
#     affil_parts <- append(affil_parts, list(as_sup(as.character(i))))
#     # Add the affiliation text
#     affil_parts <- append(affil_parts, list(as_chunk(affiliation_list[i])))
#     
#     # Add line break after each affiliation (except the last one)
#     if (i < length(affiliation_list)) {
#       affil_parts <- append(affil_parts, list(as_chunk("\n")))
#     }
#   }
#   
#   # Create the affiliations paragraph
#   affiliations_paragraph <- do.call(as_paragraph, affil_parts)
#   
#   # Create affiliations flextable
#   affil_content_df <- data.frame(
#     Affiliations = NA_character_,
#     stringsAsFactors = FALSE
#   )
#   
#   affiliations_ft <- flextable(affil_content_df) %>%
#     delete_part(part = "header") %>%  # Remove header
#     compose(i = 1, j = 1, value = affiliations_paragraph) %>%
#     width(width = 6.5) %>%
#     align(align = "left") %>%
#     fontsize(size = 11) %>%  # Set font size to 11
#     font(fontname = "Arial") %>%  # Set font to Arial
#     line_spacing(space = 1) %>%  # Single spacing
#     border_remove()
#   
#   return(list(
#     members_table = members_ft, 
#     affiliations_table = affiliations_ft, 
#     affiliations = affiliation_list
#   ))
# }
# 
# # Generate the formatted consortium list
# result <- create_formatted_consortium_list(consortium_sheet)
# 
# # Create document with single spacing throughout
# doc <- read_docx() %>%
#   body_add_flextable(result$members_table) %>%
#   body_add_par("") %>%
#   body_add_flextable(result$affiliations_table)
# 
# print(doc, target = "/Users/hyang/HMS Dropbox/Helen Yang/BANCanalysis/formatted_consortium_list.docx")



# Function to create formatted consortium list (excluding SixEleven)
create_formatted_consortium_list <- function(consortium_sheet) {
  
  # Filter out SixEleven affiliations
  consortium_filtered <- consortium_sheet %>%
    filter(!str_detect(Affiliation, "SixEleven"))
  
  # Split affiliations by semicolon and create affiliation mapping
  affiliation_list <- c()
  member_affiliations <- list()
  
  for (i in seq_len(nrow(consortium_filtered))) {
    # Split affiliations by semicolon
    affiliations <- str_split(consortium_filtered$Affiliation[i], ";")[[1]]
    affiliations <- str_trim(affiliations)  # Remove leading/trailing whitespace
    
    # Get affiliation numbers for this member
    affil_numbers <- c()
    for (affil in affiliations) {
      if (!affil %in% affiliation_list) {
        affiliation_list <- c(affiliation_list, affil)
      }
      affil_numbers <- c(affil_numbers, which(affiliation_list == affil))
    }
    
    # Sort affiliation numbers in increasing order
    affil_numbers <- sort(affil_numbers)
    member_affiliations[[i]] <- affil_numbers
  }
  
  # Create the formatted text using as_paragraph with as_sup for superscripts
  member_parts <- list()
  
  for (i in seq_len(nrow(consortium_filtered))) {
    # Use the Name column directly
    member_name <- consortium_filtered$Name[i]
    
    # Add member name
    member_parts <- append(member_parts, list(as_chunk(member_name)))
    
    # Add superscript affiliation numbers
    affil_numbers <- member_affiliations[[i]]
    superscript_text <- paste(affil_numbers, collapse = ",")
    member_parts <- append(member_parts, list(as_sup(superscript_text)))
    
    # Add comma and space after member (except for last member)
    if (i < nrow(consortium_filtered)) {
      member_parts <- append(member_parts, list(as_chunk(", ")))
    }
  }
  
  # Create the paragraph with all parts
  member_paragraph <- do.call(as_paragraph, member_parts)
  
  # Create data frame for members flextable
  content_df <- data.frame(
    Members = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Create flextable and add the formatted paragraph
  members_ft <- flextable(content_df) %>%
    delete_part(part = "header") %>%  # Remove header
    compose(i = 1, j = 1, value = member_paragraph) %>%
    width(width = 6.5) %>%  # Set width
    align(align = "left") %>%  # Left align
    fontsize(size = 11) %>%  # Set font size to 11
    font(fontname = "Arial") %>%  # Set font to Arial
    line_spacing(space = 1) %>%  # Single spacing
    border_remove()  # Remove all borders
  
  # Create affiliations as a single paragraph with line breaks
  affil_parts <- list()
  
  for (i in seq_along(affiliation_list)) {
    # Add superscript number
    affil_parts <- append(affil_parts, list(as_sup(as.character(i))))
    # Add the affiliation text
    affil_parts <- append(affil_parts, list(as_chunk(affiliation_list[i])))
    
    # Add line break after each affiliation (except the last one)
    if (i < length(affiliation_list)) {
      affil_parts <- append(affil_parts, list(as_chunk("\n")))
    }
  }
  
  # Create the affiliations paragraph
  affiliations_paragraph <- do.call(as_paragraph, affil_parts)
  
  # Create affiliations flextable
  affil_content_df <- data.frame(
    Affiliations = NA_character_,
    stringsAsFactors = FALSE
  )
  
  affiliations_ft <- flextable(affil_content_df) %>%
    delete_part(part = "header") %>%  # Remove header
    compose(i = 1, j = 1, value = affiliations_paragraph) %>%
    width(width = 6.5) %>%
    align(align = "left") %>%
    fontsize(size = 11) %>%  # Set font size to 11
    font(fontname = "Arial") %>%  # Set font to Arial
    line_spacing(space = 1) %>%  # Single spacing
    border_remove()
  
  return(list(
    members_table = members_ft, 
    affiliations_table = affiliations_ft, 
    affiliations = affiliation_list
  ))
}

# Function to create SixEleven list (names only, no affiliations)
create_sixeleven_list <- function(consortium_sheet) {
  
  # Filter for SixEleven affiliations only
  sixeleven_members <- consortium_sheet %>%
    filter(str_detect(Affiliation, "SixEleven"))
  
  # Create comma-separated list of names
  if (nrow(sixeleven_members) > 0) {
    names_list <- paste(sixeleven_members$Name, collapse = ", ")
    
    # Create the paragraph
    names_paragraph <- as_paragraph(as_chunk(names_list))
    
    # Create data frame for flextable
    content_df <- data.frame(
      Names = NA_character_,
      stringsAsFactors = FALSE
    )
    
    # Create flextable
    names_ft <- flextable(content_df) %>%
      delete_part(part = "header") %>%  # Remove header
      compose(i = 1, j = 1, value = names_paragraph) %>%
      width(width = 6.5) %>%  # Set width
      align(align = "left") %>%  # Left align
      fontsize(size = 11) %>%  # Set font size to 11
      font(fontname = "Arial") %>%  # Set font to Arial
      line_spacing(space = 1) %>%  # Single spacing
      border_remove()  # Remove all borders
    
    return(names_ft)
  } else {
    return(NULL)
  }
}

# Generate the formatted consortium list (excluding SixEleven)
result <- create_formatted_consortium_list(consortium_sheet)

# Create main consortium document
doc <- read_docx() %>%
  body_add_flextable(result$members_table) %>%
  body_add_par("") %>%
  body_add_flextable(result$affiliations_table)

print(doc, target = "/Users/hyang/HMS Dropbox/Helen Yang/BANCanalysis/formatted_consortium_list.docx")

# Generate the SixEleven list
sixeleven_result <- create_sixeleven_list(consortium_sheet)

# Create SixEleven document
if (!is.null(sixeleven_result)) {
  sixeleven_doc <- read_docx() %>%
    body_add_flextable(sixeleven_result)
  
  print(sixeleven_doc, target = "/Users/hyang/HMS Dropbox/Helen Yang/BANCanalysis/formatted_SixEleven_list.docx")
  
  cat("Both documents created successfully!\n")
  cat("- Main consortium list (excluding SixEleven): formatted_consortium_list.docx\n")
  cat("- SixEleven members only: formatted_SixEleven_list.docx\n")
} else {
  cat("Main consortium document created. No SixEleven members found.\n")
}
