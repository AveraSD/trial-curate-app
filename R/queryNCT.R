library(httr)
library(jsonlite)
library(glue)
library(tidyverse)


# function to get trial info from the API in json format
nctApi <- function(nct_id) {
  
  # API info and query params
  #  base_url <- "https://clinicaltrials.gov/"
  
  
  #block for classic code - good 
  # base_url <- "https://classic.clinicaltrials.gov/"
  # path <- "api/query/full_studies"
  # min_rnk <- 1
  # max_rnk <- 1
  # fmt <- "json"
  # 
  # # generate trial specific API request
  # request <- list(expr = nct_id,
  #                 min_rnk = min_rnk,
  #                 max_rnk = max_rnk,
  #                 fmt = fmt)
  #block for classic good
  
  base_url <- "https://www.clinicaltrials.gov/"
  path <- "api/v2/studies"
  
  format <- "json" 
  
  #fmt <- "json"
  # NCTId <- "NCT02428712"
  # generate trial specific API request
  request <- list(filter.ids = nct_id,
                  #   min_rnk = min_rnk,
                  #  max_rnk = max_rnk,
                  format = format)  
               
  
  
  # query the API
  nct_json <- GET(url = base_url,
                  path = path,
                  query = request)
  
  # check for API errors
  if (http_error(nct_json)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        status_code(nct_json),
        nct_parsed$message,
        nct_parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  # check if API returned a json file - API says "text/plain" even though it is json format so check does not work
  # if (http_type(nct_json) != "application/json") {
  #   stop("API did not return data in json format.", call. = FALSE)
  # }
  
  
  # return parsed json 
  nct_parsed <- fromJSON(content(nct_json, "text"), simplifyVector = T)
  nct_parsed
}



# function to parse json file returned by the API
# parseNct <- function(nct_id, nct_parsed) {
#   
#   # nct_id <- parsed_json$NCT
#   # nct_parsed <- api_out
#   
#   # study and study modules
#   study <- nct_parsed$FullStudiesResponse$FullStudies$Study$ProtocolSection
#   
#   id = study$IdentificationModule
#   status = study$StatusModule
#   description = study$DescriptionModule
#   design = study$DesignModule
#   elig = study$EligibilityModule
# 
#   #  generate arm group info - cohort label, drugs used, type of arm
#   arms = study$ArmsInterventionsModule
#   
#   arm_groups <- function(x) {
#     cohorts <- x$ArmGroupList$ArmGroup[[1]]$ArmGroupLabel
#     dru <- unite(data = (x$ArmGroupList$ArmGroup[[1]]$ArmGroupInterventionList %>% 
#                          unnest_wider(col = "ArmGroupInterventionName",names_sep = ":")), 
#                  col = "drug", 
#                  sep = " | ")
#     armtype <- x$ArmGroupList$ArmGroup[[1]]$ArmGroupType
#     armdf <- bind_cols(cohortlabel = cohorts, dru, arm_type = armtype)
#     
#     # clean up medication labels
#     pat <- c("Drug: |Biological: |Dietary Supplement: ")
#     armdf$drug <- str_replace_all(armdf$drug, pattern = pat, replacement = "")
#     return(armdf)
#   }
# 
#   # check if nct_id returned is the same as that queried
#   if (nct_id != id$NCTId) {
#     stop("NCT ID requested was not returned by the API.", call. = FALSE)
#   }
#   
#   # create tibble with parsed info  
#   out <- tibble(
#     
#     # study identification
#     nct = id$NCTId,
#     title = id$BriefTitle,
# 
#     # study status
#     current_status = status$OverallStatus,
#     status_verif_date = status$StatusVerifiedDate,
#     last_update_date = status$LastUpdateSubmitDate,
#     
#     # study sponsor
#     sponsor = study$SponsorCollaboratorsModule$LeadSponsor$LeadSponsorName,
#     
#     # study description
#     brief_summary = description$BriefSummary,
#     
#     # study disease conditions
#     conditiions = glue_collapse(study$ConditionsModule$ConditionList[[1]][[1]], sep = " | "),
#     
#     # study design
#     type = design$StudyType,
#     phase = glue_collapse(design$PhaseList$Phase[[1]], sep = " | "),
#     
#     # study arms
#     ### v1
#     # arm_group_label = arms$ArmGroupList$ArmGroup[[1]] %>% select(ArmGroupLabel),
#     # arm_group_type = arms$ArmGroupList$ArmGroup[[1]] %>% select(ArmGroupType),
#     
#     ### v2
#     # arm_group = arms$ArmGroupList$ArmGroup[[1]] %>% 
#     #   select(ArmGroupLabel, ArmGroupType) %>% 
#     #   unite("arm_group", ArmGroupLabel:ArmGroupType, sep = ":") %>% 
#     #   glue_collapse(sep = " | "),
#     
#     ### v3
#     # if this is a named list, out is a 1-row tbl and can use unnest_longer(arm) to get each "cohort" in its own line, 
#     # if this is simply used as arm_groups(arms), then out is a (no. of cohorts)-row tbl i.e., already in the unnested form
#     arm = list("arm" = arm_groups(arms)), 
# 
#     
#     # study eligibility
#     min_age = elig$MinimumAge,
#     criteria = elig$EligibilityCriteria,
#     gender = elig$Gender,
#     
#     # hyperlink to NCT ID # note: target="blank" is the HTML way to open a link in a new tab
#     link = glue("<a href=\"https://clinicaltrials.gov/ct2/show/", nct_id, "\" target=\"_blank\">", nct_id, "</a>")
# 
#   )
#   
#   out
#   
# }


# function to parse json file returned by the API
parseNct <- function(nct_id, nct_parsed) {
  #new codes for extraction
  
  #######################################################old working block for classic 
  
  # study and study modules
  #study <- nct_parsed$FullStudiesResponse$FullStudies$Study$ProtocolSection   #commented in old working block not usable
  
  
  
  #studynew <- nct_parsed$studies$protocolSection
  study <- nct_parsed$studies$protocolSection
  
  # identification = studynew$identificationModule
  id = study$identificationModule
  
  
  #status = studynew$statusModule
  status = study$statusModule
  
  
  #description = studynew$descriptionModule
  description = study$descriptionModule
  
  
  #design = studynew$designModule
  design = study$designModule
  
  
  
  #elig = studynew$eligibilityModule
  elig = study$eligibilityModule
  ###
  
  #arms = studynew$armsInterventionsModule
  arms = study$armsInterventionsModule
  ####
  
  condi = study$conditionsModule
  
  #arms = nct_parsed$studies$protocolSection$armsInterventionsModule  - detailed form
  
  arm_groups <- function(x) {
    cohorts <- x$armGroups[[1]]$label 
    #dru <- unite(data = (x$armGroups[[1]]  %>% <<< original working
    
    # unnest_wider(col = "interventionNames")), <<<< original working
    #     col = "drug", 
    #    sep = " | ") 
    
    
    dru<- unite(data = x$armGroups[[1]] %>% select(interventionNames), col = "drug", sep = "|")
    
    
    
    #       dru <- unite(data = (x$interventions[[1]]  %>% 
    #                             unnest_wider(col = "name")),              
    
    
    armtype <- x$armGroups[[1]]$type
    armdf <- bind_cols(cohortlabel = cohorts, dru, arm_type = armtype)
    
    # clean up medication labels
    pat <- c("Drug: |Biological: |Dietary Supplement: ")
    armdf$drug <- str_replace_all(armdf$drug, pattern = pat, replacement = "")
    return(armdf)
    
    
    
    
    # nct_id <- parsed_json$NCT
    # nct_parsed <- api_out
    
    # study and study modules
    # study <- nct_parsed$FullStudiesResponse$FullStudies$Study$ProtocolSection
    # 
    # id = study$IdentificationModule
    # status = study$StatusModule
    # description = study$DescriptionModule
    # design = study$DesignModule
    # elig = study$EligibilityModule
    # 
    # #  generate arm group info - cohort label, drugs used, type of arm
    # arms = study$ArmsInterventionsModule
    # 
    # arm_groups <- function(x) {
    #   cohorts <- x$ArmGroupList$ArmGroup[[1]]$ArmGroupLabel
    #   dru <- unite(data = (x$ArmGroupList$ArmGroup[[1]]$ArmGroupInterventionList %>% 
    #                          unnest_wider(col = "ArmGroupInterventionName")), 
    #                col = "drug", 
    #                sep = " | ")
    #   armtype <- x$ArmGroupList$ArmGroup[[1]]$ArmGroupType
    #   armdf <- bind_cols(cohortlabel = cohorts, dru, arm_type = armtype)
    #   
    #   # clean up medication labels
    #   pat <- c("Drug: |Biological: |Dietary Supplement: ")
    #   armdf$drug <- str_replace_all(armdf$drug, pattern = pat, replacement = "")
    #   return(armdf)
  }
  
  # check if nct_id returned is the same as that queried
  if (nct_id != id$nctId) {
    stop("NCT ID requested was not returned by the API.", call. = FALSE)
  }
  
  # create tibble with parsed info  
  out <- tibble(
    
    nct = id$nctId,
    title = id$officialTitle,
    current_status = status$overallStatus,
    status_verif_date = status$statusVerifiedDate,
    
    
    
  #  last_update_date = status$LastUpdateSubmitDate,
    
   last_update_date = status$lastUpdateSubmitDate,
    
    
    #  sponsor = nct_parsed$studies$protocolSection$sponsorCollaboratorsModule$leadSponsor$name,
    sponsor = study$sponsorCollaboratorsModule$leadSponsor$name,
    brief_summary = description$briefSummary,
    #  condiitions = glue_collapse(nct_parsed$studies$protocolSection$conditionsModule$conditions[[1]], sep = "|"),
   
    
   #  conditiions = glue_collapse(study$conditionsModule$conditions[[1]], sep = "|"),
    
  conditiions = glue_collapse(condi$conditions[[1]], sep = "|"),
    
    
    type = design$studyType,
    
    phase = glue_collapse(design$phases[[1]], sep = "|"),
    
    arm = list("arm" = arm_groups(arms)), 
    min_age = elig$minimumAge,
    criteria = elig$eligibilityCriteria,
    gender = elig$sex,
    
    
    # studyid = identification$orgStudyIdInfo$id,
    # 
    # 
    # 
    # 
    # 
    # description = description$detailedDescription,
    # 
    # 
    # 
    # #Newly added - use it if needed
    # last_update_post_date = status$LastUpdatePostDateStruct$LastUpdatePostDate,
    # 
    
    # last_update_postdate = status$lastUpdatePostDateStruct$date
    #  last_update_date = status$
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # cohortlabel = nct_parsed$studies$protocolSection$armsInterventionsModule$armGroups[[1]]$label 
    # 
    # # wrong - intrevenlist = nct_parsed$studies$protocolSection$armsInterventionsModule$armGroups[[1]]$interventionNames[[1]] %>% unlist()
    # 
    # armtype = nct_parsed$studies$protocolSection$armsInterventionsModule$armGroups[[1]]$type
    # 
    # nct_parsed$studies$protocolSection$armsInterventionsModule$armGroups[[1]] %>% 
    #   unnest_wider(col = "interventionNames")   ### correct one
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # study identification
    # nct = id$NCTId,
    # title = id$BriefTitle,
    # 
    # # study status
    # current_status = status$OverallStatus,
    # status_verif_date = status$StatusVerifiedDate,
    # last_update_date = status$LastUpdateSubmitDate,
    # 
    # #Newly added - use it if needed
    # last_update_post_date = status$LastUpdatePostDateStruct$LastUpdatePostDate,
    # 
    # #currently you only need to update the following three which is in browse app
    # #current_status, status_verif_date and last_update_date
    # 
    # # study sponsor
    # sponsor = study$SponsorCollaboratorsModule$LeadSponsor$LeadSponsorName,
    # 
    # # study description
    # brief_summary = description$BriefSummary,
    # 
    # # study disease conditions
    # conditiions = glue_collapse(study$ConditionsModule$ConditionList[[1]][[1]], sep = " | "),
    # 
    # # study design
    # type = design$StudyType,
    # phase = glue_collapse(design$PhaseList$Phase[[1]], sep = " | "),
    # 
    # # study arms
    # ### v1
    # # arm_group_label = arms$ArmGroupList$ArmGroup[[1]] %>% select(ArmGroupLabel),
    # # arm_group_type = arms$ArmGroupList$ArmGroup[[1]] %>% select(ArmGroupType),
    # 
    # ### v2
    # # arm_group = arms$ArmGroupList$ArmGroup[[1]] %>% 
    # #   select(ArmGroupLabel, ArmGroupType) %>% 
    # #   unite("arm_group", ArmGroupLabel:ArmGroupType, sep = ":") %>% 
    # #   glue_collapse(sep = " | "),
    # 
    # ### v3
    # # if this is a named list, out is a 1-row tbl and can use unnest_longer(arm) to get each "cohort" in its own line, 
    # # if this is simply used as arm_groups(arms), then out is a (no. of cohorts)-row tbl i.e., already in the unnested form
    # arm = list("arm" = arm_groups(arms)), 
    # 
    # 
    # # study eligibility
    # min_age = elig$MinimumAge,
    # criteria = elig$EligibilityCriteria,
    # gender = elig$Gender,
    
    
    
    
    # hyperlink to NCT ID # note: target="blank" is the HTML way to open a link in a new tab
    link = glue("<a href=\"https://clinicaltrials.gov/ct2/show/", nct_id, "\" target=\"_blank\">", nct_id, "</a>")
    
  )
  
  out
  
}








# function to query an NCT ID against the clinicaltrials.gov API
queryNct <- function(nct_id) {
  api_out <- nctApi(nct_id)
  out_tbl <- parseNct(nct_id, api_out)
  out_tbl
}





