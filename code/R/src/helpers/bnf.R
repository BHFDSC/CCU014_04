
get_chapter_colors <- function(){
  
  data <- data.table(chapter_color=c(
    '#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0',
    '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#000000', 
    '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#000000'
  ),
  
  BNF_CHAPTER_CODE = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,22,23),
  
  chapter_text = c("Gastro-Intestinal System" , "Cardiovascular System",                            
                   "Respiratory System" ,"Central Nervous System" ,                            
                   "Infections", "Endocrine System" , 
                   "Obstetrics, Gynaecology and Urinary-Tract Disorders",
                   "Malignant Disease and Immunosuppression","Nutrition and Blood",
                   "Musculoskeletal and Joint Diseases",            
                   "Eye" , "Ear, Nose and Oropharynx",
                   "Skin" ,"Immunological Products and Vaccines" ,      
                   "Anaesthesia" , "Preparations used in Diagnosis",         
                   "Other Drugs and Preparations" , "Dressings",
                   "Appliances","Incontinence Appliances",                    
                   "Stoma Appliances")
  )
  
  data
  
}


dt_swap_bnf_chapters <- function(dt_chapters,dt){
  
  
  dt <- merge (x=dt, y=dt_chapters, by.x='prescribed_chapter_num', 
               by.y='BNF_CHAPTER_CODE', 
               all.x = T)
  
  factor_levels <- dt_chapters$chapter_text
  dt$chapter_text <- factor(dt$chapter_text, levels = factor_levels)
  
  dt
  
}


ggplot_manual_chapters_colors <- function(){
  
  dt <- get_chapter_colors()
  my_list <- list()                     # Create empty list
  for(i in 1:nrow(dt)-1) { 
    r<- dt[i,]# Add key/value pairs in for-loop
    my_list[r$chapter_text] <- r$chapter_color
  }
  
  my_list
  
}

bnf_sections_list <- function(){
  dt_bnf_map <-  fread(paste0(data_lists_dir,'bnf_meds_cs.csv'),
                       colClasses = c(BNF_Section_Code = "character"))
  
  cols <- c('BNF_Section_Code','BNF_Section')
  dt_bnf_map <- dt_bnf_map[,..cols]
  dt_bnf_map <- dt_bnf_map[, head(.SD,1), by='BNF_Section_Code']
  setnames(dt_bnf_map,'BNF_Section_Code','prescribed_section_code')
  dt_bnf_map
  
}

bnf_para_list <- function(){
  dt_bnf_map <-  fread(paste0(data_lists_dir,'bnf_meds_cs.csv'),
                       colClasses = c(BNF_Paragraph_Code= "character"))
  
  cols <- c('BNF_Paragraph_Code','BNF_Paragraph')
  dt_bnf_map <- dt_bnf_map[,..cols]
  dt_bnf_map <- dt_bnf_map[, head(.SD,1), by='BNF_Paragraph_Code']
  setnames(dt_bnf_map,'BNF_Paragraph_Code','prescribed_para_code')
  dt_bnf_map
  
}


get_bnf_dt <- function(){
  
  bnf_name <-  paste0(getwd(),'/data/bnf_info.csv')
  dt <- fread(bnf_name,colClasses = c(BNF_SECTION_CODE = "character",
                                      BNF_PARAGRAPH_CODE = "character",
                                      BNF_SUBPARAGRAPH_CODE = "character"
  ))
  
}
