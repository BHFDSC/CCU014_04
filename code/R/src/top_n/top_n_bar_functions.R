
get_data_top_n <- function(out_file,dt_bnf,dt_colors){
  
  dt <- fread(out_file)
  
  dt <- merge (x=dt, y=dt_bnf, by.x='PrescribedBNFCode', by.y='BNF_PRESENTATION_CODE', x.all=FALSE, y.all=FALSE)
  dt <- merge (x=dt, y=dt_colors, by.x='BNF_CHAPTER_CODE', by.y='BNF_CHAPTER_CODE', x.all=FALSE, y.all=FALSE)
  
  #dt[,c('BNF_CHEMICAL_SUBSTANCE_CODE','BNF_PARAGRAPH_CODE','BNF_PARAGRAPH', 'BNF_SUBPARAGRAPH_CODE','BNF_SECTION_CODE' ):=NULL]
  
  dt <- dt[year > 2018]
  dt[ , combined_sex := c("Unknown", "Unknown", "Male", "Female")[match(combined_sex, c(0,9,1,2))] ]
  dt <- dt[!is.na((dt$combined_age_band))]
  dt
  
}

get_plot <- function(dt_x,sex_label){
  
  labels_y <- dt_x$label_text
  
  max_x <- ceiling(max(dt_x$count))
  
  g <- ggplot(dt_x) +
    geom_col(aes(count, as.factor(-id)), fill = dt_x$chapter,alpha=0.5, width= 1 ,position=position_dodge(.7),colour="black") +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(color = "#ebebeb", linewidth  = 0.3),
      
    ) +
    
    labs(y='',x= paste0(sex_label, ' Counts')) +
    
    scale_x_continuous(
      limits = c(0, max_x),
      breaks = seq(0, max_x, by = 1000000), 
      expand = c(0, 0), # The horizontal axis does not extend to either side
      sec.axis = dup_axis(),
      labels= addUnits
    ) +
    
    geom_text(
      data = dt_x,
      aes(100000, y = as.factor(-id), label = label_text),
      hjust = 0,
      nudge_x = 0.3
    ) 
  
  ab_len <- length((age_bands))
  for(i in seq(1:ab_len)){
    #
    
    idx <- ab_len + 1 -i
    
    g <- g + geom_hline(yintercept= (i*top_n) +0.5,linetype = "dashed") +
      geom_text(x=8e+06, label=age_bands[idx], 
                y= (i*top_n) -2.5, size = 6 ,colour="black")
    
    
  }
  
  g
  
}

