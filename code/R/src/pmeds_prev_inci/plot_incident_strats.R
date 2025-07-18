
years = c(2019,2020,2021,2022,2023,2024)
plot_dates =  c('2019-11-01','2024-09-30')
plot_dates_limits =  c(as.Date('2019-11-01', format = "%Y-%m-%d"),
                       as.Date('2024-09-30', format = "%Y-%m-%d"))

dt_chapters <- get_chapter_colors()

get_sex_data <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_age_chapter_sex_inci_linked.csv')
  print(fp_data)
  dt <- fread(fp_data)
  dt <- dt[!is.na(dt$strat_value)]
  
  dt$strat_value <- as.character(dt$strat_value)
  
  dt <- dt[strat_value == 1, strat_value := 'Male']
  dt <- dt[strat_value == 2, strat_value := 'Female']
  
  dt$strat_value = factor(dt$strat_value, levels = c("Male","Female"))
  dt
}


get_imd_data <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_age_chapter_imd_quintile_inci_linked.csv')
  print(fp_data)
  dt <- fread(fp_data)
  
  dt <- dt[!is.na(dt$strat_value)]
  
  strat_unique <- c(1,2,3,4,5)
  dt$strat_value = factor(dt$strat_value, levels=strat_unique)
  dt
}

get_imd_deci_data <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_age_chapter_imd_deci_inci_linked.csv')
  print(fp_data)
  dt <- fread(fp_data)
  
  dt <- dt[!is.na(dt$strat_value)]
  
  strat_unique <- c(1,2,3,4,5,6,7,8,9,10)
  dt$strat_value = factor(dt$strat_value, levels=strat_unique)
  dt
}




get_ethnic_data <- function(){
  
  ethnic_mapping <- fread(paste0(data_lists_dir,'ethnic_mapping.csv'))
  setnames(ethnic_mapping,'ethnicity','ethnicity_description')
  fp_data <- paste0(data_root_dir,'pmeds_age_chapter_ethnic_inci_linked.csv')
  print(fp_data)
  dt <- fread(fp_data)
  dt <- merge (x=dt, y=ethnic_mapping, by.x='strat_value', 
               by.y='value', 
               all.x = T)
  
  dt <- ethnic_desc_collapse(dt)
  dt <- dt[, .(count_pop=sum(count_pop)), by=list(year,month,ethnicity_description,prescribed_chapter_num)]
  
  setnames(dt,'ethnicity_description','strat_value')
  dt <- dt[!is.na(dt$strat_value)]
  
  dt <- dt[dt$strat_value != 'White']
  
  strat_unique <-  unique(dt$strat_value)
  dt$strat_value = factor(dt$strat_value, levels=strat_unique)
  dt
}


get_combined_age_band_data <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_age_chapter_age_band_inci_linked.csv')
  print(fp_data)
  dt <- fread(fp_data)
  
  dt <- dt[!is.na(dt$strat_value)]
  
  strat_unique <- age_bands
  dt$strat_value = factor(dt$strat_value, levels=strat_unique)
  dt
}


get_plot <- function(dt_c,plot_title,show_legend,s_colors){
  
  
  
  g <- ggplot(dt_c, aes(x=date, y=count_pop, group=strat_value, color=strat_value)) + geom_line() +
    
    scale_x_date(
      date_labels="%b %y",
      date_breaks  ="3 month",
      limits= plot_dates_limits,
      expand = c(0,0)
    ) +
    scale_y_continuous(labels = addUnits) +
    geom_vline(xintercept=as.numeric(covid_dates), colour="grey")+
    scale_color_manual( name="", values=s_colors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = paste0('Chapter : ',plot_title), y = "Incident Count", x = "Date")
  if(show_legend != ''){
    g <- g +  theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 27)))
    #g <- g + guides(fill = guide_legend(ncol = 3))
  }else{
    g <- g +  theme(legend.position = "none")
  }
  
  g
}


plot_inci_strat <- function(dt,s_colors,plot_main_title,plot_save_strat_name){
  
  all_plots <- list()
  chaps_seq <- seq(1,15)
  chaps_seq <- c(1, 4, 5, 7, 10, 13,2, 6, 9, 15,3, 8, 11, 12, 14)
  
  dt <- dt[year %in% years ]
  dt <- dt[prescribed_chapter_num <16]
  setkey(dt,prescribed_chapter_num,year,month)
  
  dt$date <- as.Date(paste(dt$month, '01', dt$year,sep="-"),format="%m-%d-%Y") 
  dt <- dt[date %between% plot_dates]
  
  dt <- merge (x=dt, y=dt_chapters, by.x='prescribed_chapter_num', by.y='BNF_CHAPTER_CODE', all.x = T)
  
  setkey(dt,year,month,prescribed_chapter_num)
  
  
  
  for(c_id in chaps_seq){
    #  stuff, such as
    print(c_id)
    
    
    dt_c <-dt[ dt$prescribed_chapter_num == c_id]
    cc <- unique(dt$chapter_color)
    plot_title <- unique(dt_c$chapter_text)
    
    g <- get_plot(dt_c,plot_title,'',s_colors)
    
    
    all_plots[[(length(all_plots) +1)]] <- g
    
  }
  
  p_legend <- get_plot(dt_c,'','b',s_colors)
  
  shared_legend <- extract_legend(p_legend)
  
  plot_save_name <- paste0(analysis_plots_dir,'/',plot_save_strat_name)
  plot_main_title <- ''
  g <- grid.arrange(arrangeGrob(grobs = all_plots, shared_legend,  ncol=3 ),
                    shared_legend, nrow = 2, heights = c(10, 1),
                    top = textGrob(plot_main_title,gp=gpar(fontsize=30,fontface = 'bold')))
  
  
  print(plot_save_name)
  ggsave(file=plot_save_name, g, height=15, width=15) #saves g
  
}


dt <- get_imd_data()
s_colors <- gradient_colors_5()
plot_main_title <- 'Incidents by IMD Quintile Nov 2019 - Sep 2024'
plot_save_strat_name <- 'incidents_imd_quintile_by_chapter.pdf'

plot_inci_strat(dt,s_colors,plot_main_title,plot_save_strat_name)

dt <- get_imd_deci_data()
s_colors <- gradient_colors()
plot_main_title <- 'Incidents by IMD DECI Nov 2019 - Sep 2024'
plot_save_strat_name <- 'incidents_imd_deci_by_chapter.pdf'

plot_inci_strat(dt,s_colors,plot_main_title,plot_save_strat_name)


dt <- get_sex_data()
s_colors <- palette_one()
plot_main_title <- 'Incidents by Sex Nov 2019 - Sep 2024'
plot_save_strat_name <- 'incidents_sex_by_chapter.pdf'

plot_inci_strat(dt,s_colors,plot_main_title,plot_save_strat_name)

dt <- get_ethnic_data()
s_colors <- palette_one()
plot_main_title <- 'Incidents by Ethnic Nov 2019 - Sep 2024'
plot_save_strat_name <- 'incidents_ethnic_by_chapter.pdf'

plot_inci_strat(dt,s_colors,plot_main_title,plot_save_strat_name)


dt <- get_combined_age_band_data()
s_colors <- palette_one()
plot_main_title <- 'Incidents by Age Band Jan 2019 - Dec 2023'
plot_save_strat_name <- 'incidents_age_band_by_chapter.pdf'

plot_inci_strat(dt,s_colors,plot_main_title,plot_save_strat_name)


