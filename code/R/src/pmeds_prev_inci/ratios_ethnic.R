
fp <- paste0(analysis_data_dir,'prev_age_chapter_stacked_ethnicity_description_2023.csv')

dt_in <- fread(fp)



dt_chapters <- get_chapter_colors()
dt_chapters <- dt_chapters[BNF_CHAPTER_CODE < 16]
chapters_f <- rev(dt_chapters$chapter_text)

keep_ages <- c(5,10,20,30,40,50,60,70,80,90)

b = c("Any other Asian background","Bangladeshi","Black","Caribbean","Chinese",
      "Indian","Mixed","Other","Pakistani")

#b = c("Pakistani")

a <- "White"  


dt_x <- dt_in[strat_value == 'White' & age %in% keep_ages]

for(e in b){
  
  #e = "Pakistani"
  print(e)
  dt_y <- dt_in[strat_value == e & age %in% keep_ages]
  dt_out <- merge(x= dt_x,y = dt_y, by = c("prescribed_chapter_num","age" ), all.x=T)
  
  dt_out$rr <- dt_out$rate.y/dt_out$rate.x
  keep_cols <- c("prescribed_chapter_num","chapter_text.x","age","rate.x","rate.y","rr")
  dt_out <- dt_out[,..keep_cols]
  
  dt_out$age <- factor(dt_out$age,levels = keep_ages)
  
  dt_out$chapter_text.x <- factor(dt_out$chapter_text.x,levels = chapters_f)
  
  setnames(dt_out,c('chapter_text.x','age','rr'), c('Chapter','Age','Ratio'))
  
  dt_out[is.na(dt_out)] <- 1
  dt_out <- dt_out[Ratio > 5, Ratio := 5]
  
  #dt_non <- dt_out[prescribed_chapter_num != 14]
  #mid_value <- (1/max(dt_non$Ratio))
  mid_value <- (1/max(dt_out$Ratio))
  
  
  
  print(mid_value)
  print(paste0(min(dt_out$Ratio),' - ',max(dt_out$Ratio)))
  #print(paste0(min(dt_non$Ratio),' - ',max(dt_non$Ratio)))
  
  g <- ggplot(dt_out,aes(Age,Chapter,fill=Ratio)) +
    geom_tile() +
    coord_fixed()+ 
    scale_fill_gradientn(
      colours = c("blue", "white", "red"),
      values = c(0,mid_value,1)
    ) +
    theme(axis.text.y = element_blank())
  #scale_fill_steps(breaks = bks)
  # scale_x_discrete(expand = c(0,0))
  #scale_fill_distiller(palette = "RdPu")
  
  plot_save_name <- paste0(analysis_plots_dir,'imd_rates_heat_',substr(e,1,3),'.pdf')
  
  #print(plot_save_name)
  ggsave(file=plot_save_name, g, height = 7, width=7) #saves g
  print('---')
}
