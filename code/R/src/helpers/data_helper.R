add_date_col <- function(dt){
  dt$date <- as.Date(paste(dt$month, '01', dt$year,sep="-"),format="%m-%d-%Y")
  setkey(dt,year,month)
  dt
}


ethnic_desc_collapse = function(dt,col=''){
  
  if(col != ''){
    setnames(dt,col,'ethnicity_description')
  }
  
  dt <- dt[is.na(ethnicity_description), ethnicity_description := 'Missing']
  dt <- dt[ethnicity_description == 'Not stated', ethnicity_description := 'Missing']
  dt <- dt[ethnicity_description == '', ethnicity_description := 'Missing']
  
  dt <- dt[ethnicity_description == "Any other ethnic group", ethnicity_description := 'Other']
  dt <- dt[ethnicity_description == "Arab", ethnicity_description := 'Other']
  
  dt <- dt[ethnicity_description == "Any other mixed background", ethnicity_description := 'Mixed']
  dt <- dt[ethnicity_description == "White and Black African", ethnicity_description := 'Mixed']
  dt <- dt[ethnicity_description == "White and Black Caribbean", ethnicity_description := 'Mixed']
  dt <- dt[ethnicity_description == "White and Asian", ethnicity_description := 'Mixed']
  
  
  dt <- dt[ethnicity_description == "Any other Black background", ethnicity_description := 'Black']
  #dt <- dt[ethnicity_description == "Caribbean", ethnicity_description := 'Black']
  dt <- dt[ethnicity_description == "African", ethnicity_description := 'Black']
  
  dt <- dt[ethnicity_description == "Irish", ethnicity_description := 'White']
  dt <- dt[ethnicity_description == "British", ethnicity_description := 'White']
  dt <- dt[ethnicity_description == "Traveller", ethnicity_description := 'White']
  dt <- dt[ethnicity_description == "Any other White background", ethnicity_description := 'White']
  
  if(col != ''){
    setnames(dt,'ethnicity_description',col)
  }
  
  dt
  ##dt <- dt[, .(count=sum(count)), by=list(year,month,strat_key)]
  
}

ethnic_desc_collapse_one = function(dt,col=''){
  
  if(col != ''){
    setnames(dt,col,'ethnicity_description')
  }
  
  dt <- dt[is.na(ethnicity_description), ethnicity_description := 'Missing']
  dt <- dt[ethnicity_description == 'Not stated', ethnicity_description := 'Missing']
  dt <- dt[ethnicity_description == '', ethnicity_description := 'Missing']
  
  dt <- dt[ethnicity_description == "Any other ethnic group", ethnicity_description := 'Other']
  dt <- dt[ethnicity_description == "Arab", ethnicity_description := 'Other']
  
  dt <- dt[ethnicity_description == "Any other mixed background", ethnicity_description := 'Mixed']
  dt <- dt[ethnicity_description == "White and Black African", ethnicity_description := 'Mixed']
  dt <- dt[ethnicity_description == "White and Black Caribbean", ethnicity_description := 'Mixed']
  dt <- dt[ethnicity_description == "White and Asian", ethnicity_description := 'Mixed']
  
  
  dt <- dt[ethnicity_description == "Any other Black background", ethnicity_description := 'Black']
  dt <- dt[ethnicity_description == "Caribbean", ethnicity_description := 'Black']
  dt <- dt[ethnicity_description == "African", ethnicity_description := 'Black']
  
  dt <- dt[ethnicity_description == "Irish", ethnicity_description := 'White']
  dt <- dt[ethnicity_description == "British", ethnicity_description := 'White']
  dt <- dt[ethnicity_description == "Traveller", ethnicity_description := 'White']
  dt <- dt[ethnicity_description == "Any other White background", ethnicity_description := 'White']
  
  if(col != ''){
    setnames(dt,'ethnicity_description',col)
  }
  
  dt
  ##dt <- dt[, .(count=sum(count)), by=list(year,month,strat_key)]
  
}


age_cut <- function(dtx){
  
  age= dtx$age
  out = ''
  
  if (is.na(age)){
    out = ''
  } else if (age == 0){
    out = '0'
  } else if (age >= 1 & age <= 4){
    out = '1-4'
  } else if (age >= 5 & age <= 9){
    out = '5-9'
  } else if (age >= 10 & age <= 14) {
    out = '10-14'
  }else if (age >= 15 & age <= 19) {
    out = '15-19'
  }else if (age >= 20 & age <= 29){
    out = '20-19'
  } else if (age >= 30 & age <= 39) {
    out = '30-39'
  } else if (age >= 40 & age <= 49) {
    out = '40-49'
  } else if (age >= 50 & age <= 59) {
    out = '50-59'
  }else if (age >= 60 & age <= 69) {
    out = '60-69'
  }else if (age >= 70 & age <= 79) {
    out = '70-79'
  }else if (age >= 80 & age <= 89) {
    out = '80-89'
  } else if (age >= 90) {
    out = '90+'
  }else{
    out = ''
  }
  
  out
  
}
