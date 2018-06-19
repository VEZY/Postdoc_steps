#' Extract variable from source data
#'
#' @description Helper function to extract the data from the source 
#' observation files
#'
#' @param df_obs Source data.frame
#' @param Var    Character vector. Variable to extract
#' @param crop   Crop type to consider
#'
#' @examples
#'\dontrun{
#' extract_var(df_obs= obs, Var= "rie.",crop= "IC_W_P_nefer_lucy_0.5_0.5_N0_F0")
#'}
#' @export
#'
extract_var= function(df_obs,Var,crop){
  df_obs2= df_obs[df_obs$code_crop==crop,]%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  df_obs2_sd= df_obs[df_obs$code_crop==crop,]%>%
    summarise_if(is.numeric, sd, na.rm = TRUE)
  tmp= df_obs2[,grep(Var,colnames(df_obs2))]
  tmp2= df_obs2_sd[,grep(Var,colnames(df_obs2_sd))]
  Dates= as.POSIXct(gsub(Var,"",colnames(tmp)),
                    format="%Y.%m.%d", tz="UTC")
  data.frame(Date= Dates, value= t(tmp), sd= t(tmp2),row.names = NULL)
}


#' Write STICS obs data from data.frame
#'
#' @description Write STICS obs data from data.frame
#'
#' @param x       Data.frame to write
#' @param obspath Path to the file to write to
#'
#' @examples
#'\dontrun{
#' write_obs(obs, "wheat.obs")
#'}
#' @export
#'
write_obs= function(x,obspath){
  x= x[,-grep("Date|Plant",colnames(x))]
  x= data.frame(x[,grep("ian|mo|jo|jul",colnames(x))],
                x[,-grep("ian|mo|jo|jul",colnames(x))])
  
  colnames(x)= gsub("_n","(n)",colnames(x))
  colnames(x)[grep("_[0-9]$",colnames(x))]=
    gsub("$",")",gsub("_","(",colnames(x)[grep("_[0-9]$",colnames(x))]))
  colnames(x)[grep("_[0-9]_sd$",colnames(x))]=
    gsub("_sd$",")_sd",gsub("_","(",colnames(x)[grep("_[0-9]_sd$",colnames(x))]))
  
  splitted_sd= strsplit(colnames(x)[grep("_[0-9]_sd$",colnames(x))],"_")
  colnames(x)[grep("_[0-9]_sd$",colnames(x))]=
    lapply(splitted_sd, function(z){
      z= paste(c(paste(z[1:2], collapse = "("),z[3]), collapse = ")_")
    })
  
  write.table(x,obspath,sep=";",na="-999.99",row.names= F, quote=F)
}


#' Merge source data with obs
#'
#' @description Function to merge observation from \code{\link{extract_var}}
#' to the observation file from \code{\link[sticRs]{read_obs}}.
#'
#' @param x   Data.frame from \code{\link{extract_var}}
#' @param y   Data.frame from \code{\link[sticRs]{read_obs}}
#' @param Var The variable name to put in the merged table
#'
#' @examples
#'\dontrun{
#' write_obs(obs, "wheat.obs")
#'}
#' @export
#'
merge_obs= function(x,y,Var){
  x$sd= round(x$sd, 3)
  colnames(x)= c("Date",Var,paste0(Var,"_sd"))
  merge(y,x,all.x = T)
}


#' Render the Auzeville report
#'
#' @description Render the Auzeville report using simulation outputs
#'  and observations
#'
#' @param Report_dir  Report Rmarkdown file path
#' @param rootdir     The root directory of the project
#'
#' @examples
#'\dontrun{
#' Render_Auzeville()
#'}
#' @export
#'
Render_Auzeville= function(Report_dir="0-DATA/report_templates/Auzeville.Rmd",
                           rootdir= getwd(),Source_dir= "0-DATA/dummy",
                           Simulation_dir, Stics_exe_dir= "0-DATA/stics_executable",
                           obs_names= list(Mixed= c("6_IC_Wheat_N02.obs","6_IC_Pea_N02.obs"))
){
  # Function to change simulation working directory on rmarkdown before knitting it :
  reportrmd= readLines(Report_dir)
  ReadIndex= grep("knitr::opts_knit\\$set\\(root.dir",reportrmd)
  reportrmd[ReadIndex]= paste0('knitr::opts_knit$set(root.dir = "',rootdir,'")')
  writeLines(text = reportrmd, con = Report_dir)
}


#' Evaluate the STICS model on Auzeville
#'
#' @description Evaluate the STICS model using the Auzeville 
#' data, and make a report
#'
#' @param stics       SICS executable path(s)
#' @param dir.sim     Path to the directory for simulations
#' @param dir.report  Path to the directory for the report
#' @param name        Name of the report
#'
#' @export
#'
Eval_Auzeville= function(stics,dir.sim,dir.report,name,Parameter=NULL,Plant=1){
  
  
  # Wheat sole crop ---------------------------------------------------------
  
  SC_Wheat_Eval= 
    stics_eval(dir.orig = "0-DATA/dummy/SC_Wheat", 
               dir.targ = file.path(dir.sim,"Wheat"),
               stics= stics, Parameter=  Parameter, Plant= Plant,
               obs_name =  "Wheat_N0.obs",Parallel = T,
               Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
                           "hauteur", "cumraint", "fapar", "eai","tauxcouv(n)"),
               Title = "SC Wheat Auzeville 2005-2006 N0", plot_it = F)
  
  
  # Pea in sole crop --------------------------------------------------------
  
  SC_Pea_Eval= 
    stics_eval(dir.orig = "0-DATA/dummy/SC_Pea", 
               dir.targ = file.path(dir.sim,"Pea"),
               stics= stics, Parameter=  Parameter, Plant= Plant,
               obs_name =  "Pea_N0.obs",Parallel = T,
               Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
                           "hauteur", "cumraint", "fapar", "eai","tauxcouv(n)"),
               Title = "SC Pea Auzeville 2005-2006 N0", plot_it = F)
  
  
  # Wheat self-intercroping (Wheat-Wheat) -----------------------------------
  
  Wheat_Wheat_Eval= 
    stics_eval(dir.orig = "0-DATA/dummy/IC_Wheat_Wheat/", 
               dir.targ = file.path(dir.sim,"Wheat_Wheat"),
               stics= stics, Parameter=  Parameter, Plant= Plant,
               obs_name =  c("6_IC_Wheat1_N0.obs","6_IC_Wheat2_N0.obs"),
               Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
                           "hauteur", "cumraint", "fapar", "eai","tauxcouv(n)"),
               Title = "Wheat-Wheat Auzeville 2005-2006 N0", plot_it = F)
  
  
  
  # Pea intercropping (Pea-Pea) ---------------------------------------------
  
  Pea_Pea_Eval= 
    stics_eval(dir.orig = "0-DATA/dummy/IC_Pea_Pea", 
               dir.targ = file.path(dir.sim,"Pea_Pea"),
               stics= stics, Parameter=  Parameter, Plant= Plant,
               obs_name =  c("6_IC_Pea1_N0.obs","6_IC_Pea2_N0.obs"),
               Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
                           "hauteur", "cumraint", "fapar", "eai","tauxcouv(n)"),
               Title = "Pea-Pea Auzeville 2005-2006 N0", plot_it = F)
  
  # Intercropping: ----------------------------------------------------------
  
  Wheat_Pea_Eval= 
    stics_eval(dir.orig = "0-DATA/dummy/IC_Wheat_Pea/", 
               dir.targ = file.path(dir.sim,"Wheat_Pea"),
               stics= stics, Parameter=  Parameter, Plant= Plant,
               obs_name =  c("6_IC_Wheat_N02.obs","6_IC_Pea_N02.obs"),
               Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
                           "hauteur", "cumraint", "fapar", "eai","tauxcouv(n)"),
               Title = "Wheat-Pea Auzeville 2005-2006 N0", plot_it = F)
  
  
  # Report ------------------------------------------------------------------
  
  rmarkdown::render(input= '0-DATA/report_templates/Auzeville.Rmd',
                    output_dir= dir.report,
                    output_file = paste0(name,".html"),
                    params = list(sim_Wheat= SC_Wheat_Eval,
                                  sim_Pea= SC_Pea_Eval,
                                  sim_Wheat_Wheat= Wheat_Wheat_Eval,
                                  sim_Pea_Pea= Pea_Pea_Eval,
                                  sim_Wheat_Pea= Wheat_Pea_Eval,
                                  Out_var_plant= c("raint", "cumraint", "lai(n)", "masec(n)","hauteur", "eai"),
                                  Out_var_plot= c("fapar","tauxcouv(n)"))
  )
}





#' Multiple ggplots object
#'
#' @description Assemble multiple ggplot2 objects on one plot
#'
#' @param ...       ggplot object
#' @param plotlist  Alternative way of ggplot object input, as a list.
#' @param cols      Number of columns for plots disposition
#' @param layout    An optional layout for plot dispositions.
#' 
#' @details If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' 
#' @note This function is taken from the r cookbook at 
#' http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
#'
#' @export
#'
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}