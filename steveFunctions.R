# Steve functions
#------------------------------ DCLS Analytes -------------------------------------------------------------------------

# Total Dissolved Nitrogen plus particulate nitrogen is used to calculate total nitrogen (TN=TDN+PN).
# Parameter short names (Pg_Parm_Short_Name) are blank for some rows and shouldn't be used to filter data. Use Pg_Storet_Code instead

# NITROGEN TOTAL, FIELD FILTERED, DISSOLVED,WTR MG/L =  49571
# NITROGEN PARTICULATE, FIELD FILT., SUSP., WTR MG/L =  49570
# PHOSPHOROUS TOTAL, FIELD FILTRED, DISSLVD,WTR MG/L =  49572
# PHOSPHOROUS PARTICULATE, FIELD FILT.,SUSP,WTR MG/L =  49567
# PERCENT SAND IN SEDIMENT ON A DRY WEIGHT BASIS =      82007
# CARBON, ORGANIC, IN BED MATERIAL (GM/KG AS C)  =      00687
# CHLOROPHYLL-A UG/L SPECTROPHOTOMETRIC ACID. METH.=    32211

# Need to make more concise & clean up with purrr

# param_names=c("Total_Nitrogen","Total_Phosphorus","Percent_Sand","TOC")  
# Parm_Codes=list(c("49571","49570"), c("49572","49567"),"8207","00687")

DCLS_Analytes_fun=function(x){
  
  
  TN= x %>%
    group_by(Ana_Sam_Mrs_Container_Id_Desc,Ana_Sam_Fdt_Id,Fdt_Sta_Id,Fdt_Date_Time)%>%
    filter(Pg_Storet_Code %in% c("49571","49570"))%>%
    summarise(Total_Nitrogen=sum(Ana_Value))
  
  TP= x %>%
    group_by(Ana_Sam_Mrs_Container_Id_Desc,Ana_Sam_Fdt_Id,Fdt_Sta_Id,Fdt_Date_Time)%>%
    filter(Pg_Storet_Code %in% c("49572","49567"))%>%
    summarise(Total_Phosphorus=sum(Ana_Value))
  
  Sand= x %>%
    group_by(Ana_Sam_Mrs_Container_Id_Desc,Fdt_Sta_Id,Fdt_Date_Time)%>%
    filter(Pg_Storet_Code %in% "82007")%>%
    summarise(Percent_Sand=max(Ana_Value))%>%
    mutate(Subtrate_Type=Percent_Sand_fun(Percent_Sand))
  
  TOC= x %>%
    group_by(Ana_Sam_Mrs_Container_Id_Desc,Fdt_Sta_Id,Fdt_Date_Time)%>%
    filter(Pg_Storet_Code %in% "00687")%>%
    summarise(TOC=max(Ana_Value))%>%
    mutate(TOC_Quality=TOC_fun(TOC))
  
  Chla= x %>%
    group_by(Ana_Sam_Mrs_Container_Id_Desc,Fdt_Sta_Id,Fdt_Date_Time)%>%
    filter(Pg_Storet_Code %in% "32211")%>%
    summarise(Chlorophyll=max(Ana_Value)) %>%
    mutate(Chlorophyll_Quality=NCCA_Chla_fun(Chlorophyll))
  
  #data.frame(plyr::join_all(list(TN,TP,Sand), type='left'))
  Nutrients=left_join(TN,TP)
  Sand_Toc=left_join(Sand,TOC)
  
  Total=left_join(Nutrients,Sand_Toc,by=c("Ana_Sam_Mrs_Container_Id_Desc","Fdt_Sta_Id","Fdt_Date_Time")) %>%
    left_join(Chla)
  
  return(Total)          
}