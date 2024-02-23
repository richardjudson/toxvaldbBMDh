#-----------------------------------------------------------------------------------
#' Calculate the BMDh values per study
#'
#' `bmdh.per.study` Calculates one BMDh value per study using the Aurisano algorithm.
#' Because EPA has not fully developed the mapping from critical effects in ToxValDB
#' to standardized effects, teh values from Aurisano are used where records match.
#' Aurisano used ToxValDB 9.1, whereas 9.5 is used here. There is also code here to do the other
#' required mappings, and thos may need to be updated.For records in both the old and new
#' databases, an on-the-fly plot is produced to show the corresponded between study-level
#' BMDh values.
#'
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @return Write a file with the results: toxval_PODs_for_BMDh {toxval.db} {sys.date}.xlsx
#' @export
#-----------------------------------------------------------------------------------
bmdh.per.study <- function(toxval.db="res_toxval_v95",sys.date="2024-02-23") {
  printCurrentFunction(toxval.db)
  dir = "data/"
  if(!exists("RES")) {
    file = paste0(dir,"results/ToxValDB for BMDh filtered ",toxval.db," ",sys.date,".xlsx")
    print(file)
    res = read.xlsx(file)
    RES <<- res
  }

  file = paste0(dir,"Aurisano S1.xlsx")
  print(file)
  s1 = read.xlsx(file)
  file = paste0(dir,"Aurisano S2.xlsx")
  print(file)
  s2 = read.xlsx(file)
  res = RES

  ttlist = c("NOAEL","LOAEL","NOEL","LOEL","BMD","BMDL (10)","BMDL (05)" ,"BMDL (10 HED)","BMDL (1SD)","BMDL (1SD HED)")
  res = res[is.element(res$toxval_type,ttlist),]
  # res = res[res$toxval_type=="BMDL",]
  s1[is.element(s1$tested_species_curated,"rat"),"tested_species_curated"] = "Rat"
  s1[is.element(s1$tested_species_curated,"rat*"),"tested_species_curated"] = "Rat"
  s1[is.element(s1$tested_species_curated,"mouse"),"tested_species_curated"] = "Mouse"
  s1[is.element(s1$tested_species_curated,"human"),"tested_species_curated"] = "Human"
  s1[is.element(s1$tested_species_curated,"dog"),"tested_species_curated"] = "Dog"
  s1[is.element(s1$tested_species_curated,"rabbit"),"tested_species_curated"] = "Rabbit"
  s1[s1$study_type_curated=="subacute","study_type_curated"] = "short-term"

  s2[is.element(s2$tested_species_curated,"rat"),"tested_species_curated"] = "Rat"
  s2[is.element(s2$tested_species_curated,"rat*"),"tested_species_curated"] = "Rat"
  s2[is.element(s2$tested_species_curated,"mouse"),"tested_species_curated"] = "Mouse"
  s2[is.element(s2$tested_species_curated,"human"),"tested_species_curated"] = "Human"
  s2[is.element(s2$tested_species_curated,"dog"),"tested_species_curated"] = "Dog"
  s2[is.element(s2$tested_species_curated,"rabbit"),"tested_species_curated"] = "Rabbit"

  s1$key = paste(s1$dtxsid,s1$source,s1$toxval_numeric,s1$tested_species_curated,s1$toxval_type_curated,s1$study_type_curated)
  s2$key = paste(s2$dtxsid,s2$source,s2$toxval_numeric,s2$tested_species_curated,s2$toxval_type_curated,s2$study_type_curated)
  s1 = unique(s1)
  s2 = unique(s2)
  res$study_type_standard = NA
  res$effect_category_standard = NA
  res$toxval_type_standard = NA
  res$conceptual_model_1 = "-"
  res$conceptual_model_2 = "-"
  res$conceptual_model_1_aurisano = "-"
  res$conceptual_model_2_aurisano = "-"
  res$bmdh1 = NA
  res$bmdh2 = NA
  res$bmdh = NA
  res$bmdh1_aurisano = NA
  res$bmdh2_aurisano = NA
  res$bmdh_aurisano = NA
  res$bmdh_ratio = NA
  res$F1 = NA
  res$F2 = NA
  res$F31 = NA
  res$F32 = NA
  res$F4 = NA
  res$F5 = NA

  file = paste0(dir,"effect category dictionary.xlsx")
  print(file)
  dict = read.xlsx(file)
  rownames(dict) = dict$critical_effect
  file = paste0(dir,"study type dictionary.xlsx")
  print(file)
  sdict = read.xlsx(file)
  rownames(sdict) = sdict$study_type_original

  file = paste0(dir,"conceptual model dictionary 2.xlsx")
  print(file)
  cdict = read.xlsx(file)
  res$key = NA
  nlist=c("dtxsid","casrn","name","source","toxval_type","toxval_type_standard","study_type","study_type_standard",
          "critical_effect",
          "effect_category_standard","conceptual_model_1","conceptual_model_2",
          "conceptual_model_1_aurisano","conceptual_model_2_aurisano",
          "bmdh1","bmdh2","bmdh",
          "bmdh1_aurisano","bmdh2_aurisano","bmdh_aurisano","bmdh_ratio",
          "F1","F2","F31","F32","F4","F5",
          "common_name","toxval_numeric","toxval_units",
          "toxval_numeric_qualifier",
          "study_duration_value","study_duration_units","study_duration_class",
          "exposure_route",
          "year","long_ref","url","source_hash",
          "study_group","key")
  res = res[,nlist]

  res[res$critical_effect=="-","effect_category_standard"] = "none"
  for(i in 1:nrow(res)) {
    x = res[i,"toxval_type"]
    if(is.element(res[i,"study_type"],sdict$study_type_original)) {
      st = sdict[res[i,"study_type"],"study_type"]
      res[i,"study_type"] = st
    }

    if(substr(x,1,1)=="N")  res[i,"toxval_type_standard"] = "NOAEL"
    else if(substr(x,1,1)=="L")  res[i,"toxval_type_standard"] = "LOAEL"
    else if(substr(x,1,4)=="BMDL")  res[i,"toxval_type_standard"] = "BMDL"
    else if(substr(x,1,3)=="BMD")  res[i,"toxval_type_standard"] = "BMD"
    else browser()

    x = res[i,"study_type"]
    if(x=="chronic") res[i,"study_type_standard"] = "chronic"
    else if(x=="developmental") res[i,"study_type_standard"] = "reproductive developmental"
    else if(x=="human") res[i,"study_type_standard"] = "subchronic"
    else if(x=="immunotoxicity") res[i,"study_type_standard"] = "subchronic"
    else if(x=="neurotoxicity") res[i,"study_type_standard"] = "subchronic"
    else if(x=="neurotoxicity chronic") res[i,"study_type_standard"] = "chronic"
    else if(x=="neurotoxicity short-term") res[i,"study_type_standard"] = "short-term"
    else if(x=="neurotoxicity subchronic") res[i,"study_type_standard"] = "subchronic"
    else if(x=="repeat dose other") res[i,"study_type_standard"] = "subchronic"
    else if(x=="reproduction") res[i,"study_type_standard"] = "reproductive developmental"
    else if(x=="reproduction developmental") res[i,"study_type_standard"] = "reproductive developmental"
    else if(x=="short-term") res[i,"study_type_standard"] = "short-term"
    else if(x=="subchronic") res[i,"study_type_standard"] = "subchronic"
    ce = res[i,"critical_effect"]
    if(is.element(ce,dict$critical_effect)) res[i,"effect_category_standard"] = dict[ce,"standardized_effect_category"]
    else res[i,"effect_category_standard"] = "other"

    res[i,"key"] = paste(res[i,"dtxsid"],res[i,"source"],res[i,"toxval_numeric"],res[i,"common_name"],res[i,"toxval_type"],res[i,"study_type_standard"])
    key = res[i,"key"]
    if(is.element(key,s1$key)) {
      temp = unique(s1[s1$key==key,])
      #browser()
      res[i,"bmdh1_aurisano"] = 10**(temp[1,"log_BMDh_nrd_1.[mg/kg-d]"])
      res[i,"bmdh2_aurisano"] = 10**(temp[1,"log_BMDh_nrd_2.[mg/kg-d]"])
      res[i,"bmdh_aurisano"] = 10**(temp[1,"log_BMDh_nrd_avg.[mg/kg-d]"])
      res[i,"conceptual_model_1_aurisano"] = temp[1,"conceptual_model_nrd_1"]
      res[i,"conceptual_model_2_aurisano"] = temp[1,"conceptual_model_nrd_2"]
      res[i,"effect_category_standard"] = temp[1,"standardized_effect_categories"]
      #browser()
    }
    else if(is.element(key,s2$key)) {
      temp = unique(s2[s2$key==key,])
      #browser()
      res[i,"bmdh1_aurisano"] = 10**(temp[1,"log_BMDh_rd_1.[mg/kg-d]"])
      res[i,"bmdh2_aurisano"] = 10**(temp[1,"log_BMDh_rd_2.[mg/kg-d]"])
      res[i,"bmdh_aurisano"] = 10**(temp[1,"log_BMDh_rd_avg.[mg/kg-d]"])
      res[i,"conceptual_model_1_aurisano"] = temp[1,"conceptual_model_rd_1"]
      res[i,"conceptual_model_2_aurisano"] = temp[1,"conceptual_model_rd_2"]
      res[i,"effect_category_standard"] = temp[1,"standardized_effect_categories"]
      #browser()
    }

    if(!is.na(res[i,"effect_category_standard"]) && !is.na(res[i,"study_type_standard"])) {
      ecs = res[i,"effect_category_standard"]
      sts = res[i,"study_type_standard"]

      if(is.element(sts,c("chronic","subchronic","short-term"))) sts2 = "repeat dose"
      else sts2 = "reproductive developmental"

      st = res[i,"study_type"]
      tts = res[i,"toxval_type_standard"]

      temp = cdict[cdict$study_type_standard==sts2,]
      temp = temp[temp$effect_category_standard==ecs,]
      res[i,"conceptual_model_1"] = temp[1,"conceptual_model_1"]
      res[i,"conceptual_model_2"] = temp[1,"conceptual_model_2"]

      F1 = 1
      if(is.element(st,c("subchronic","neurotoxicity subchronic","human","neurotoxicity","repeat dose other"))) F1 = 2
      if(is.element(st,c("short-term","neurotoxicity short-term"))) F1 = 5

      F2 = 1
      if(tts=="LOAEL") F2 = 3
      if(tts=="BMDL" && sts2=="repeat dose") F2 = 0.5

      cm1 = res[i,"conceptual_model_1"]
      cm2 = res[i,"conceptual_model_2"]
      if(!is.na(cm1) && !is.na(cm2)) {
        F31 = 1
        F32 = 1
        if(cm1=="Continuous") F31 = 1/3
        if(cm1=="Continuous" && tts=="BMDL" && sts2=="repeat dose") F31 = 2/3
        if(cm1=="Quantal-Deterministic") F31 = 2/9
        if(cm1=="Quantal-Stochastic") F31 = 2/3
        if(cm2=="Continuous") F32 = 1/3
        if(cm2=="Quantal-Deterministic") F32 = 2/9
        if(cm2=="Quantal-Stochastic") F32 = 2/3
        if(cm2=="Quantal-Stochastic" && tts=="BMDL") F32 = 1/3

        species = res[i,"common_name"]
        F4 = 1
        if(species=="Rat") F4 = 4.1
        if(species=="Mouse") F4 = 7.3
        if(species=="Rabbit") F4 = 2.4
        if(species=="Dog") F4 = 1.5
        F5 = 1

        denom1 = F1*F2*F31*F4*F5
        denom2 = F1*F2*F32*F4*F5
        pod = res[i,"toxval_numeric"]

        bmdh1 = pod / denom1
        bmdh2 = pod / denom2
        if(cm2!="-") bmdh = 10**(0.5*(log10(bmdh1)+log10(bmdh2)))
        else bmdh = bmdh1
        res[i,"bmdh1"] = bmdh1
        if(cm2!="-") res[i,"bmdh2"] = bmdh2
        res[i,"bmdh"] = bmdh
        res[i,"F1"] = F1
        res[i,"F2"] = F2
        res[i,"F31"] = F31
        res[i,"F32"] = F32
        res[i,"F4"] = F4
        res[i,"F5"] = F5
      }
      else browser()
    }
    if(!is.na(res[i,"bmdh_aurisano"])) res[i,"bmdh_ratio"] = res[i,"bmdh"]/res[i,"bmdh_aurisano"]
    if(i%%1000==0) cat(i,"out of",nrow(res),"\n")
  }
  res = res[res$study_type!="acute",]

  x = res$bmdh
  y = res$bmdh_aurisano
  x = x[!is.na(y)]
  y = y[!is.na(y)]
  plot(y~x)
  sty = createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db," ",sys.date,".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
