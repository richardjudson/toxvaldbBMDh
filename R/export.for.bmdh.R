library(openxlsx)
library(digest)
#-----------------------------------------------------------------------------------
#' Export records required for calculating BMDh values.
#'
#' `export.for.bmdh` Exports all of the data required for the BMDh calculations.
#' The main query may need to be modified to extract more columns if needed for
#' the final application. Certain sources have been excluded because they have a high
#' percentage of read-across values. Species are filtered to only include Human,
#' Dog, Mouse, Rat and Rabbit. If more species are to be included, then allometric
#' scaling factors for those need to added to the function bmd.per.study().
#'
#' @param toxval.db Database version
#' @param user The username for the MySQL database. The database instance is
#' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @return Write a file with the results: ToxValDB for BMDh {toxval.db} {Sys.Date()}.xlsx
#' @export
#-----------------------------------------------------------------------------------
export.for.bmdh <- function(toxval.db="res_toxval_v95",user,password) {
  printCurrentFunction(toxval.db)
  dir = "data/"
  setDBConn(user=user,password=password)
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  exclude.sources = c("COSMOS","EFSA","PPRTV (NCEA)")
  slist = slist[!is.element(slist,exclude.sources)]
  res = NULL
  for(src in slist) {
    n = runQuery(paste0("select count(*) from toxval where source='",src,"'"),toxval.db)[1,1]
    cat(src,":",n,"\n")
    query = paste0("SELECT
                      a.dtxsid,a.casrn,a.name,
                      b.source,
                      b.toxval_type,
                      b.toxval_numeric_qualifier,
                      b.toxval_numeric,
                      b.toxval_units,
                      b.study_type,
                      b.study_duration_value,
                      b.study_duration_units,
                      b.study_duration_class,
                      d.common_name,
                      b.sex,
                      b.exposure_route,
                      b.critical_effect,
                      b.year,
                      f.long_ref,
                      f.url,
                      f.record_source_level,
                      f.record_source_type,
                      b.source_hash,
                      b.study_group,
                      a.cleaned_casrn,a.cleaned_name
                      FROM
                      toxval b
                      INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                      LEFT JOIN species d on b.species_id=d.species_id
                      INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                      INNER JOIN record_source f on b.toxval_id=f.toxval_id
                      WHERE
                      b.source='",src,"'
                      and b.human_eco='human health'
                      and e.toxval_type_supercategory in ('Point of Departure')
                      and b.toxval_units='mg/kg-day'
                      and b.exposure_route='oral'
                      and b.qc_status='pass'
                     ")


    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    mat[is.na(mat$toxval_numeric_qualifier),"toxval_numeric_qualifier"] = "="
    mat[is.element(mat$toxval_numeric_qualifier,c("~","<","<=")),"toxval_numeric_qualifier"] = "="
    mat = mat[mat$toxval_numeric_qualifier=="=",]

    exclude.list = c("HNEL","LEC","LEL","NEL","NOTEL","POD (HE)","POD (HEC)","POD (surrogate)","critical value")
    mat = mat[!is.element(mat$toxval_type,exclude.list),]
    exclude.list = c("-","Hershberger","acute","clinical","dose selection","in vitro","neurotoxicity acute","uterotrophic")
    mat = mat[!is.element(mat$study_type,exclude.list),]

    mat[mat$common_name=="European Rabbit","common_name"] = "Rabbit"
    mat[mat$common_name=="House Mouse","common_name"] = "Mouse"
    mat[mat$common_name=="Rabbit Family","common_name"] = "Rabbit"
    mat = mat[is.element(mat$common_name,c("Rat","Mouse","Dog","Rabbit","Human")),]

    mat[is.na(mat$casrn),"casrn"] = mat[is.na(mat$casrn),"cleaned_casrn"]
    mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
    mat[is.na(mat$name),"name"] = mat[is.na(mat$name),"cleaned_name"]
    mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
    cat(src,nrow(mat),"\n\n")
    cremove = c("cleaned_name","cleaned_casrn")
    mat = mat[ , !(names(mat) %in% cremove)]
    res = rbind(res,mat)
  }
  sty = createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"results/ToxValDB for BMDh ",toxval.db," ",Sys.Date(),".xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
