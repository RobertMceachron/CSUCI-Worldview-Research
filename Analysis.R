library(readxl) # For Reading XLSX
library(lavaan) # For FA & PA
library(lavaangui) # For FA & PA
library(tidyverse) # For Everything
library(haven) # For SPSS Output
library(stats) # For PCA
library(car) # For Assumption Checks
library(ltm) # For Reliability 
library(psych) # For Descriptive
library(apaTables) # For APA Tables 
library(kableExtra) # Table Extra

  # Reads Scored Qualtrics File
    dataraw1 <- read_xlsx("data/WV_V1.xlsx") # Inputs 1st Survey
    dataraw2 <- read_xlsx("data/WV_V2.xlsx") # Inputs 2nd Survey
  
  # Reads All Variable Qualtrics Files
    allcolumns1 <- read_xlsx("data/V1_All.xlsx") # Inputs 1st Survey
    allcolumns2 <- read_xlsx("data/V2_All.xlsx") # Inputs 2nd Survey

########################
#    Data Cleaning     #
########################

  # Removes Extra Rows 
    data1trans <- dataraw1
    data2trans <- dataraw2
  
  # Removes Pesky Qualtrics Labels >:(
    data1trans <- data1trans[-1,]
    data2trans <- data2trans[-c(1,83),]
    
  # Combines Data Sets  
    data <- rbind(data1trans, data2trans)
    
  # Remove Outliers
    data <- data[-c(99, 100),]
    
  # Give New Column Names
    colnames(data) <- c("ID", "Age", "Gender", "Race", "MathAnxiety", "MathSelfConcept", "Religiosity",
                        "SocioeeconomicStatus", "MathAvoidance", "SelfInvestmentMutability",
                        "SelfInvestmentLOR", "EducationSES", "ParentClass", "SESrank", "Affluency",
                        "Prestige", "WAIMutability", "WAIRelationToAuthority", "WAIMetaphysics", 
                        "WAILocusOfResponsibility", "WAIAgency", "WAIRelationToGroup")
    
  # Converts to Numeric
    data <- data |>
            mutate_at(c("Age", "MathAnxiety", "MathAvoidance", "MathSelfConcept",
                        "ParentClass", "Affluency", "Religiosity", "EducationSES", "SocioeeconomicStatus",
                        "SESrank", "Prestige", "SelfInvestmentMutability", "SelfInvestmentLOR", 
                        "WAIMutability", "WAIRelationToAuthority", "WAIMetaphysics", "WAILocusOfResponsibility",
                        "WAIAgency", "WAIRelationToGroup"), as.numeric)

  # Only variables we need
    cleandata <- data |>
      dplyr::select(!c(ParentClass, Affluency, Religiosity, EducationSES, SocioeeconomicStatus,
                  SESrank, Prestige))
    
  # Outputs Combined Files for rest of Group
    write_sav(data, "data/Combined_Data.sav")
    write_csv(data, "data/Combined_Data.csv")
  
    
###########################
#     Normality Check     #
###########################
    
  # WAI
    hist(data |> 
           dplyr::select(contains("WAIMUTABILITY")))
    hist(data |> 
           dplyr::select(contains("WAIRELATIONTOAUTHORITY")))
    hist(data |> 
           dplyr::select(contains("WAIMetaphysics")))
    hist(data |> 
           dplyr::select(contains("WAILocusOfResponsibility")))
    hist(data |> 
           dplyr::select(contains("WAIRelationtoGroup")))
    
  # Self Investment
    hist(data |> 
            dplyr::select(contains("SelfInvestmentMutability")))
    hist(data |> 
            dplyr::select(contains("SelfInvestmentLOR")))
    
  # Math
    hist(data |> 
           dplyr::select(contains("MathAnxiety")))
    hist(data |> 
           dplyr::select(contains("MathSelfConcept")))
    hist(data |> 
           dplyr::select(contains("MathAvoidance")))

  # Other Demographics
    hist(data |> 
           dplyr::select(contains("ParentClass")))
    hist(data |> 
           dplyr::select(contains("Affluency")))
    hist(data |> 
           dplyr::select(contains("Religiosity")))
    hist(data |> 
           dplyr::select(contains("EducationSES")))   
    hist(data |> 
           dplyr::select(contains("SocioeeconomicStatus")))
    hist(data |> 
           dplyr::select(contains("SESrank")))    
    hist(data |> 
           dplyr::select(contains("Prestige"))) 
    
###############################
#     Reliability Checking    #
###############################
    
  # Removes Labels
    allcolumns1 <- allcolumns1[-1,]
    allcolumns2 <- allcolumns2[-1,]
    
  # Self Investment
    SIScaleData1 <- allcolumns1 |> 
      dplyr::select(contains("selfin"))
    SIScaleData2 <- allcolumns2 |> 
      dplyr::select(contains("selfin"))
    SIScale <- rbind(SIScaleData1, SIScaleData2)
    SISScale <- na.omit(SIScale)
    # SE Mutability
      SIMScale <- SIScale |>
        dplyr::select(ends_with("M"))
      cronbach.alpha(na.omit(SIMScale))
    # SE Locus of Responsibility
      SILORScale <- SIScale |>
        dplyr::select(ends_with("L"))
      cronbach.alpha(na.omit(SILORScale))
    
  # Agency
    Q1 <- allcolumns1 |> 
      dplyr::select(ends_with(c("AGV_14","AGV_101","AGV_55","AGV_116","AGD_43","AGD_44","AGD_136","AGD_120")))
    Q2 <- allcolumns2 |>
      dplyr::select(ends_with(c("Q5","Q22","Q30","Q51","Q38","Q34","Q13","Q9")))
    ScaleAgency <- rbind(Q1, setNames(Q2, names(Q1)))
    cronbach.alpha(na.omit(ScaleAgency))

  # Mutability
    Q1 <- allcolumns1 |> 
      dplyr::select(ends_with(c("MP_103","MP_100","MP_109","MP_110")))
    Q2 <- allcolumns2 |>
      dplyr::select(ends_with(c("Q11","Q32","Q44","Q19")))
    ScaleMutabililty <- rbind(Q1, setNames(Q2, names(Q1)))
    cronbach.alpha(na.omit(ScaleMutabililty))   
      
  # Authority
    Q1 <- allcolumns1 |> 
      dplyr::select(ends_with(c("AUL_135","AUL_145","AUL_115","AULAT_137","AULAT_87","AULAT_61")))
    Q2 <- allcolumns2 |>
      dplyr::select(ends_with(c("Q7","Q40","Q36","Q23","Q47","Q15")))
    ScaleAuthority <- rbind(Q1, setNames(Q2, names(Q1)))
    cronbach.alpha(na.omit(ScaleAuthority))

  # Relation to Group
    Q1 <- allcolumns1 |> 
      dplyr::select(ends_with(c("GRPC_81","GRPC_89","GRPC_2","GRPC_50","GRPC_121","GRPC_122","GRPI_108","GRPI_70","GRPI_62","GRPI_15","GRPI_54","GRPI_104")))
    Q2 <- allcolumns2 |>
      dplyr::select(ends_with(c("Q2","Q6","Q18","Q26","Q39","Q43","Q14","Q35","Q52","Q31","Q10","Q54")))
    ScaleRelationToGroup <- rbind(Q1, setNames(Q2, names(Q1)))
    cronbach.alpha(na.omit(ScaleRelationToGroup))   
   
  # Locus of Responsibility
    Q1 <- allcolumns1 |> 
      dplyr::select(ends_with(c("RESPE_150","RESPE_126","RESPE_4","RESPE_132","RESPE_133","RESPE_91","RESPE_49","RESPI_140","RESPI_66","RESPI_72","RESPI_149","RESPI_97","RESPI_69","RESPI_56")))
    Q2 <- allcolumns2 |>
      dplyr::select(ends_with(c("Q1","Q8","Q21","Q25","Q46","Q42","Q27","Q48","Q17","Q29","Q33","Q50","Q4","Q53")))
    ScaleLOR <- rbind(Q1, setNames(Q2, names(Q1)))
    cronbach.alpha(na.omit(ScaleLOR))

  # Metaphysics
    Q1 <- allcolumns1 |> 
      dplyr::select(ends_with(c("METAS_139","METAS_58","METAS_8","METAS_143","METAS_138","METAM_148","METAM_96","METAM_142","METAM_41","METAM_92")))
    Q2 <- allcolumns2 |>
      dplyr::select(ends_with(c("Q3","Q20","Q24","Q41","Q49","Q16","Q37","Q45","Q28","Q12")))
    ScaleMetaphysics <- rbind(Q1, setNames(Q2, names(Q1)))
    cronbach.alpha(na.omit(ScaleMetaphysics))
    
    
#################################
#     Descriptive Statistics    #
#################################
  
  # Computes Mean, SD, SS for Variables
    print(describe(data, na.rm = TRUE, omit=TRUE))
    
  # Checks # of M/F
    print(table(data$Gender))
  
  # Checks # of Race Category
    Race <- data$Race  
    sum(str_count(Race, pattern = "White"))
    sum(str_count(Race, pattern = "Black"))
    sum(str_count(Race, pattern = "Asian"))
    sum(str_count(Race, pattern = "Middle Eastern"))
    sum(str_count(Race, pattern = "Chicano/Chicana"))
    sum(str_count(Race, pattern = "I Identify as"))
    sum(str_count(Race, pattern = "Prefer Not to Say")) 
  
  # Creates Large Correlation Table
    apa.cor.table(
      data, filename="CorTable.doc")
    
#####################################
#   Principal Component Analysis    #
#####################################
    
  # Selects Columns we want to analyze
    PCAdata <- data |>
                dplyr::select(WAIMutability, WAIRelationToAuthority, WAIMetaphysics, 
                              WAILocusOfResponsibility, WAIAgency, WAIRelationToGroup, SelfInvestmentMutability, SelfInvestmentLOR)
    
  # Creates Eigenvalues for factors
    eigenvalues <- eigen(cor(PCAdata))$values
    print(eigenvalues)
  
  # Creates Scree Plot for Eigenvalues
    scree_plot <- data.frame(
      eigenvalues = eigen(cor(PCAdata))$values,
      component = 1:length(eigen(cor(PCAdata))$values)
    )

    plot(scree_plot$component, scree_plot$eigenvalues, type = "b",
         xlab = "Component Number", ylab = "Eigenvalue",
         main = "Scree Plot")
    abline(h = 1, col = "red", lty = 2)
    
  # Runs PCA
    ncomp <- 3
    pca_rotated <- psych::principal(PCAdata, rotate="varimax", nfactors=ncomp, scores=TRUE)
    print(pca_rotated)
    fa.diagram(pca_rotated)

    # Generates Charts
      loadings_mat <- as.matrix(pca_rotated$loadings)
      
      table_df <- data.frame(
        loadings_mat
        )
      print(table_df)

      # Grabs all percentages explained
        RC1Var <- round(pca_rotated$Vaccounted[3, 1], 3)*100
        RC2Var <- round(pca_rotated$Vaccounted[3, 2], 3)*100
        RC3Var <- round(pca_rotated$Vaccounted[3, 3], 3)*100
        
      # Edit the following as HTML Its easier than figuring out whatever madness Kable needs to work
        kable(table_df, align = "l", caption = "Principal Loadings (Pattern Matrix)") |>
          # add_header_above(c(" ", RC1Var, RC2Var, RC3Var)) |>
          kable_styling(full_width = FALSE) |>
          save_kable("RCmatrixx.html")
        
      # Do Communality Matrix
        communality_mat <- as.matrix(pca_rotated$communality)
        kable(communality_mat, align = "l", caption = "Principal Loadings (Pattern Matrix)", digits = 3) |>
          kable_styling(full_width = FALSE)
        
##################################
#   Path Analyses & MR w/Math    #
##################################

      # Creates new data that are standardized
        PAdata <- data
        numcols <- sapply(PAdata, is.numeric)
        PAdata[numcols] <- lapply(PAdata[numcols], function(x) as.numeric(scale(x, center = TRUE, scale = TRUE)))
        
      # Runs Path Analysis  
        model <-'
                MathAnxiety ~ SelfInvestmentLOR + MathAvoidance 
                SelfInvestmentLOR ~ WAILocusOfResponsibility 
                '
        
        pa_fit <- cfa(model, data = data)
        summary(pa_fit, standardized=TRUE, fit.measures =TRUE)
        
      # Creates Plot
        plot_lavaan(pa_fit) 
      