# ............................................................................
# ............................................................................
# Cleaning for DIBS Calibration
#
# Script to generate clean data sets (outlier removal) and identification of relevant variables
#
# The targets delta_b_v and delta_b_v_percentage are not considered in this script
# They are found in H to H4
# Here only the en_cons target of space heating, occupancy caused hot water and electricity are considered
#
# Date: 01.09.2023
# Developer: Julian Bischof
#
# Dependencies: JBs_R_Functions.R
# Encoding: UTF-8
# ............................................................................
# ............................................................................



# ............................................................................
#
# SET UP####
#
# ............................................................................
    rm(list = ls()) # cleans Environment


    # SET Problem TYPE!!!
    Problem_Type <- "Regression"
    # Problem_Type <- "Classification"


    # Determining the Variable name of the target Variable
    # target_name <- "en_cons_space_heating"
    target_name <- "en_cons_electricity"
    # target_name <- "en_cons_ohw"
    #
    # target_name <- "en_cons"
    # target_name = "delta_b_v"
    # target_name = "delta_b_v_percentage"

    # What value is targeted
    target = "en_cons"
    if(target_name == "delta_b_v"){
        target = "delta"
    }

    if(Problem_Type=="Classification" & target_name == "en_cons_space_heating"){
        target = "ratio"
    }

    # define file paths
    GitHub_Data_Access <- "S:/GitHub_Data_Access/"
    # GitHub_Data_Access <- "E:/GitHub_Data_Access/"
    # paste(GitHub_Data_Access, "data_BE_DIBS_input_output_calibrated_", ".csv" ,sep = "")

    # Save_path <- "D://OneDrive//OneDrive - Technological University Dublin//GitHub_Large_Files//R---BEM-Calibration-and-Extrapolation-FILES//"
    Save_path <- "C://Users//Julian//OneDrive - Technological University Dublin//GitHub_Large_Files//R---BEM-Calibration-and-Extrapolation-FILES//"


    # set working directory
    setwd(Save_path)

    #set test_name
    test_name <- paste(Problem_Type, "_", target_name, "_no_weights_sig_001_", sep = "")
    print(test_name)







# ............................................................................
#
# Libraries laden####
#
# ............................................................................

    # install.packages("dplyr")
    library(dplyr)

    # install.packages("ggplot2")
    library(ggplot2)

    # install.packages("grid")
    library(grid)

    # install.packages("downloader")
    library(downloader)

    # install.packages("expss")
    library(expss)

    # install.packages("Rcpp")
    library(Rcpp)
    # install.packages("caret")
    library(caret)
    # https://topepo.github.io/caret/
    # install.packages("klaR")
    library(klaR)

    # install.packages("corrplot")
    library(corrplot)

    # install.packages("caretEnsemble")
    library(caretEnsemble)

    # install.packages("relaimpo")
    library(relaimpo)

    # install.packages("httpgd")
    library(httpgd)

    # install.packages("splitstackshape")
    library(data.table)
    library(caret)
    library(purrr)
    library(rlang)
    library(tidyverse)
    library(sjstats)
    library(rstatix)
    library(splitstackshape)

    # #install.packages("magicfor")
    # library(magicfor)

    # install.packages("parallel")
    library(parallel)
    # install.packages("doParallel")
    library(doParallel)




# ............................................................................
#
# Funktionen einfuehren####
#
# ............................................................................

    # calls my function R-Script!

    # library(downloader)

    SHA <-
    sha_url(
        "https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R"
    )
    SOURCE <-
    "https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R"

    downloader::source_url(SOURCE, sha = SHA, prompt = FALSE, encoding = "UTF-8")
    # downloader::source_url(SOURCE, prompt=FALSE)
    print("Downloaded and using JBs_R_Functions")

    
        ###########################################################
        # 1. Laden des Headers
        #	 Laedt Gebaeudedaten, stichprobenrelevante Daten 
        #	 inkl. der Schaetzfunktionen in den Arbeitsspeicher 
        ###########################################################
        # Read BE Data from R
        source(paste(GitHub_Data_Access, "header_bre.r", sep = ""))



# ............................................................................
#
# Daten einlesen####
#
# ............................................................................

    # Read CSV DB-Tiefenerhebung###
    DB_V_B <- read.csv(
    # "E:/GitHub_Data_Access/data_BE_TE_TEK_Verbrauch.csv",
    paste(GitHub_Data_Access, "data_BE_TE_TEK_Verbrauch", ".csv", sep = ""),
    header = TRUE,
    sep = ";",
    dec = ",",
    fill = TRUE,
    fileEncoding = "UTF-8"
    )
    DB_V_B_orig <- DB_V_B





# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Remove 0 or NA cases for targets
#
# # ...................................
# # ...................................
#

    # ............................................................................
    # Electricity
    # ............................................................................

        DB_V_B$energy_consumption_electricity_clean <-
        as.double(DB_V_B$energy_consumption_electricity_clean)

        # ...................................
        # Alle NA in Verbrauch zu 0
        # Dannach alle 0 in Verbrauch delete
        # ...................................

        # Replaces NA/NaN/Inf with 0
        DB_V_B$energy_consumption_electricity_clean[DB_V_B$energy_consumption_electricity_clean ==
        ""] <- 0

        # Alle Zeilen mit Verbrauch = 0 rauswerfen
        DB_V_B_el <- subset(DB_V_B, energy_consumption_electricity_clean != 0)
        # ...................................


    # ............................................................................
    # Heat
    # ............................................................................

        DB_V_B$energy_consumption_heating_clean <-
        as.double(DB_V_B$energy_consumption_heating_clean)

        # ...................................
        # Alle NA in Verbrauch zu 0
        # Dannach alle 0 in Verbrauch delete
        # ...................................

        # Replaces NA/NaN/Inf with 0
        DB_V_B$energy_consumption_heating_clean[DB_V_B$energy_consumption_heating_clean ==
        ""] <- 0

        # Alle Zeilen mit Verbrauch = 0 rauswerfen
        DB_V_B_th <- subset(DB_V_B, energy_consumption_heating_clean != 0)
        # ...................................




    # ............................................................................
    # Occupancy Hot Water (OHW)
    # ............................................................................

        DB_V_B$energy_consumption_DHW_clean <-
        as.double(DB_V_B$energy_consumption_DHW_clean)

        # ...................................
        # Alle NA in Verbrauch zu 0
        # Dannach alle 0 in Verbrauch delete
        # ...................................

        # Replaces NA/NaN/Inf with 0
        DB_V_B$energy_consumption_DHW_clean[DB_V_B$energy_consumption_DHW_clean == ""] <- 0

        # Alle Zeilen mit Verbrauch = 0 rauswerfen
        DB_V_B_th_ohw <- subset(DB_V_B, energy_consumption_DHW_clean != 0)
        # ...................................





    # ............................................................................
    # Total cleaned up data set without the measured energy consumption for heat and electricity = 0 or NA
    # ............................................................................

        # # ........................................
        # # Alle Zeilen mit Verbrauch = 0 rauswerfen
        # # ........................................
        # DB_V_B_clean <- subset(DB_V_B_th, energy_consumption_electricity_clean != 0)

        # Selection of clean data set dependent on target

        if(target_name == "en_cons_space_heating"){
            DB_V_B_clean <- DB_V_B_th
        }

        if(target_name == "en_cons_ohw"){
            DB_V_B_clean <- DB_V_B_th_ohw
        }

        if(target_name == "en_cons_electricity"){
            DB_V_B_clean <- DB_V_B_el
        }

        if(target_name == "delta_b_v"){
            DB_V_B_clean <- DB_V_B_th
        }
        

    # ........................................
    # Variablen Definieren
    # ........................................

        DB_V_B_clean$hk_geb_agg <- as.integer(DB_V_B_clean$hk_geb_agg)
        # DB_V_B_clean <- subset(DB_V_B_clean, hk_geb_agg==1)
        # DB_V_B_clean <- subset(DB_V_B_clean, hk_geb_agg==2)

        # hk_geb_agg <- as.integer(DB_V_B_clean$hk_geb_agg)
        # hk_geb_BE <- DB_V_B_clean$hk_geb_BE
        # uk_geb_BE <- DB_V_B_clean$uk_geb_BE
        # bak_grob <- DB_V_B_clean$bak_grob
        # bak <- DB_V_B_clean$bak
        # geb_flaeche <- as.double(DB_V_B_clean$geb_flaeche)
        DB_V_B_clean$geb_flaeche <- as.double(DB_V_B_clean$geb_flaeche)
        DB_V_B_clean$energy_demand_heating_clean_EBF_tek_DIBS_BE <- as.double(DB_V_B_clean$energy_demand_heating_clean_EBF_tek_DIBS_BE)

        # energy_consumption_heating_clean <- as.double(DB_V_B_clean$energy_consumption_heating_clean)
        DB_V_B_clean$energy_consumption_heating_clean <- as.double(DB_V_B_clean$energy_consumption_heating_clean)
        # HeatingEnergy_DIBS_BE <- as.double(DB_V_B_clean$HeatingEnergy_DIBS_BE)
        DB_V_B_clean$HeatingEnergy_DIBS_BE <- as.double(DB_V_B_clean$HeatingEnergy_DIBS_BE)

        # energy_consumption_electricity_clean <- as.double(DB_V_B_clean$energy_consumption_electricity_clean)
        DB_V_B_clean$energy_consumption_electricity_clean <- as.double(DB_V_B_clean$energy_consumption_electricity_clean)
        # ElectricityDemandTotal_DIBS_BE <- as.double(DB_V_B_clean$ElectricityDemandTotal_DIBS_BE)
        DB_V_B_clean$ElectricityDemandTotal_DIBS_BE <- as.double(DB_V_B_clean$ElectricityDemandTotal_DIBS_BE)

        DB_V_B_clean$EnergyRefArea_DIBS_TE <- as.double(DB_V_B_clean$EnergyRefArea_DIBS_TE)
        EnergyRefArea_DIBS_TE <- as.double(DB_V_B_clean$EnergyRefArea_DIBS_TE)

        DB_V_B_clean$d_fl_wueoa <- as.double(DB_V_B_clean$d_fl_wueoa)
        DB_V_B_clean$EnergyRefArea_DIBS_BE <- as.double(DB_V_B_clean$EnergyRefArea_DIBS_BE)



        # write it as a csv file
        con <- file(paste(GitHub_Data_Access, "DB_V_B_clean_", target_name, ".csv", sep = ""), encoding = "UTF-8")
        write.csv(DB_V_B_clean, file = con)

# ...................................






# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Outlier elimination based on unrealistic consumption and ratio between demand and consumption
#
# # ...................................
# # ...................................
#

    #......................................................................
    # Removal of Outliers in Dataset to create basis for simple calibration
    #......................................................................

        # Clean up data to correct type
        DB_V_B_clean$energy_consumption_heating_clean_EBF_tek <- as.double(DB_V_B_clean$energy_consumption_heating_clean_EBF_tek)
        DB_V_B_clean$energy_consumption_electricity_clean_EBF_tek <- as.double(DB_V_B_clean$energy_consumption_electricity_clean_EBF_tek)
        DB_V_B_clean$energy_demand_electricity__non_heat__clean_EBF_tek_DIBS_BE <- as.double(DB_V_B_clean$energy_demand_electricity__non_heat__clean_EBF_tek_DIBS_BE)
        DB_V_B_clean$HRF <- as.double(DB_V_B_clean$HRF)

        # Heat
        if(target_name == "en_cons_space_heating"){
            db_heat_DB_V_B_clean_ex_red <- 
            ratio.based.outlier.removal(dataset = DB_V_B_clean, 
            measurement = DB_V_B_clean$energy_consumption_heating_clean_EBF_tek,
            calculation = DB_V_B_clean$energy_demand_heating_clean_EBF_tek_DIBS_BE, 
            upper_outlier_percentile = 0.98)
            DB_V_B_clean_ex_red <- db_heat_DB_V_B_clean_ex_red
            # measurement_to_calculated_energy
            DB_V_B_clean$ratio_plot <- DB_V_B_clean$energy_consumption_heating_clean_EBF_tek / DB_V_B_clean$energy_demand_heating_clean_EBF_tek_DIBS_BE
        }

        # Heat delta_b_v
        if(target_name == "delta_b_v"){
            db_heat_DB_V_B_clean_ex_red <- 
            ratio.based.outlier.removal(dataset = DB_V_B_clean, 
            measurement = DB_V_B_clean$energy_consumption_heating_clean_EBF_tek,
            calculation = DB_V_B_clean$energy_demand_heating_clean_EBF_tek_DIBS_BE, 
            upper_outlier_percentile = 0.98)
            DB_V_B_clean_ex_red <- db_heat_DB_V_B_clean_ex_red
            # measurement_to_calculated_energy
            DB_V_B_clean$ratio_plot <- DB_V_B_clean$energy_consumption_heating_clean_EBF_tek / DB_V_B_clean$energy_demand_heating_clean_EBF_tek_DIBS_BE
        }

        # DHW / OHW
        if(target_name == "en_cons_ohw"){
            DB_V_B_clean$energy_demand_DHW_clean_EBF_tek_DIBS_BE <- as.double(DB_V_B_clean$HotWaterEnergy_DIBS_BE) / as.double(DB_V_B_clean$TEK_res_energy_relevant_area)
            # implemented in dataset as energy_demand_DHW_clean_EBF_tek_DIBS_BE (21.02.2023)
            #
            # Replaces NA/NaN/Inf with 0
            DB_V_B_clean$energy_consumption_DHW_clean_EBF_tek[DB_V_B_clean$energy_consumption_DHW_clean_EBF_tek ==
            ""] <- 0
            DB_V_B_clean$energy_consumption_DHW_clean_EBF_tek <- as.double(DB_V_B_clean$energy_consumption_DHW_clean_EBF_tek)
            DB_V_B_clean$energy_demand_DHW_clean_EBF_tek_DIBS_BE <- as.double(DB_V_B_clean$energy_demand_DHW_clean_EBF_tek_DIBS_BE)
            # Alle Zeilen mit Verbrauch = 0 und Bedarf = 0 (Dann kein WW) rauswerfen
            DB_V_B_clean_DHW <- subset(DB_V_B_clean, energy_consumption_DHW_clean_EBF_tek != 0)
            DB_V_B_clean_DHW <- subset(DB_V_B_clean_DHW, energy_demand_DHW_clean_EBF_tek_DIBS_BE != 0)
            # ...................................
            db_DHW_DB_V_B_clean_ex_red <- 
            ratio.based.outlier.removal(dataset = DB_V_B_clean_DHW, 
            measurement = DB_V_B_clean_DHW$energy_consumption_DHW_clean_EBF_tek,
            calculation = DB_V_B_clean_DHW$energy_demand_DHW_clean_EBF_tek_DIBS_BE, 
            upper_outlier_percentile = 0.98)
            DB_V_B_clean_ex_red <- db_DHW_DB_V_B_clean_ex_red
            # measurement_to_calculated_energy
            DB_V_B_clean <- DB_V_B_clean_DHW
            DB_V_B_clean$ratio_plot <- DB_V_B_clean_DHW$energy_consumption_DHW_clean_EBF_tek / DB_V_B_clean_DHW$energy_demand_DHW_clean_EBF_tek_DIBS_BE
        }

        # Electricity
        if(target_name == "en_cons_electricity"){
            db_electricity_DB_V_B_clean_ex_red <- 
            ratio.based.outlier.removal(dataset = DB_V_B_clean, 
            measurement = DB_V_B_clean$energy_consumption_electricity_clean_EBF_tek,
            calculation = DB_V_B_clean$energy_demand_electricity__non_heat__clean_EBF_tek_DIBS_BE, 
            upper_outlier_percentile = 0.98)
            DB_V_B_clean_ex_red <- db_electricity_DB_V_B_clean_ex_red
            # measurement_to_calculated_energy
            DB_V_B_clean$ratio_plot <- DB_V_B_clean$energy_consumption_electricity_clean_EBF_tek / DB_V_B_clean$energy_demand_electricity__non_heat__clean_EBF_tek_DIBS_BE
        }

        # Plot density of ratio
                    upper_outlier_percentile <- 0.98
                    m_to_c <- DB_V_B_clean$ratio_plot
                    # Number of objects with measured consumption greater than the calculated demand
                    number_m_to_c_above <- length(m_to_c[m_to_c > 1])
                    # Number of objects with measured consumption smaller than the calculated demand
                    number_m_to_c_below <- length(m_to_c[m_to_c < 1])
                    # Determination of the upper Faktor based on the upper_outlier_percentile
                    Faktor <- sort(m_to_c)[upper_outlier_percentile * length(m_to_c)] # by representing value
                    # Determination of the lower Faktor based on the upper "Faktor" based on the ratios of the sample above and below 1
                    lowerFaktor <- (Faktor / number_m_to_c_above) * number_m_to_c_below
                    EinsdurchFaktor <- 1 / lowerFaktor
                    # Electricity
        if(target_name == "en_cons_electricity"){
            p <- print(ggplot(DB_V_B_clean, aes(x = ratio_plot, after_stat(scaled))) + #, after_stat(scaled)
                geom_density(fill = "grey") + 
                #geom_histogram(mapping = aes(x = ratio_plot)) +
                geom_vline(xintercept=Faktor, linetype="dotted", size = 0.8, , color = "red") + 
                geom_vline(xintercept=EinsdurchFaktor, linetype="dotted", size = 0.8, , color = "red") + 
                geom_vline(xintercept=1, linetype="dotted", size = 0.8, , color = "#2600ff") +
                geom_text(aes(x=Faktor, label="Upper outlier boundary", y=0.90), colour="red", angle=90, nudge_x = 0.3, size = 2.75) +
                geom_text(aes(x=EinsdurchFaktor, label="Lower outlier boundary", y=0.90), colour="red", angle=90, nudge_x = -0.4, size = 2.75) + 
                geom_text(aes(x=1, label="measured energy = calculated energy", y=0.82), colour="#2600ff", angle=90, nudge_x = 0.3, size = 2.75) +
                geom_text(aes(x=Faktor, label=round(Faktor, digits = 2), y=-0.1), colour="red", angle=00, nudge_x = 0.7, size = 2.75) +
                geom_text(aes(x=EinsdurchFaktor, label=round(EinsdurchFaktor, digits = 2), y=-0.1), colour="red", angle=00, nudge_x = -0.7, size = 2.75) + 
                xlab("ratio: measured energy to calculated energy (only showing section up to a ratio of 25)") + 
                ylab("density") +
                coord_cartesian(xlim = c(0, 25))
                ) 
        }
        if(target_name == "en_cons_space_heating"){
            p <- print(ggplot(DB_V_B_clean, aes(x = ratio_plot, after_stat(scaled))) + #, after_stat(scaled)
                geom_density(fill = "grey") + 
                #geom_histogram(mapping = aes(x = ratio_plot)) +
                geom_vline(xintercept=Faktor, linetype="dotted", size = 0.8, , color = "red") + 
                geom_vline(xintercept=EinsdurchFaktor, linetype="dotted", size = 0.8, , color = "red") + 
                geom_vline(xintercept=1, linetype="dotted", size = 0.8, , color = "#2600ff") +
                geom_text(aes(x=Faktor, label="Upper outlier boundary", y=0.90), colour="red", angle=90, nudge_x = 0.5, size = 2.75) +
                geom_text(aes(x=EinsdurchFaktor, label="Lower outlier boundary", y=0.90), colour="red", angle=90, nudge_x = -0.6, size = 2.75) + 
                geom_text(aes(x=1, label="measured energy = calculated energy", y=0.82), colour="#2600ff", angle=90, nudge_x = 0.5, size = 2.75) +
                geom_text(aes(x=Faktor, label=round(Faktor, digits = 2), y=-0.1), colour="red", angle=00, nudge_x = 1, size = 2.75) +
                geom_text(aes(x=EinsdurchFaktor, label=round(EinsdurchFaktor, digits = 2), y=-0.1), colour="red", angle=00, nudge_x = 1, size = 2.75) + 
                xlab("ratio: measured energy to calculated energy") + 
                ylab("density")
                ) 
        }
        if(target_name == "en_cons_ohw"){
            p <- print(ggplot(DB_V_B_clean, aes(x = ratio_plot, after_stat(scaled))) + #, after_stat(scaled)
                geom_density(fill = "grey") + 
                #geom_histogram(mapping = aes(x = ratio_plot)) +
                geom_vline(xintercept=Faktor, linetype="dotted", size = 0.8, , color = "red") + 
                geom_vline(xintercept=EinsdurchFaktor, linetype="dotted", size = 0.8, , color = "red") + 
                geom_vline(xintercept=1, linetype="dotted", size = 0.8, , color = "#2600ff") +
                geom_text(aes(x=Faktor, label="Upper outlier boundary", y=0.90), colour="red", angle=90, nudge_x = 0.3, size = 2.75) +
                geom_text(aes(x=EinsdurchFaktor, label="Lower outlier boundary", y=0.90), colour="red", angle=90, nudge_x = -0.4, size = 2.75) + 
                geom_text(aes(x=1, label="measured energy = calculated energy", y=0.82), colour="#2600ff", angle=90, nudge_x = 0.3, size = 2.75) +
                geom_text(aes(x=Faktor, label=round(Faktor, digits = 2), y=-0.1), colour="red", angle=00, nudge_x = 0.4, size = 2.75) +
                geom_text(aes(x=EinsdurchFaktor, label=round(EinsdurchFaktor, digits = 2), y=-0.1), colour="red", angle=00, nudge_x = 0.4, size = 2.75) + 
                xlab("ratio: measured energy to calculated energy") + 
                ylab("density")
                ) 
        }
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations_total", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations_total_S", ".pdf", sep = "") # PDF
            ggsave(print_path, p, width=2, height=2, units="in", scale=3)
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations_total", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600)
            
        # ........................
        # write it in a txt file: Here the different ratios are documented
        # ........................
        ratio <- DB_V_B_clean$ratio_plot
        sink(paste(Save_path, "//", test_name, "_ratios", ".txt", sep = ""))
        print("Minimum ratio of measured energy to calculated energy:")
        print(min(ratio))
        print("Maximum ratio of measured energy to calculated energy:")
        print(max(ratio))
        print("Upper outlier boundary = 98 percentile:")
        print(max(Faktor))
        print("Lower outlier boundary:")
        print(max(EinsdurchFaktor))
        print("Upper Outliers removed:")
        print(length(ratio[ratio>Faktor]))
        print("Lower Outliers removed:")
        print(length(ratio[ratio<EinsdurchFaktor]))
        print("number of ratios above 1")
        print(length(ratio[ratio>1]))
        print("number of ratios below 1")
        print(length(ratio[ratio<1]))
        sink()


    # ........................
    # write it as a csv file
    # ........................
        # con<-file('E:/BE_Reg_Verbrauch_Bedarf/GitHub_Data_Access/DB_V_B_clean_ex_red.csv',encoding="UTF-8")
        con <- file(paste(GitHub_Data_Access, "DB_V_B_clean_ex_red_", target_name, ".csv", sep = ""), encoding = "UTF-8")
        write.csv(DB_V_B_clean_ex_red, file = con)



# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Variable effect size -> only keep variables with high effect on target
#
# # ...................................
# # ...................................
#

    # Set Seed for any randomized functions.... Allos the same "random" start and
    # therefore replicateability (same results with each time the code runs)
    set.seed(123)

    dt <- DB_V_B_clean_ex_red

    # ...................................................
    # Prepering relevant variables for further analysis####
    # ...................................................
    # The target variable of the analysis is the measured energy consumption of
    # the buildings. Therefore the physically causal related variables with
    # influence on the measured energy consumption are considered. These are
    # determined based on the most relevant variables for physical building
    # energy simulations.
    #
    # Physically most relevant variables for building energy simulations
    # \cite{Smith.2009, Olivero.2016, Heo.2011, Corrado.2009, Famuyibo.2012b}
    #
    # The variables primarily related to heat and/or cooling are marked with (HC)
    # while electricity nor for heating and cooling is (E).
    # However, to be sure all should be also tested for E, as the measured consumption
    # "might" include in the electricity part also electricity for heating/cooling.
    #
    # Below the most relevant variables according to the references above are listed.
    # To each group of most relevant variables the related (directly of indirectly)
    # variables of the ENOB:dataNWG data set are matched:
    #
    # 1: indoor temperature and set-points for heating, cooling and humidification (HC),
    # ## hk_geb_BE, hk_geb_agg indirectly via according usage categories (e.g. DIN V 18599-10)
    #
    # 2: air change rate including infiltration, mechanical and natural ventilation (HC),
    # ## might partly indirectly depend on bak, bak_grob (how was build during a certain time period),
    # ## The area ventilated and heated determines the volumes f_ant_belueftet, f_ant_beheizt,
    #
    # 3: envelope (walls, roof, floor and windows) thermal properties (Resistance and Conductivity) including thermal-bridges (HC),
    # ## bak, bak_grob indirectly via typical materials and their thermal properties
    # ## used during a certain time and the way that was build during the period.
    # ## aw_daemm_staerke_1, aw_flantgedges, d_fl_wueoa,
    #
    # 4: size and form (Area/Volume) (HC),
    # ## describing the share of conditioning: f_ant_belueftet, f_ant_beheizt,
    # ## geb_flaeche, n_ug, n_og
    #
    # 5: system efficiency (heating, cooling and ventilation (heat recovery)) (HC),
    # ## w_erz_art_et, qh3_1, qh1 and partly bak, bak_grob if no replacement of
    # ## the heating/cooling system has been undergone and it still represents the initial status of the bak, bak_grob
    #
    # 6: number of occupants (HC)(E),
    # ## q25_1
    #
    # 7: kind of occupants (metabolism rate) (HC)(E),
    # ## hk_geb_BE, hk_geb_agg indirectly
    #
    # 8: appliance power density (HC)(E),
    # ## hk_geb_BE, hk_geb_agg indirectly
    #
    # 9: lighting power density (HC)(E),
    # ## hk_geb_BE, hk_geb_agg indirectly and indirectly by geb_flaeche (indirect geometry, defines partly natural lighting)
    #
    # 10: heating and cooling distribution loss factor (HC) and
    # w_erz_art_et indirectly and bak, bak_grob indirectly due to differences in insulation of internal piping, describing the geometry: f_ant_belueftet, f_ant_beheizt,
    #
    # 11: glass emissivity and solar transmittance (HC)(E?).
    # bak, bak_grob indirectly via building in windows
    #
    #
    # 12: ENERGY DEMAND
    # ## energy_demand_heating_clean_EBF_tek_DIBS_BE is included as meta variable physically combining the above

        # Bring relevant variables into correct data type
        dt$bak <- as.factor(dt$bak)
        dt$hk_geb_BE <- as.factor(dt$hk_geb_BE)
        dt$bak_grob <- as.factor(dt$bak_grob)
        dt$hk_geb_agg <- as.factor(dt$hk_geb_agg)
        dt$qh3_1 <- as.factor(dt$qh3_1)
        dt$w_erz_art_et <- as.factor(dt$w_erz_art_et)
        dt$qh1 <- as.factor(dt$qh1)

        
    #..........................
    # rename variables to shorter more handable names
    #..........................
    phy.relevant.variables.datanwg.calibration.rename <- function(dt, target_name, DB_V_B_clean_ex_red, aim) {
        # dt - dataframe that is a copy of DB_V_B_clean_ex_red with factor variables defined as factors
        # target name - targeted value of calibration
        # DB_V_B_clean_ex_red - Extrema and outlier removed in case of preperation for training models,
        # or dataset on which the calibraion model is to be applied (in case of calibration and extrapolation)
        # for which purpose/aim is the rename undertaken (training or prediction)
        cols <- c(
            "hk_geb_BE", 
            "hk_geb_agg",
            "bak", 
            "bak_grob",
            "q25_1", 
            "geb_flaeche", 
            "n_og", 
            "n_ug", 
            "f_ant_belueftet", 
            "f_ant_beheizt",
            "aw_daemm_staerke_1", 
            "aw_flantgedges", 
            "d_fl_wueoa",
            "w_erz_art_et", 
            "qh3_1", 
            "qh1", 
            "EnergyRefArea_DIBS_BE", 
            "HRF"
        ) # HRF is included as it is requiered for the weighting and the later extrapolation
        # dt <- dt[,..cols]
        dt <- select(dt, cols)
        col_names <- c(
            "hk_geb_BE", 
            "hk_geb_agg",
            "bak", 
            "bak_grob",
            "q25_1", 
            "geb_flaeche", 
            "n_og", 
            "n_ug", 
            "f_ant_belueftet", 
            "f_ant_beheizt",
            "aw_daemm_staerke_1", 
            "aw_flantgedges", 
            "d_fl_wueoa",
            "w_erz_art_et", 
            "qh3_1", 
            "qh1", 
            "EnergyRefArea_DIBS_BE", 
            "HRF"
        )
        colnames(dt) <- col_names

        if(aim == "training"){
            if(target_name == "en_cons_space_heating"){
                cols <- c(
                    "energy_consumption_heating_clean",
                    "HeatingEnergy_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    "en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            if(target_name == "delta_b_v"){
                cols <- c(
                    "energy_consumption_heating_clean",
                    "HeatingEnergy_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    "en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            if(target_name == "en_cons_ohw"){
                cols <- c(
                    "energy_consumption_DHW_clean",
                    "HotWaterEnergy_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    "en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            if(target_name == "en_cons_electricity"){
                cols <- c(
                    "energy_consumption_electricity_clean",
                    "energy_demand_electricity__non_heat__clean_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    "en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            # add temporarry dt1 en_cons and en_dem to dt
            dt$en_cons <- dt1$en_cons
            dt$en_dem <- dt1$en_dem
        }

        if(aim == "prediction"){
            if(target_name == "en_cons_space_heating"){
                cols <- c(
                    #"energy_consumption_heating_clean",
                    "HeatingEnergy_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    #"en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            if(target_name == "delta_b_v"){
                cols <- c(
                    #"energy_consumption_heating_clean",
                    "HeatingEnergy_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    #"en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            if(target_name == "en_cons_ohw"){
                cols <- c(
                    #"energy_consumption_DHW_clean",
                    "HotWaterEnergy_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    #"en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            if(target_name == "en_cons_electricity"){
                cols <- c(
                    #"energy_consumption_electricity_clean",
                    "energy_demand_electricity__non_heat__clean_DIBS_BE"
                ) 
                dt1 <- select(DB_V_B_clean_ex_red, cols)
                col_names <- c(
                    #"en_cons",
                    "en_dem"
                )
                colnames(dt1) <- col_names
            }

            # add temporarry dt1 en_cons and en_dem to dt
            #dt$en_cons <- dt1$en_cons
            dt$en_dem <- dt1$en_dem
        }

        return(dt)
    }

    aim <- "training"
    dt <- phy.relevant.variables.datanwg.calibration.rename(dt = dt, target_name = target_name, DB_V_B_clean_ex_red = DB_V_B_clean_ex_red, aim = aim)

    # Determine number of different variable values (categories) of each variable (coloum in dt)
    zusammenfassung <- purrr::modify(dt, ~ length(unique(.x))) %>% distinct()
    print("Number of unique values or categories in variables:")
    zusammenfassung

    # Show data type of each column of dt
    str(dt) 

    # correct data types 
        dt$n_og <- as.double(dt$n_og)
        dt$n_ug <- as.double(dt$n_ug)
        dt$aw_flantgedges <- as.double(dt$aw_flantgedges)
        dt$en_dem <- as.double(dt$en_dem)
        dt$q25_1 <- as.double(dt$q25_1)
        dt$geb_flaeche <- as.double(dt$geb_flaeche)
        dt$f_ant_belueftet <- as.double(dt$f_ant_belueftet)
        dt$f_ant_beheizt <- as.double(dt$f_ant_beheizt)
        dt$aw_daemm_staerke_1 <- as.double(dt$aw_daemm_staerke_1)
        dt$d_fl_wueoa <- as.double(dt$d_fl_wueoa)
        dt$EnergyRefArea_DIBS_BE <- as.double(dt$EnergyRefArea_DIBS_BE)
        dt$HRF <- as.double(dt$HRF)
        dt$en_cons <- as.double(dt$en_cons)

        if(target_name == "delta_b_v"){
        specific_consumption <- dt$en_cons / dt$EnergyRefArea_DIBS_BE
        dt$delta <- dt$en_dem - dt$en_cons
        drops <- c("en_cons")
        dt <- dt[, !(names(dt) %in% drops)]
        }

        if(Problem_Type=="Classification" & target_name == "en_cons_space_heating"){
        specific_consumption <- dt$en_cons / dt$EnergyRefArea_DIBS_BE
        specific_demand <- dt$en_dem / dt$EnergyRefArea_DIBS_BE
        dt$ratio <- dt$en_cons / dt$en_dem
        drops <- c("en_cons")
        dt <- dt[, !(names(dt) %in% drops)]
        }

    #set test_name
        test_name <- paste(Problem_Type, "_", target_name, "_no_weights_sig_001_", sep = "")
        print(test_name)


    # .......................................................
    # Consideration of combination dummy variables
    # .......................................................
        drops <- c("HRF", target)
        dt_no_HRF <- dt[, !(names(dt) %in% drops)]
        c1 <- expand.grid(colnames(dt_no_HRF), colnames(dt_no_HRF)) # all variables numeric and factor

        c2 <- as.character(c1[, 2])
        c1 <- as.character(c1[, 1])

        # determination of the correlation between en_cons (in anova_auswertung)
        # and the combination of the variables in c1 and c2 (consequently the combination of each variable with each variable)
        # and each variable individually
        erg_tmp <- map2(c1, c2, ~ anova_auswertung(dt, .x, .y, target)) %>% discard(is.null) # All dt variables need to be in the correct data type (check in case of error)
        erg_tmp <- map_df(erg_tmp, as.data.frame)
        erg_tmp <- erg_tmp[, 1:6]
        erg <- bind_rows(erg_tmp) # bind rows, so that they are not shuffled when arranging them below

        # Print ANOVA-analysis results according to relevance of Eta2_partial
        erg <- erg %>% arrange(desc(Eta2_partial))
        erg

                # The following rules of thumb are used to interpret values for Partial eta squared:
                # .01: Small effect size
                # .06: Medium effect size
                # .14 or higher: Large effect size
                # Quelle: https://www.statology.org/partial-eta-squared/
                # and https://www.reneshbedre.com/blog/repeated-measure-anova.html
                # and https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
                # \cite(Cohen.1992, Cohen.1988) % Cohen.1988 Pages 283 - 288

    # .......................................................
    # Summary of Results of eta2 depending on target
    # .......................................................
            # .......................
            # target_name = "en_cons_space_heating"
            # .......................
            # .......................................................
            # In Case of consideration of combination dummy variables
            # .......................................................
            # Identification of Variables that are either relevant individually * or in combination with other variables ** or both ***
            # >= 0.06 ....................
            # "en_dem"                ***  metric
            # "hk_geb_BE"             ***
            # "hk_geb_agg"            **   x Remove of further Training data, as it is just a summary of hk_geb_BE
            # "bak"                   ***
            # "bak_grob"              **   x Remove of further Training data, as it is just a summary of bak
            # "q25_1"                 ***  metric
            # "geb_flaeche"           ***  metric
            # "n_og"                  ***  metric
            # "n_ug"                  ***  metric
            # "f_ant_belueftet"       ***  metric
            # "f_ant_beheizt"         **   metric
            # "aw_daemm_staerke_1"    **   metric
            # "aw_flantgedges"        **   metric
            # "d_fl_wueoa"            ***  metric
            # "w_erz_art_et"          ***
            # "qh3_1"                 ***
            # "qh1"                   ***
            # "EnergyRefArea_DIBS_BE" ***  metric

        # Save eta2 ANOVA results
        con <- file(paste(Save_path, "//", test_name, "Anova_eta2_partial",
        "_Summary_Results", ".csv", sep = ""), encoding = "UTF-8")
        write.csv(erg, file = con)

        # get relevant combinations only ( and remove lines with metric variables)
        erg_rel <- subset(erg, Eta2_partial >= 0.06)
        unique_erg_rel <- unique(erg_rel$Parameter)
        unique_erg_rel <- as.data.frame(unique_erg_rel)
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('q25_1', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('geb_flaeche', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('n_og', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('n_ug', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('f_ant_belueftet', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('f_ant_beheizt', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('aw_daemm_staerke_1', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('aw_flantgedges', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('d_fl_wueoa', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('EnergyRefArea_DIBS_BE', unique_erg_rel))
        unique_erg_rel <- unique_erg_rel %>%  filter(!grepl('en_dem', unique_erg_rel))
        path <- getwd()
        sink(paste(path, "//", test_name, "Anova_eta2_partial", "_Unique_larger_0.06_Results", ".txt", sep = ""))
        print(unique_erg_rel)
        sink()


# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Baue Dummy-Variable based on Variable effect size
#
# # ...................................
# # ...................................
#

    # !!!
    # This generation of the dummy-variables is based on the effect size results from above!
    # This has to be updated after the DIBS has been adopted
    # This has to be updated when a new traget is used (add another if())

        # partial eta_squared interpretation > 0.06 medium effect size
        # Only categorial combinations, all metric variables are considered for the next step of significance testing.
        # Combining of both metric and categorial variables would lead to physically uninterpretable meta variables (e.g.
        # w_erz_art_et345:geb_flaeche123292.322). Within categories this is just a combination of atributes that occur together...
        # A transformation of metric variables into categries would loss importent information ... so this is not an option...
        # Aim is to reduce number of coefficients that are considered by the models.... for the sample of 400 cases we should e.g. not have
        # 200 coefficients that are consiedered, as this leads to an definit overfitting.....

    # Interaktionsspalten

    generate.dummy.variables.based.on.identified.eta2.datanwg <- function(dt) {
        # dt - dataframe which includes the relevant variables for training or for application of the models to 
        # which the dummy variables are added
        # Checked for both weather data sets: W04-18 and W07-21
        if (target_name == "en_cons_space_heating" && Problem_Type=="Regression") {
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:bak - 1, data = dt)) # 123
            dt <- cbind(dt, model.matrix(~ bak:hk_geb_BE - 1, data = dt)) # 146
            dt <- cbind(dt, model.matrix(~ qh1:hk_geb_BE - 1, data = dt)) # 248
            #<0.14
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:hk_geb_BE - 1, data = dt)) # 260
            dt <- cbind(dt, model.matrix(~ qh3_1:hk_geb_BE - 1, data = dt)) # 276
            dt <- cbind(dt, model.matrix(~ qh3_1:w_erz_art_et - 1, data = dt)) # 380
            # dt = cbind(dt,model.matrix(~qh1:hk_geb_agg-1, data=dt)) # 397
            dt <- cbind(dt, model.matrix(~ qh3_1:bak - 1, data = dt)) # 405
            dt <- cbind(dt, model.matrix(~ qh1:bak - 1, data = dt)) # addition through DIBS v.1.1.0 Results W04-18 and W07-21
            #<0.06

            # hier die Einzelvariablen
            dt <- cbind(dt, model.matrix(~ hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh1 - 1, data = dt))
            
            # Spalten mit nur Nuller rauslassen
            dt <- dt %>%
                select_if(is.numeric) %>%
                select_if(any) %>%
                as.data.table()
        }

        # Checked for both weather data sets: W04-18 and W07-21 DIFFERENCE
        if (target_name == "en_cons_space_heating" && Problem_Type == "Classification") {
            dt <- cbind(dt, model.matrix(~ bak:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1:bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh1:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1:hk_geb_BE - 1, data = dt))
            # dt <- cbind(dt, model.matrix(~ qh3_1:w_erz_art_et - 1, data = dt)) # ONLY addition through DIBS v.1.1.0 Results W04-18
            
            # hier die Einzelvariablen
            dt <- cbind(dt, model.matrix(~ hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1 - 1, data = dt)) # addition through DIBS v.1.1.0 Results
            dt <- cbind(dt, model.matrix(~ qh1 - 1, data = dt)) # addition through DIBS v.1.1.0 Results
        
            # Spalten mit nur Nuller rauslassen
            dt <- dt %>%
                select_if(is.numeric) %>%
                select_if(any) %>%
                as.data.table()
        }

        # delta_b_v
        # Checked for both weather data sets: W04-18 and W07-21 DIFFERENCE
        if (target_name == "delta_b_v") {
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1:bak - 1, data = dt))
            # dt <- cbind(dt, model.matrix(~ bak:qh1 - 1, data = dt)) # removed through DIBS v.1.1.0 Results
            dt <- cbind(dt, model.matrix(~ qh1:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1:w_erz_art_et - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1:hk_geb_BE - 1, data = dt)) # ONLY addition through DIBS v.1.1.0 Results W07-21
            #<0.06

            # hier die Einzelvariablen
            dt <- cbind(dt, model.matrix(~ hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh1 - 1, data = dt))

            # Spalten mit nur Nuller rauslassen
            dt <- dt %>%
                select_if(is.numeric) %>%
                select_if(any) %>%
                as.data.table()
        }

        # OHW
        # Checked for both weather data sets: W04-18 and W07-21
         if(target_name == "en_cons_ohw"){
            dt <- cbind(dt, model.matrix(~ bak:hk_geb_BE - 1, data = dt)) # 1
            dt <- cbind(dt, model.matrix(~ qh3_1:hk_geb_BE - 1, data = dt)) # 3
            dt <- cbind(dt, model.matrix(~ qh1:hk_geb_BE - 1, data = dt)) # 5
            # dt <- cbind(dt, model.matrix(~ f_ant_belueftet:hk_geb_BE - 1, data = dt)) # 7 # metric
            # dt <- cbind(dt, model.matrix(~ geb_flaeche:hk_geb_BE - 1, data = dt)) # 9 # metric
            # metric variables are not combinded
            #dt <- cbind(dt, model.matrix(~ bak:n_og - 1, data = dt)) # 133
            dt <- cbind(dt, model.matrix(~ qh3_1:bak - 1, data = dt)) # 144
            dt <- cbind(dt, model.matrix(~ qh3_1:w_erz_art_et - 1, data = dt)) # 162
            dt <- cbind(dt, model.matrix(~ bak:qh1 - 1, data = dt)) # 219
            #<0.14
            
            #<0.06

            # hier die Einzelvariablen
            dt <- cbind(dt, model.matrix(~ hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh1 - 1, data = dt))

            # Spalten mit nur Nuller rauslassen
            dt <- dt %>%
                select_if(is.numeric) %>%
                select_if(any) %>%
                as.data.table()
         }


        # Electricity
        # Checked for both weather data sets: W04-18 and W07-21
        if(target_name == "en_cons_electricity"){
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ hk_geb_BE:qh3_1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ hk_geb_BE:bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak:qh3_1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:qh3_1 - 1, data = dt))
            # dt <- cbind(dt, model.matrix(~ w_erz_art_et:bak_grob - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et:qh1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ hk_geb_BE:qh1 - 1, data = dt)) # removed through DIBS v.1.1.0 Results, BUT back after Appliance Gains Bug Fix!
            #<0.06

            # hier die Einzelvariablen
            dt <- cbind(dt, model.matrix(~ hk_geb_BE - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ bak - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh3_1 - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ w_erz_art_et - 1, data = dt))
            dt <- cbind(dt, model.matrix(~ qh1 - 1, data = dt))

            # Spalten mit nur Nuller rauslassen
            dt <- dt %>%
                select_if(is.numeric) %>%
                select_if(any) %>%
                as.data.table()
        }

        return(dt)
    }

    dt <- generate.dummy.variables.based.on.identified.eta2.datanwg(dt = dt)

    # ...................................
    # Remove variables or variable interactions that occure in less than 1% of the training sample of about (400 cases)
    # ...................................
    # consider only combinations that are occurring in over 1% (n=3) of the buildings of the sample,
    # in order to reduce the likelihood of a random single occurrence of variable combinations with a high weighting (HRF)
    # which would greatly influence the to be trained model. So this likelihood is reduced by only considering variable combinations with at
    # least 4 occurrences, which, if significant, will much more likely represent a general impact on the target variable that should be considered in an
    # universal/general calibration model.
    v <- dt[, colSums(dt) > 3]
    dt <- dt[, v, with = FALSE]

    # Remove the not relevant variables for training:
    weights <- dt$HRF
    dt$HRF <- NULL

    # Set aside the EBF area for later normalisation of results, as it might not be part of relevant variables in dt
    dt_EBF <- dt$EnergyRefArea_DIBS_BE


# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Keep only significant variables and develop stuetzstellen for classification case 
#
# # ...................................
# # ...................................
#
    # .........................................................
    # determination of significant variables
    # .........................................................
        
        target_vector <- dt$en_cons

        if(target_name == "delta_b_v"){
            target_vector <- dt$delta
        }

        if(Problem_Type=="Classification" & target_name == "en_cons_space_heating"){
        target_vector <- dt$ratio
        }

        dt <- significant.variables(dt = dt, target = target, target_vector = target_vector, weights = weights, name = test_name)


    # .........................................................
    # defining stuetzstellen und klassen
    # .........................................................
        df <- dt # dataframe for later training

        if ((target_name == "en_cons_space_heating" || target_name == "en_cons_ohw" || target_name == "en_cons_electricity") && Problem_Type == "Regression") {
            # print and save the deviation of V/B
            df$measurement_to_calculated_energy <- dt$en_cons / dt$en_dem
            print(ggplot(df, aes(x = measurement_to_calculated_energy)) +
                geom_density()) # plot deviation of ziel from def.stuezstellen.klassen
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 1600, height = 900)
            df$measurement_to_calculated_energy <- NULL # remove again, as df is used for training....
        }

        if ((target_name == "en_cons_space_heating" || target_name == "en_cons_ohw" || target_name == "en_cons_electricity") && Problem_Type == "Classification") {
            # print and save the deviation of V/B
            df$measurement_to_calculated_energy <- dt$ratio
            print(ggplot(df, aes(x = measurement_to_calculated_energy)) +
                geom_density()) # plot deviation of ziel from def.stuezstellen.klassen
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "_Deviation_Measurments_to_Calculations", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 1600, height = 900)
            df$measurement_to_calculated_energy <- NULL # remove again, as df is used for training....
            if (Problem_Type == "Classification") {
                target <- dt$ratio # target for stuetzstellen
                stuetzstellen <- def.stuetzstellen(df = df, target = target, GitHub_Data_Access = GitHub_Data_Access)
                df <- def.stuetzstellen.klassen(df = df, target = target)
                # save stuetzstellen als LaTex Table
                stuetzstellen_to_table <- as.data.frame(stuetzstellen)
                library(xtable)
                print(xtable(stuetzstellen_to_table, type = "latex"), file = paste(test_name, "stuetzstellen_to_table", ".tex", sep = ""))
                target <- "ratio"
            }
        }





# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Model Training and Testing (k-Fold cross-validation) 
#
# # ...................................
# # ...................................


    # .........................................................
    # Initiallising of Function for parallel training
    # .........................................................
        amp_up_models()


    df_prediction <- df # before the dublication the df is set aside for later prediction...


    # .........................................................
    # Define algorithems for training
    # .........................................................
        if(Problem_Type == "Regression"){
            #........................................
            # Regression Models working (04.11.2022)
            #........................................
            # A list sorted by algorithem type is available in H3
            #..........
            # ALL Regression models
            #..........
            caret_models <-
            c("svmLinear",
                "svmLinear2",
                "svmLinear3",
                "svmPoly",
                "svmRadial",
                "svmRadialCost",
                "svmRadialSigma",
                "rf",
                "cforest",
                "qrf",
                #"rfRules", # Error in case without weights
                "RRF",
                "RRFglobal",
                "parRF",
                "ranger",
                "Rborist",
                "brnn",
                "qrnn",
                "monmlp",
                "rbfDDA",
                "lm",
                "glm",
                "BstLm",
                "penalized",
                "glmboost",
                "leapBackward",
                "leapForward",
                "leapSeq",
                "lmStepAIC",
                "gaussprLinear",
                "glmStepAIC",
                "plsRglm",
                "kknn",
                "xgbLinear",
                "gbm",
                "bayesglm",
                "blasso",
                "bridge",
                "blassoAveraged",
                "gaussprPoly",
                "gaussprRadial"
            )
            
            All_regression_models <- as.data.frame(caret_models)
            #
            ML_family <- c("SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "RF",
                        "RF",
                        "RF",
                        #"RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "NN",
                        "NN",
                        "NN",
                        "RBF",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "LM",
                        "KNN",
                        "GBoost",
                        "GBoost",
                        "Bayesian",
                        "Bayesian",
                        "Bayesian",
                        "Bayesian",
                        "GP",
                        "GP",
                        "GP"
            )
            All_regression_models$Type <- ML_family

            library(xtable)
            print(xtable(All_regression_models, type = "latex"), file = "All_regression_models.tex")
            }

        if(Problem_Type == "Classification"){
            #........................................
            # Classification Models working (01.11.2022)
            #........................................
            # A list sorted by algorithem type is available in H3
            #..........
            # All Classification
            #..........
            # 29 working models of caret package that can solve this classification problem
            caret_models <-
            c("svmLinear",
                "svmLinear2",
                "svmLinear3",
                "svmRadial",
                "svmPoly",
                "svmRadialCost",
                "svmRadialSigma",
                "rf",
                "cforest",
                "rfRules",
                "RRF",
                "RRFglobal",
                "ranger",
                "Rborist",
                "ordinalRF",
                "wsrf",
                "rbfDDA",
                "multinom",
                "avNNet",
                "monmlp",
                "nnet",
                "pcaNNet",
                "regLogistic",
                "kknn",
                "ownn",
                "snn",
                "xgbLinear",
                "gbm",
                "bayesglm"#,
                #"gaussprRadial" # Removed as it causes errors when applied onto a larger data set: https://stackoverflow.com/questions/35808311/gaussian-process-classification-with-r-kernlab-package-issue-predicting-test-se
            )

            All_Classification_models <- as.data.frame(caret_models)
            ML_family <- c("SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "SVM",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RF",
                        "RBF",
                        "RBF",
                        "NN",
                        "NN",
                        "NN",
                        "NN",
                        "LM",
                        "KNN",
                        "KNN",
                        "KNN",
                        "GBoost",
                        "GBoost",
                        "Bayesian"#,
                        #"GP"
            )
            All_Classification_models$Type <- ML_family

            library(xtable)
            print(xtable(All_Classification_models, type = "latex"), file = "All_classification_models.tex")
        }


    #........................................
    # Define control options for the caret package
    #........................................
        ctrl <- trainControl(
        method = "repeatedcv",
        allowParallel = TRUE,
        repeats = 5, # 5
        number = 10, # 10; changing to 5 and 3 reduces the mean accuracy in a test categorization only by about 2 % !!! this is expected, as we have many doubles in the weighted data set!!!
        savePredictions = "final"
        ) # linout = TRUE,

        if (Problem_Type == "Classification") {
        stuetzstellen <- stuetzstellen # Only for Classification Problem
        met <- "Accuracy"
        output <- "klasse"
        } else {
        # met <- "Rsquared"
        met <- "MAE" # Choosen for the comparion of DIBS Calibration -> as Aim is to get smalest Error over the Building Stock (RMSE would punish outliers more)
        output <- target
        }

        if (target == "en_cons") {
            specific_demand <- dt$en_dem / dt_EBF # dt$EnergyRefArea_DIBS_BE
            specific_consumption <- dt$en_cons / dt_EBF # dt$EnergyRefArea_DIBS_BE
        }
        if (target == "delta") {
            specific_demand <- dt$en_dem / dt_EBF # dt$EnergyRefArea_DIBS_BE
        }
        EBF <- dt_EBF # dt$EnergyRefArea_DIBS_BE

        # Remove en_cons of df in Case of Classification so that is not used for training!!!
        if (Problem_Type == "Classification") {
        #df$en_cons <- NULL 
        #specific_demand <- dt$en_dem / dt_EBF # dt$EnergyRefArea_DIBS_BE
        #specific_consumption <- dt$en_cons / dt_EBF # dt$EnergyRefArea_DIBS_BE
        target <- "en_cons"
        df$ratio <- NULL
        }


    # ............................................................................
    #
    # Training Models ####
    #
    # ............................................................................
        results <- caret.batch.training(
        Problem_Type = Problem_Type,
        caret_models = caret_models,
        df = df,
        output = output,
        ctrl = ctrl,
        met = met,
        test_name = test_name,
        specific_demand = specific_demand,
        specific_consumption = specific_consumption,
        EBF = EBF,
        df_prediction = df_prediction,
        stuetzstellen = stuetzstellen
        )

        # save Environment with all results for independent plotting etc.
        path <- getwd()
        save.image(file = paste(path, "//", test_name, "Training_Environment", ".RData", sep = ""))


            #
            ##
            ###
            ####
            #####
            ######
            #######
            ########
            #########
            ##########
            ###########
            ############
            #############
            ##############
            ###############
            ################
            #################
            ##################
            ###################
            ####################
            #####################
            ######################
            #######################
            ########################


            # set working directory after opening .RData file
            # Save_path <- "C://Users//Julian//OneDrive - Technological University Dublin//GitHub_Large_Files//2023_04_28_Best_Classification_Models_no_weights_sig001//"
                setwd(Save_path)

            # ............................................................................
            #
            # Libraries laden after opening .RData file ####
            #
            # ............................................................................

                # install.packages("dplyr")
                library(dplyr)
                # install.packages("ggplot2")
                library(ggplot2)
                # install.packages("grid")
                library(grid)
                # install.packages("downloader")
                library(downloader)
                # install.packages("expss")
                library(expss)
                # install.packages("Rcpp")
                library(Rcpp)
                # install.packages("caret")
                library(caret)
                # https://topepo.github.io/caret/
                # install.packages("klaR")
                library(klaR)
                # install.packages("corrplot")
                library(corrplot)
                # install.packages("caretEnsemble")
                library(caretEnsemble)
                # install.packages("relaimpo")
                library(relaimpo)
                # install.packages("httpgd")
                library(httpgd)
                # install.packages("splitstackshape")
                library(data.table)
                library(caret)
                library(purrr)
                library(rlang)
                library(tidyverse)
                library(sjstats)
                library(rstatix)
                library(splitstackshape)
                # #install.packages("magicfor")
                # library(magicfor)
                # install.packages("parallel")
                library(parallel)
                # install.packages("doParallel")
                library(doParallel)



    # ..........................................
    # comparison of model prediction via individual plots####
    # ..........................................

        # here the individual plots of the coparing plot above are indivdually ploted and saved!

        # 1: Verbrauch zu Bedarf ohne Predict
        plot(specific_demand, specific_consumption, main = "DIBS demand", ylab = "Measurment", xlab = "DIBS", xlim = c(0, 600), ylim = c(0, 600)) # Verbrauch zu Bedarf
        abline(0, 1, col = "blue")
        abline(0, 2, col = "green")
        abline(0, 0.5, col = "green")
        # print plot
        print_path <- paste(test_name, "DIBS_", "Plot", ".pdf", sep = "") # PDF
        dev.print(pdf, print_path)
        print_path <- paste(test_name, "DIBS_", "Plot", ".png", sep = "") # png
        dev.print(png, file = print_path, width = 900, height = 900)


        # 2: All tested models mit predict
        for (model in caret_models) {
        my_model <- readRDS(paste(Save_path, model, "__", test_name, ".rds", sep = "", collapse = ""))
        predict_output <- predict.train(my_model, df_prediction, se.fit = FALSE)
        if (Problem_Type == "Regression") {
            rounded_predict <- round(predict_output / EBF, digits = 4)
            if (target_name == "delta_b_v") {
            rounded_predict <- round((df$en_dem - predict_output) / EBF, digits = 4)
            }
            if (target_name == "delta_b_v_percentage") {
            rounded_predict <- round((df$en_dem - ((predict_output / 100) * df$en_dem)) / EBF, digits = 4)
            }
        } else {
            rounded_predict <- round(stuetzstellen[predict_output] * specific_demand, digits = 4)
        }
            plot(rounded_predict, specific_consumption, main = paste(as.character(model), collapse = ""), 
            ylab = "Measurment", xlab = paste("DIBS cor. by ", as.character(model), collapse = " "), 
            xlim = c(0, 600), ylim = c(0, 600)) # Verbrauch zu Kalibriertem-Bedarf
            abline(0, 1, col = "blue")
            abline(0, 2, col = "green")
            abline(0, 0.5, col = "green")
            # print plot
            print_path <- paste(test_name, model, "_", "Plot", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, model, "_", "Plot", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 900) # Potrait
        }


    # ..........................................
    # comparison of model prediction via plots####
    # ..........................................
        # par(mfrow=c(4, round(((length(caret_models)+1)/4)+0.49))) # Grafik ausma(aeoeuesss)e an Modelanzahl Anpassen # Horizontal 4 rows
        par(mfrow = c(round(((length(caret_models) + 1) / 6) + 0.49), 6)) # Grafik ausma(aeoeuesss)e an Modelanzahl Anpassen # Potrait 5 cols

        # 1: Verbrauch zu Bedarf ohne Predict
        plot(specific_demand, specific_consumption, main = "DIBS demand", ylab = "Measurment", xlab = "DIBS", xlim = c(0, 600), ylim = c(0, 600)) # Verbrauch zu Bedarf
        abline(0, 1, col = "blue")
        abline(0, 2, col = "green")
        abline(0, 0.5, col = "green")

        # Initializing the data.frame for saving the Selected model attributes (based on application onto the entire data set)
        Selected_model_attributes <- data.frame()
        # calculate attributes of DIBS calculated demand
        Model_name <- "DIBS"
        Rsquared_DIBS <- R2(specific_demand, specific_consumption)
        MAE_DIBS <- MAE(specific_demand, specific_consumption)
        RMSE_DIBS <- RMSE(specific_demand, specific_consumption)
        demand_absolut <- specific_demand * EBF
        consumption <- specific_consumption * EBF
        Rsquared_DIBS_absolut <- R2(demand_absolut, consumption)
        MAE_DIBS_absolut <- MAE(demand_absolut, consumption)
        RMSE_DIBS_absolut <- RMSE(demand_absolut, consumption)

        Selected_model_attributes[nrow(Selected_model_attributes) + 1, ] <- list(Model_name, Rsquared_DIBS, MAE_DIBS, RMSE_DIBS)
        Selected_model_attributes$Model[1] <- Model_name
        Selected_model_attributes$R2 <- Rsquared_DIBS
        Selected_model_attributes$MAE <- MAE_DIBS
        Selected_model_attributes$RMSE <- RMSE_DIBS
        Selected_model_attributes$R2_Abs <- Rsquared_DIBS_absolut
        Selected_model_attributes$MAE_Abs <- MAE_DIBS_absolut
        Selected_model_attributes$RMSE_Abs <- RMSE_DIBS_absolut



        Selected_model_uncertainties <- data.frame()

        Error_population_my_model_absolut <- sum(consumption) - sum(demand_absolut) # in kWh
        Error_population_my_model_percentage <- (sum(consumption) - sum(demand_absolut)) / sum(consumption) *100
        Average_Error_building_my_model_absolut <- sum(consumption - demand_absolut) / length(consumption)
        Average_Error_building_my_model_percentage <- sum((consumption - demand_absolut) / consumption) / length(consumption) *100 # in %
        
        Selected_model_uncertainties[nrow(Selected_model_uncertainties) + 1, ] <- list(Model_name, Error_population_my_model_absolut, Error_population_my_model_percentage, Average_Error_building_my_model_absolut, Average_Error_building_my_model_percentage)
        Selected_model_uncertainties$Model[1] <- Model_name
        Selected_model_uncertainties$Error_population_my_model_absolut <- Error_population_my_model_absolut
        Selected_model_uncertainties$Error_population_my_model_percentage <- Error_population_my_model_percentage
        Selected_model_uncertainties$Average_Error_building_my_model_absolut <- Average_Error_building_my_model_absolut
        Selected_model_uncertainties$Average_Error_building_my_model_percentage <- Average_Error_building_my_model_percentage
        
        # Initialzing the data.frame for saving the BEST model attributes (based on cross validation during training)
        # DataFrame
        Best_model_attributes <- data.frame()

        # 2: All tested models mit predict
        for (model in caret_models) {
        my_model <- readRDS(paste(Save_path, model, "__", test_name, ".rds", sep = "", collapse = ""))
        
        predict_output <- predict.train(my_model, df_prediction, se.fit = FALSE)
        if (Problem_Type == "Regression") {
            rounded_predict <- round(predict_output / EBF, digits = 4)
            # if (target_name == "delta_b_v") {
            # rounded_predict <- round((df$en_dem - predict_output) / EBF, digits = 4)
            # }
            # if (target_name == "delta_b_v_percentage") {
            # rounded_predict <- round((df$en_dem - ((predict_output / 100) * df$en_dem)) / EBF, digits = 4)
            # }
        } else {
            rounded_predict <- round(stuetzstellen[predict_output] * specific_demand, digits = 4)
        }
        plot(rounded_predict, specific_consumption, main = paste(as.character(model), collapse = ""), 
        ylab = "Measurment", xlab = paste("DIBS cor. by ", as.character(model), collapse = " "), 
        xlim = c(0, 600), ylim = c(0, 600)) # Verbrauch zu Kalibriertem-Bedarf
        abline(0, 1, col = "blue")
        abline(0, 2, col = "green")
        abline(0, 0.5, col = "green")


    #--------------------------------------------------
    # Calculated Rsquared, MAE and RMSE of the final best model and their application to the on-site inspection sample
    #---------------------------------------------------

        rounded_predict_absolut <- rounded_predict * EBF
        consumption <- specific_consumption * EBF

        # Definition of the Rsquared function
        # R-squared (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values.
        # The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
        #
        # rsq <- function (x, y) cor(x, y) ^ 2
        # https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
        # Rsquared_my_model <- rsq(rounded_predict, specific_consumption)
        Rsquared_my_model <- R2(rounded_predict, specific_consumption) # caret Rsuqared function
        Rsquared_my_model_absolut <- R2(rounded_predict_absolut, consumption) # caret Rsuqared function

        # Definition of the MAE function
        # MAE (Mean absolute error) represents the difference between the original and predicted
        # values extracted by averaged the absolute difference over the data set.
        MAE_my_model <- MAE(rounded_predict, specific_consumption)
        MAE_my_model_absolut <- MAE(rounded_predict_absolut, consumption)

        # Definition of the RMSE function
        # RMSE (Root Mean Squared Error) is the error rate by the square root of MSE.
        RMSE_my_model <- RMSE(rounded_predict, specific_consumption) # caret Rsuqared function
        RMSE_my_model_absolut <- RMSE(rounded_predict_absolut, consumption)

        # Estimation of Model uncertainty based on samll sample of on-site inspections
        Error_population_my_model_absolut <- sum(consumption) - sum(rounded_predict_absolut) # in kWh
        Error_population_my_model_percentage <- (sum(consumption) - sum(rounded_predict_absolut)) / sum(consumption) *100 # in %

        Div_con_predict <- consumption - rounded_predict_absolut
        Div_con_predict_df <- as.data.frame(Div_con_predict)

        Average_Error_building_my_model_absolut <- sum(consumption - rounded_predict_absolut) / length(consumption)
        Average_Error_building_my_model_percentage <- sum((consumption - rounded_predict_absolut) / consumption) / length(consumption) *100 # in %
        Max_Error_building_my_model_absolut <- max(consumption - rounded_predict_absolut)
        Min_Error_building_my_model_absolut <- min(consumption - rounded_predict_absolut)

        p <- print(ggplot(Div_con_predict_df, aes(x = Div_con_predict, after_stat(scaled))) + #, after_stat(scaled)
                geom_density(fill = "grey") + 
                xlab(paste(model," absolut error: consumption - rounded_predict_absolut", sep = "")) + 
                ylab("density")
                ) 
            print_path <- paste(model, "_Deviation_absolut_error", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(model, "_Deviation_absolut_error_S", ".pdf", sep = "") # PDF
            ggsave(print_path, p, width=2, height=2, units="in", scale=3)
            print_path <- paste(model, "_Deviation_absolut_error", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600)


        # Add results of my_model to data.frame
        Model_name <- model
        Selected_model_attributes[nrow(Selected_model_attributes) + 1, ] <- 
        list(Model_name, Rsquared_my_model, MAE_my_model, RMSE_my_model, Rsquared_my_model_absolut, MAE_my_model_absolut, RMSE_my_model_absolut)

        Selected_model_uncertainties[nrow(Selected_model_uncertainties) + 1, ] <-
        list(Model_name, Error_population_my_model_absolut, Error_population_my_model_percentage, Average_Error_building_my_model_absolut, Average_Error_building_my_model_percentage)
    #--------------------------------------------------
    # Summarize Attributes (Rsuqared, MAE, and RMSE) of the selected best model of each machine learning algorithem of the training runs
    # THESE ATTRIBUTES PRESENT THE VALUES OF THE APPLICATION ON THE 10th PART TESTING SUB-SAMPLE APPLICATION!!!!!
    #--------------------------------------------------
        
        # DataFrame
        Best_model_attributes <- rbind(Best_model_attributes, getTrainPerf(my_model))
        }

        par(mfrow = c(1, 1)) # Grafikfenster zu einem zur(aeoeuesss)cksetzen

        # print plot
        print_path <- paste(test_name, "Plots_Summary_RDS", ".pdf", sep = "") # PDF
        dev.print(pdf, print_path)

        print_path <- paste(test_name, "Plots_Summary_RDS", ".png", sep = "") # png
        # dev.print(png, file=print_path, width=1600, height=900) # Horizontal
        dev.print(png, file = print_path, width = 900, height = 1600) # Potrait

        path <- getwd()
        # Save Best_model_attributes in text document
        if (Problem_Type == "Regression") {
        Best_model_attributes <- Best_model_attributes[order(Best_model_attributes$TrainMAE), ] # TrainRsquared
        } else {
            Best_model_attributes <- Best_model_attributes[order(Best_model_attributes$TrainAccuracy), ]
        }
        sink(paste(path, "//", test_name, "Summary_Best_model_attributes", ".txt", sep = "")) # TESTING
        # sink("E:/BE_Reg_Verbrauch_Bedarf/H0_Heat/RF_Classification_TEST_Summary_Results.txt")
        print(Best_model_attributes)
        sink() # returns output to the console
        # Save as LaTex Table
        library(xtable)
        print(xtable(Best_model_attributes, type = "latex"), file = paste(test_name, "Summary_Best_model_attributes", ".tex", sep = ""))
        # Save as data.frame
        saveRDS(Best_model_attributes, file = paste(path, "//", test_name, "Summary_Best_model_attributes", ".RData", sep = ""))

        # Save Selected_model_attributes in text document
        Selected_model_attributes <- Selected_model_attributes[order(Selected_model_attributes$MAE), ] # R2
        sink(paste(path, "//", test_name, "Summary_Selected_model_attributes", ".txt", sep = "")) # TESTING
        # sink("E:/BE_Reg_Verbrauch_Bedarf/H0_Heat/RF_Classification_TEST_Summary_Results.txt")
        print(Selected_model_attributes)
        sink() # returns output to the console
        # Save as LaTex Table
        library(xtable)
        print(xtable(Selected_model_attributes, type = "latex"), file = paste(test_name, "Summary_Selected_model_attributes", ".tex", sep = ""))
        # Save as data.frame
        saveRDS(Selected_model_attributes, file = paste(path, "//", test_name, "Summary_Selected_model_attributes", ".RData", sep = ""))


        # Save Selected_model_uncertainties in text document
        Selected_model_uncertainties <- Selected_model_uncertainties[order(Selected_model_uncertainties$Error_population_my_model_percentage), ] 
        sink(paste(path, "//", test_name, "Summary_Selected_model_uncertainties", ".txt", sep = "")) # TESTING
        print(Selected_model_uncertainties)
        sink() # returns output to the console
        # Save as LaTex Table
        library(xtable)
        print(xtable(Selected_model_uncertainties, type = "latex"), file = paste(test_name, "Summary_Selected_model_uncertainties", ".tex", sep = ""))
        # Save as data.frame
        saveRDS(Selected_model_uncertainties, file = paste(path, "//", test_name, "Summary_Selected_model_uncertainties", ".RData", sep = ""))


        #..........................................................
        # Plot MAE over R2 of selected models
        R2 <- Selected_model_attributes$R2_Abs
        MAE <- Selected_model_attributes$MAE_Abs
        Model <- Selected_model_attributes$Model
        plot(R2, MAE, main = paste("MAE over Rsquared (absolut values)", collapse = ""), 
            ylab = "MAE in kWh/a", xlab = paste("Rsquared", collapse = " "), 
            xlim = c(0.5, 1), ylim = c(0, 120000),
            col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
            ) 
            abline(h=75000, col="#147dd3") # two models and DIBS are not shown in this selection
            text(0.85, 78000,  "MAE = 75,000",
            cex=0.65, pos=2,col="#147dd3") 
            abline(v=0.85, col="#18b13e")
            text(0.85, 100,  "Rsquared = 0.85",
            cex=0.65, pos=2,col="#18b13e") 
            text(R2, MAE, labels=Model, cex= 1, pos=2)
        # print plot
        print_path <- paste(test_name, "MAE_over_Rsquared_absolut_values_final_model", ".pdf", sep = "") # PDF
        dev.print(pdf, print_path)
        print_path <- paste(test_name, "MAE_over_Rsquared_absolut_values_final_model", ".png", sep = "") # png
        dev.print(png, file = print_path, width = 900, height = 1600) # Potrait
        print_path <- paste(test_name, "MAE_over_Rsquared_absolut_values_final_model_S", ".png", sep = "") # png
        dev.print(png, file = print_path, width = 600, height = 600) # Potrait

        #..........................................................
        # Plot Mean MAE (Best) over MEA of selected models
        comparison_best_selected_model_attributes <- merge(x = Best_model_attributes,
                y = Selected_model_attributes,
                by.x = "method",
                by.y = "Model")

        if (Problem_Type == "Regression") {
            Mean_MAE <- comparison_best_selected_model_attributes$TrainMAE
            MAE <- comparison_best_selected_model_attributes$MAE_Abs
            Model <- comparison_best_selected_model_attributes$method
            plot(Mean_MAE, MAE, main = paste("MAE final model over Mean_MAE, in kWh/a", collapse = ""), 
                ylab = "MAE final model", xlab = paste("Mean_MAE of 10-fold cross-validation", collapse = " "), 
                #xlim = c(0, 1), ylim = c(0, 1000),
                col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
                ) 
                # abline(0, 1, col = "blue")
                abline(h=80000, col="#147dd3") 
                text(200000, 84000,  "MAE final model = 80,000 kWh/a",
                cex=0.65, pos=4,col="#147dd3")
                abline(v=115000, col="#18b13e")
                text(115000, 249000,  "MeanMAE = 115.000 kWh/a",
                cex=0.65, pos=4,col="#18b13e") 
                text(Mean_MAE, MAE, labels=Model, cex= 1, pos=4)
            # print plot
            print_path <- paste(test_name, "MAE_final_over_Mean_MAE_absolut_values", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "MAE_final_over_Mean_MAE_absolut_values", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait
            print_path <- paste(test_name, "MAE_final_over_Mean_MAE_absolut_values_S", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600) # Potrait
        }

        if (Problem_Type == "Classification") {
            Mean_Accuracy <- comparison_best_selected_model_attributes$TrainAccuracy
            MAE <- comparison_best_selected_model_attributes$MAE_Abs
            Model <- comparison_best_selected_model_attributes$method
            plot(Mean_Accuracy, MAE, main = paste("MAE final model over Mean_Accuracy, in kWh/a and %", collapse = ""), 
                ylab = "MAE final model", xlab = paste("Mean_Accuracy of 10-fold cross-validation in %", collapse = " "), 
                #xlim = c(0, 1), ylim = c(0, 1000),
                col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
                ) 
                # abline(0, 1, col = "blue")
                abline(h=80000, col="#147dd3") 
                text(200000, 84000,  "MAE final model = 80,000 kWh/a",
                cex=0.65, pos=4,col="#147dd3")
                abline(v=0.5, col="#18b13e")
                text(115000, 249000,  "Mean_Accuracy = 0.5",
                cex=0.65, pos=4,col="#18b13e") 
                text(Mean_Accuracy, MAE, labels=Model, cex= 1, pos=4)
            # print plot
            print_path <- paste(test_name, "MAE_final_over_Mean_Accuracy_absolut_values", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "MAE_final_over_Mean_Accuracy_absolut_values", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait
            print_path <- paste(test_name, "MAE_final_over_Mean_Accuracy_absolut_values_S", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600) # Potrait
        }



    # ............................................................................
    # Comparing Klasse deviation of models...!
    # ............................................................................
        if (Problem_Type == "Classification") {
            Extrapolated_Results <- list()
            df_Klasse_Comparison <- df_prediction$klasse
            klasse <- df_prediction$klasse
            df_Klasse_Comparison <- as.data.frame(df_Klasse_Comparison)

            # Save summary results in text document
            path <- getwd()

            for (model in caret_models) {
                # model <- "svmLinear"
                set.seed(5) # allow reproducability of results

                # measure time of model loading and prediction (START)
                start_time <- Sys.time()


                # ..............................
                # load the trained model from Working Directory Path use:
                # ..............................
                
                # Best Models
                my_model <- readRDS(paste(Save_path, model, "__", test_name, ".rds", sep = "", collapse = ""))
                # ..............................
                # Calibration of DIBS_BE demand
                # ..............................
                predict_output <- predict.train(my_model, df_prediction, se.fit = FALSE)

                if (Problem_Type == "Regression") {
                rounded_predict <- round(predict_output, digits = 4)
                } else {
                rounded_predict <- round(stuetzstellen[predict_output] * df_prediction$en_dem, digits = 4)
                }

                # measure time of model loading and prediction (END)
                end_time <- Sys.time()
                modelling_time <- end_time - start_time
                time_unit <- units(modelling_time)

                print(paste("applied model", as.character(model), "in [", time_unit, "]:", modelling_time, collapse = ))

                # ..............................
                # Add rounded_predict Values to BE data set and save
                # ..............................
                Klasse_div <- as.integer(klasse) - as.integer(predict_output)
                XXX <- paste("df_Klasse_Comparison$", my_model$method, "_Klasse_Deviation", " <- Klasse_div", sep = "", collapse = "")
                XXX <- parse(text = XXX)
                eval(XXX)
            }

            # Save deviation of best model Klasse prediction to text document
            path <- getwd()
            sink(paste(path, "//", test_name, "Deviation_Klasse_Best_Model_of_Real_Klasse", ".txt", sep = ""))
            print("The deviation between the predicted Klasse of the model (the model is the best performing model of each training run 5 times 10 parts cross validation -> 50 models).")
            print("Therefore the best performing model is used to predict its training sample Klasses...")
            print("The deviation is calculated as Klasse_div <- as.integer(klasse) - as.integer(predict_output)")
            print("Negative values describe a Klasse of a greater correction factor than requiered for the building.")
            print("Positive values describe a Klasse of a smaller correction factor than requiered for the building.")
            print("0 is a perfect prediction")
            print("df_Klasse_Comparison is the real attribution of Klasse to the buildings (No deviation)")
            names <- names(df_Klasse_Comparison)
            
            for (name in names) {
                # name <- svmLinear_Klasse_Deviation
                XXX <- paste("table(df_Klasse_Comparison$", name, ")", sep = "")
                XXX <- parse(text = XXX)

                print(eval(XXX))
                print(name)
            }
            sink() # returns output to the console
        }


    # ..............................................................................................
    # saving Model Training results table to Latex
    # ..............................................................................................

        # ............................................
        # Regression
        # ............................................

            if (Problem_Type == "Regression") {
                print(summary(results))

                library(tidyr)
                library(dplyr)
                library(data.table)

                dat <- results

                # Ergebnisse der CV herausholen
                dt <- dat$values

                df <- dt %>%
                    pivot_longer(-c("Resample"),
                    names_to = c(".value", "Metrik"),
                    names_pattern = "(.*)(\\~.*)"
                    ) %>%
                    data.table()

                df$Metrik <- gsub("~", "", df$Metrik)
                df <- df[, 2:dim(df)[2]]

                # Output generieren
                df_MAE <- summary(df[Metrik == "MAE"]) %>% as.data.table()
                df_MAE[, V1 := NULL]
                df_MAE <- separate(data = df_MAE, col = N, into = c("Konzept", "Wert"), sep = "\\:")
                #df_MAE <- df_MAE[!(V2 %like% "Metrik")]
                #df_MAE <- df_MAE[!(df_MAE$V2 %like% "Metrik")]
                #df_MAE <- df_MAE[!(df_MAE$V2 == "   Metrik")]
                df_MAE <- df_MAE[df_MAE$V2 != "   Metrik", ]
                df_MAE$Metrik <- "MAE"
                colnames(df_MAE) <- c("Verfahren", "Konzept", "Wert", "Metrik")
                df_MAE <- df_MAE %>%
                    pivot_wider(names_from = Konzept, values_from = Wert)

                df_RMSE <- summary(df[Metrik == "RMSE"]) %>% as.data.table()
                df_RMSE[, V1 := NULL]
                df_RMSE <- separate(data = df_RMSE, col = N, into = c("Konzept", "Wert"), sep = "\\:")
                #df_RMSE <- df_RMSE[!(V2 %like% "Metrik")]
                df_RMSE <- df_RMSE[df_RMSE$V2 != "   Metrik", ]
                df_RMSE$Metrik <- "RMSE"
                colnames(df_RMSE) <- c("Verfahren", "Konzept", "Wert", "Metrik")
                df_RMSE <- df_RMSE %>%
                    pivot_wider(names_from = Konzept, values_from = Wert)

                df_Rsquared <- summary(df[Metrik == "Rsquared"]) %>% as.data.table()
                df_Rsquared[, V1 := NULL]
                df_Rsquared <- separate(data = df_Rsquared, col = N, into = c("Konzept", "Wert"), sep = "\\:")
                #df_Rsquared <- df_Rsquared[!(V2 %like% "Metrik")]
                df_Rsquared <- df_Rsquared[df_Rsquared$V2 != "   Metrik", ]
                df_Rsquared$Metrik <- "Rsquared"
                colnames(df_Rsquared) <- c("Verfahren", "Konzept", "Wert", "Metrik")
                df_Rsquared <- df_Rsquared %>%
                    pivot_wider(names_from = Konzept, values_from = Wert)

                # Konsolidieren
                df_out <- bind_rows(df_MAE, df_RMSE, df_Rsquared) %>% arrange(Verfahren, Metrik)

                library(xtable)
                print(xtable(df_Rsquared, type = "latex"), file = paste(test_name, "Rsquared", ".tex", sep = ""))
                print(xtable(df_RMSE, type = "latex"), file = paste(test_name, "RMSE", ".tex", sep = ""))
                print(xtable(df_MAE, type = "latex"), file = paste(test_name, "MAE", ".tex", sep = ""))

                # Sorted comparison of Mean Values
                Models <- df_Rsquared$Verfahren
                Models <- as.data.frame(Models)
                Models$Models <- caret_models
                Models$MeanR2 <- df_Rsquared$`Mean   `
                # Rsquared$MaxR2 <- df_Rsquared$`Max.   `
                # $Mean <- as.numeric(Rsquared$Mean)
                Models$MeanMAE <- df_MAE$`Mean   `
                # Rsquared$MeanMEA <- as.numeric(Rsquared$MeanMEA)
                Models$MeanRMSE <- df_RMSE$`Mean   `
                dfR <- Models[order(Models$MeanMAE), ]
                library(xtable)
                print(xtable(dfR, type = "latex"), file = paste(test_name, "Sorted_by_MAE_Comparison_R2_MEA_RMSE", ".tex", sep = ""))
                dfR <- Models[order(Models$MeanR2), ]
                library(xtable)
                print(xtable(dfR, type = "latex"), file = paste(test_name, "Sorted_by_R2_Comparison_R2_MEA_RMSE", ".tex", sep = ""))
            }


        # ............................................
        # Classification
        # ............................................
        if (Problem_Type == "Classification") {
            print(summary(results))

            library(tidyr)
            library(dplyr)
            library(data.table)

            dat <- results

            # Ergebnisse der CV herausholen
            dt <- dat$values

            df <- dt %>%
                pivot_longer(-c("Resample"),
                names_to = c(".value", "Metrik"),
                names_pattern = "(.*)(\\~.*)"
                ) %>%
                data.table()

            df$Metrik <- gsub("~", "", df$Metrik)
            df <- df[, 2:dim(df)[2]]

            # Output generieren
            df_Accuracy <- summary(df[Metrik == "Accuracy"]) %>% as.data.table()
            df_Accuracy[, V1 := NULL]
            df_Accuracy <- separate(data = df_Accuracy, col = N, into = c("Konzept", "Wert"), sep = "\\:")
            #df_Accuracy <- df_Accuracy[!("V2" %like% "Metrik")]
            #df_Accuracy <- df_Accuracy[!(V2 %like% "Metrik")]
            df_Accuracy <- df_Accuracy[df_Accuracy$V2 != "   Metrik", ]
            df_Accuracy$Metrik <- "Accuracy"
            colnames(df_Accuracy) <- c("Verfahren", "Konzept", "Wert", "Metrik")
            df_Accuracy <- df_Accuracy %>%
                pivot_wider(names_from = Konzept, values_from = Wert)

            df_Kappa <- summary(df[Metrik == "Kappa"]) %>% as.data.table()
            df_Kappa[, V1 := NULL]
            df_Kappa <- separate(data = df_Kappa, col = N, into = c("Konzept", "Wert"), sep = "\\:")
            #df_Kappa <- df_Kappa[!("V2" %like% "Metrik")]
            #df_Kappa <- df_Kappa[!(V2 %like% "Metrik")]
            df_Kappa <- df_Kappa[df_Kappa$V2 != "   Metrik", ]
            df_Kappa$Metrik <- "Kappa"
            colnames(df_Kappa) <- c("Verfahren", "Konzept", "Wert", "Metrik")
            df_Kappa <- df_Kappa %>%
                pivot_wider(names_from = Konzept, values_from = Wert)

            # Konsolidieren
            df_out <- bind_rows(df_Accuracy, df_Kappa) %>% arrange(Verfahren, Metrik)

            # df_Accuracy <- df_Accuracy[-c(3, 4, 5, 6)] # Drop unneeded columns
            # df_Accuracy <- df_Accuracy[-c(1), ] # Drop unneeded row

            # df_Kappa <- df_Kappa[-c(3, 4, 5, 6)] # Drop unneeded columns
            # df_Kappa <- df_Kappa[-c(1), ] # Drop unneeded row

            library(xtable)
            print(xtable(df_Accuracy, type = "latex"), file = paste(test_name, "Accuracy", ".tex", sep = ""))
            print(xtable(df_Kappa, type = "latex"), file = paste(test_name, "Kappa", ".tex", sep = ""))

            # Sorted comparison of Mean Values
            Models <- df_Accuracy$Verfahren
            Models <- as.data.frame(Models)
            Models$Models <- caret_models
            Models$Mean_Accuracy <- as.double(df_Accuracy$`Mean   `)
            Models$Max_Accuracy <- as.double(df_Accuracy$`Max.   `)
            # $Mean <- as.numeric(Rsquared$Mean)
            Models$Mean_Kappa <- as.double(df_Kappa$`Mean   `)
            Models$Max_Kappa <- as.double(df_Kappa$`Max.   `)
            dfA <- Models[order(Models$Mean_Accuracy), ]
            library(xtable)
            print(xtable(dfA, type = "latex"), file = paste(test_name, "Sorted_Comparison_Accuracy_Kappa", ".tex", sep = ""))
        }


    # -------------------------------------------------------------------------------------------
    # Print MAE, RMSE and R2 boxplots over k-fold cross-validation
    # -------------------------------------------------------------------------------------------
        library(lattice)

        #bwplot(results, scales = "free", as.table = TRUE)

        if (Problem_Type == "Regression") {
            print(bwplot(results, layout = c(1, 3), scales = "free", as.table = TRUE))
            print_path <- paste(test_name, "BoxPlots_Summary_MAE_RMSE_R2", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "BoxPlots_Summary_MAE_RMSE_R2", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait  -- dev.print(png, file=print_path, width=1600, height=900) # Horizontal
        }

        if (Problem_Type == "Classification") {
            print(bwplot(results, layout = c(1, 2), scales = "free", as.table = TRUE))
            print_path <- paste(test_name, "BoxPlots_Summary_Accuracy_Kappa", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "BoxPlots_Summary_Accuracy_Kappa", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait  -- # dev.print(png, file=print_path, width=1600, height=900) # Horizontal
        }





# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Model application on Stock (case study sample) and Extrapolation 
#
# # ...................................
# # ...................................
# ...................................................................................................................................

    # ............................................................................
    #
    # Read input data .csv ####
    #
    # ............................................................................
        DB_DIBS_BE_HRF <- read.csv(
            paste(GitHub_Data_Access, "data_BE_DIBS_input_output", ".csv", sep = ""),
            header = TRUE,
            sep = ";",
            dec = ",",
            fill = TRUE,
            fileEncoding = "UTF-8"
        )

    # ............................................................................
    #
    # Prepare data_for_predict according to dt ####
    #
    # ............................................................................

        DB_DIBS_BE_HRF$HeatingEnergy_DIBS_BE <- as.double(DB_DIBS_BE_HRF$HeatingEnergy_DIBS_BE) # DB_DIBS_BE_HRF$HeatingEnergy_DIBS_BE
        DB_DIBS_BE_HRF$HotWaterEnergy_DIBS_BE <- as.double(DB_DIBS_BE_HRF$HotWaterEnergy_DIBS_BE) # DB_DIBS_BE_HRF$HotWaterEnergy_DIBS_BE
        DB_DIBS_BE_HRF$ElectricityDemandTotal_DIBS_BE <- as.double(DB_DIBS_BE_HRF$ElectricityDemandTotal_DIBS_BE) # DB_DIBS_BE_HRF$ElectricityDemandTotal_DIBS_BE
        DB_DIBS_BE_HRF$Cooling_Sys_Electricity_DIBS_BE <- as.double(DB_DIBS_BE_HRF$Cooling_Sys_Electricity_DIBS_BE) # DB_DIBS_BE_HRF$Cooling_Sys_Electricity_DIBS_BE
        DB_DIBS_BE_HRF$LightingDemand_DIBS_BE <- as.double(DB_DIBS_BE_HRF$LightingDemand_DIBS_BE) # DB_DIBS_BE_HRF$LightingDemand_DIBS_BE
        DB_DIBS_BE_HRF$ApplianceGains_DIBS_BE <- as.double(DB_DIBS_BE_HRF$ApplianceGains_DIBS_BE) # DB_DIBS_BE_HRF$ApplianceGains_DIBS_BE
        DB_DIBS_BE_HRF$hk_geb_agg <- as.double(DB_DIBS_BE_HRF$hk_geb_agg)


        # Full DIBS_BE BRE data set for calibration
            data_for_predict <- DB_DIBS_BE_HRF

        # Definition of Variables
            data_for_predict$EnergyRefArea_DIBS_BE <- as.double(data_for_predict$EnergyRefArea_DIBS_BE)
            EnergyRefArea_DIBS_BE <- as.double(data_for_predict$EnergyRefArea_DIBS_BE)

            # Heat.............................................
            data_for_predict$HeatingEnergy_DIBS_BE <- as.double(data_for_predict$HeatingEnergy_DIBS_BE)
            HeatingEnergy_DIBS_BE <- data_for_predict$HeatingEnergy_DIBS_BE
            
            # OHW ...............................................
            data_for_predict$HotWaterEnergy_DIBS_BE <- as.double(data_for_predict$HotWaterEnergy_DIBS_BE)

            # Electricity........................................
            data_for_predict$energy_demand_electricity__non_heat__clean_DIBS_BE <- 
            as.double(DB_DIBS_BE_HRF$Cooling_Sys_Electricity_DIBS_BE + 
                DB_DIBS_BE_HRF$LightingDemand_DIBS_BE + 
                DB_DIBS_BE_HRF$ApplianceGains_DIBS_BE)

            data_for_predict$geb_flaeche <- as.double(data_for_predict$geb_flaeche)

            data_for_predict$HRF <- as.double(data_for_predict$HRF)
            HRF <- data_for_predict$HRF

            data_for_predict_original <- data_for_predict


        # Bring relevant variables into correct data type
            data_for_predict$bak <- as.factor(data_for_predict$bak)
            data_for_predict$hk_geb_BE <- as.factor(data_for_predict$hk_geb_BE)
            data_for_predict$bak_grob <- as.factor(data_for_predict$bak_grob)
            data_for_predict$hk_geb_agg <- as.factor(data_for_predict$hk_geb_agg)
            data_for_predict$qh3_1 <- as.factor(data_for_predict$qh3_1)
            data_for_predict$w_erz_art_et <- as.factor(data_for_predict$w_erz_art_et)
            data_for_predict$qh1 <- as.factor(data_for_predict$qh1)

        #..........................
        # rename variables to shorter more handable names
        #..........................
            aim <- "prediction"
            data_for_predict <- phy.relevant.variables.datanwg.calibration.rename(dt = data_for_predict, target_name = target_name, DB_V_B_clean_ex_red = data_for_predict_original, aim = aim)

            str(data_for_predict)
            data_for_predict$n_og <- as.double(data_for_predict$n_og)
            data_for_predict$n_ug <- as.double(data_for_predict$n_ug)
            data_for_predict$aw_flantgedges <- as.double(data_for_predict$aw_flantgedges)
            data_for_predict$en_dem <- as.double(data_for_predict$en_dem)
            data_for_predict$d_fl_wueoa <- as.double(data_for_predict$d_fl_wueoa)
            data_for_predict$q25_1 <- as.double(data_for_predict$q25_1)
            data_for_predict$geb_flaeche <- as.double(data_for_predict$geb_flaeche)
            data_for_predict$f_ant_belueftet <- as.double(data_for_predict$f_ant_belueftet)
            data_for_predict$f_ant_beheizt <- as.double(data_for_predict$f_ant_beheizt)
            data_for_predict$aw_daemm_staerke_1 <- as.double(data_for_predict$aw_daemm_staerke_1)
            data_for_predict$EnergyRefArea_DIBS_BE <- as.double(data_for_predict$EnergyRefArea_DIBS_BE)
            data_for_predict$HRF <- as.double(data_for_predict$HRF)
            


    # ............................................................................
    #
    # Baue Dummy-Variablen ####
    #
    # ............................................................................
        # based on the above identified eta2 and hard coded generation of the dummy variables
        data_for_predict <- generate.dummy.variables.based.on.identified.eta2.datanwg(dt = data_for_predict)



    # ............................................................................
    #
    # Predict and Extrapolate ####
    #
    # ............................................................................

        Extrapolated_Results <- data.frame()

        # Save summary results in text document
            path <- getwd()
            sink(paste(path, "//", test_name, "Extrapolation_Results", ".txt", sep = ""))

            # model test set
            # caret_models <-
            # c("gaussprRadial",
            #     # "svmLinear",
            #     # "RRF",
            #     # "RRFglobal",
            #     # "ownn",
            #     "snn"
            #     #"gaussprRadial"
            # )
            # model <- "gaussprRadial"

        for (model in caret_models) {
            set.seed(5) # allow reproducability of results

            # measure time of model loading and prediction (START)
                start_time <- Sys.time()

            # load model
                my_model <- readRDS(paste(Save_path, model, "__", test_name, ".rds", sep = "", collapse = ""))

            # ..............................
            # Calibration of DIBS_BE demand
            # ..............................
                predict_output <- predict.train(my_model, data_for_predict, se.fit = FALSE)
                if (Problem_Type == "Regression") {
                    if (target == "en_cons") {
                    rounded_predict <- round(predict_output, digits = 4)
                    }
                    if (target == "delta") {
                    rounded_predict <- round(data_for_predict$en_dem - predict_output, digits = 4)
                    }
                } else {
                    rounded_predict <- round(stuetzstellen[predict_output] * data_for_predict$en_dem, digits = 4)
                }

            # measure time of model loading and prediction (END)
                end_time <- Sys.time()
                modelling_time <- end_time - start_time
                time_unit <- units(modelling_time)

            # print model and prediction time
                print("---------------------------------------")
                print(as.character(model))
                print("---------------------------------------")
                print(paste("applied model", as.character(model), "in [", time_unit, "]:", modelling_time, collapse = ))

                print("---------------")

            # Setting limits for 'reasonable' energy demand for checking for outliers of
            # calibrated demand (Just Informative... in case of many above or below the limits shows uncertain models)
                
                # Heat
                if (target_name == "en_cons_space_heating" | target_name == "delta_b_v") {
                    lower_limit <- 0
                    upper_limit <- 1000
                }
                # OHW
                if(target_name == "en_cons_ohw"){
                    lower_limit <- 0
                    upper_limit <- 1000
                }
                # Electricity
                if(target_name == "en_cons_electricity"){
                    lower_limit <- 0
                    upper_limit <- 1000
                }

                xo <- rounded_predict[(rounded_predict / EnergyRefArea_DIBS_BE) < lower_limit] / EnergyRefArea_DIBS_BE[(rounded_predict / EnergyRefArea_DIBS_BE) < lower_limit] # Number of buildings that are calibrated to < lower_limit !
                cases_below_lower_limit <- length(xo)
                print(paste(cases_below_lower_limit, " cases are corrected to a value below the lower limit = ", lower_limit, "kWh/m2*a"))

                mean_below_lower_limit <- mean(xo)
                print(paste(mean_below_lower_limit, " mean value below the lower limit in ", "kWh/m2*a"))
                min_below_lower_limit <- min(xo)
                print(paste(min_below_lower_limit, " min value below the lower limit in ", "kWh/m2*a"))
                max_below_lower_limit <- max(xo)
                print(paste(max_below_lower_limit, " max value below the lower limit in ", "kWh/m2*a"))

                xo_absolut <- xo * EnergyRefArea_DIBS_BE[(rounded_predict / EnergyRefArea_DIBS_BE) < lower_limit]
                xo_absolut_EnergyRefArea_DIBS_BE <- EnergyRefArea_DIBS_BE[(rounded_predict / EnergyRefArea_DIBS_BE) < lower_limit]
                xo_absolut_EnergyRefArea_DIBS_BE_sum <- sum(xo_absolut_EnergyRefArea_DIBS_BE)

                # total_outlier_deviation_bll <- (lower_limit - mean_below_lower_limit) * cases_below_lower_limit
                # if(is.nan(total_outlier_deviation_bll) == TRUE){
                #     total_outlier_deviation_bll <- 0
                # }
                # print(paste(total_outlier_deviation_bll, " total outlier deviation below the lower limit in ", "kWh/m2*a"))

                n_cases_predicted <- length(rounded_predict)
                mean_outlier_caused_error_bll <- ((lower_limit - mean_below_lower_limit) * cases_below_lower_limit * xo_absolut_EnergyRefArea_DIBS_BE_sum) / n_cases_predicted
                if(is.nan(mean_outlier_caused_error_bll) == TRUE){
                    mean_outlier_caused_error_bll <- 0
                }
                 mean_outlier_caused_error_bll <- round(mean_outlier_caused_error_bll, digits = 0)
                print(paste(mean_outlier_caused_error_bll, " mean outlier caused error below the lower limit in ", "kWh/a"))
                mean_EBF_DIBS_BE_BLL <- xo_absolut_EnergyRefArea_DIBS_BE_sum / cases_below_lower_limit

                print("---------------")

                xoo <- rounded_predict[(rounded_predict / EnergyRefArea_DIBS_BE) > upper_limit] / EnergyRefArea_DIBS_BE[(rounded_predict / EnergyRefArea_DIBS_BE) > upper_limit] # Number of buildings that are calibrated > upper_limit
                cases_above_upper_limit <- length(xoo)
                print(paste(cases_above_upper_limit, " cases are corrected to a value above the upper limit = ", upper_limit, "kWh/m2*a"))

                mean_above_upper_limit <- mean(xoo)
                print(paste(mean_above_upper_limit, " mean value above the upper limit in ", "kWh/m2*a"))
                min_above_upper_limit <- min(xoo)
                print(paste(min_above_upper_limit, " min value above the upper limit in ", "kWh/m2*a"))
                max_above_upper_limit <- max(xoo)
                print(paste(max_above_upper_limit, " max value above the upper limit in ", "kWh/m2*a"))

                xoo_absolut <- xoo * EnergyRefArea_DIBS_BE[(rounded_predict / EnergyRefArea_DIBS_BE) > upper_limit]
                xoo_absolut_EnergyRefArea_DIBS_BE <- EnergyRefArea_DIBS_BE[(rounded_predict / EnergyRefArea_DIBS_BE) > upper_limit]
                xoo_absolut_EnergyRefArea_DIBS_BE_sum <- sum(xoo_absolut_EnergyRefArea_DIBS_BE)

                # total_outlier_deviation_aul <- (mean_above_upper_limit - upper_limit) * cases_above_upper_limit
                # if(is.nan(total_outlier_deviation_aul) == TRUE){
                #     total_outlier_deviation_aul <- 0
                # }
                # print(paste(total_outlier_deviation_aul, " total outlier deviation above the upper limit in ", "kWh/m2*a"))

                n_cases_predicted <- length(rounded_predict)
                mean_outlier_caused_error_aul <- ((mean_above_upper_limit - upper_limit) * cases_above_upper_limit * xoo_absolut_EnergyRefArea_DIBS_BE_sum) / n_cases_predicted
                if(is.nan(mean_outlier_caused_error_aul) == TRUE){
                    mean_outlier_caused_error_aul <- 0
                }
                mean_outlier_caused_error_aul <- round(mean_outlier_caused_error_aul, digits = 0)
                print(paste(mean_outlier_caused_error_aul, " mean outlier caused error above the upper limit in ", "kWh/a"))
                mean_EBF_DIBS_BE_AUL <- xoo_absolut_EnergyRefArea_DIBS_BE_sum / cases_above_upper_limit

                print("---------------")


            # ..............................
            # Add rounded_predict Values to BE data set and save
            # ..............................
                XXX <- paste("DB_DIBS_BE_HRF$", target_name, "_", Problem_Type, "_", my_model$method, " <- rounded_predict", sep = "", collapse = "")
                XXX <- parse(text = XXX)
                eval(XXX)

                # Save predicted Klasse to data set too
                    if (Problem_Type == "Classification") {
                        XXX <- paste("DB_DIBS_BE_HRF$", target_name, "_", Problem_Type, "_", my_model$method, "_Klasse_Predicted", " <- predict_output", sep = "", collapse = "")
                        XXX <- parse(text = XXX)
                        eval(XXX)
                    }


                path <- GitHub_Data_Access # getwd()
                con <- file(paste(path, "data_BE_DIBS_input_output_calibrated_", test_name, ".csv", sep = ""), encoding = "UTF-8")
                write.csv(DB_DIBS_BE_HRF, file = con)



            # ............................................................................
            # Extrapolation ####
            # ............................................................................

                #..................................................................
                # Determination of the correction Factor of HRF due to -555 removal ####
                #..................................................................
                    HRF_555 <- 183262.3
                    HRF_Total <- 1797241
                    Upscaling_for_HRF_555 <- (HRF_555 + HRF_Total) / HRF_Total
                
                #........................................................................... 
                #........................................................................... 
                # Extrapolation of calculated Demand onto the entire German NDBS ####
                #........................................................................... 
                #...........................................................................    
                    HRF <-  as.double(DB_DIBS_BE_HRF$HRF)

                    # set cal_results - the to be extrapolated calibrated results
                        XXX <- paste("cal_results <- ", "DB_DIBS_BE_HRF$", target_name, "_", Problem_Type, "_", my_model$method, sep = "", collapse = "")
                        XXX <- parse(text = XXX)
                        eval(XXX)

                    # set variable.name
                        cal_results_name <- paste(target_name, "_", Problem_Type, "_", my_model$method, sep = "", collapse = "")

                    # check if function is in JBs functions?
                        DE.variable.TWh <-
                            extrapolation.DE.quantitiy.energy(
                                variable = cal_results,
                                variable.name = cal_results_name,
                                extrapol.factor = HRF,
                                subset.upscaling.factor = Upscaling_for_HRF_555
                            )

                    # print('Total annual building heat demand in the German non-domestic building stock, in TWh/(a):')
                        print(model)
                        DE.variable.TWh
                        print(paste(DE.variable.TWh, "TWh/a"))

                        print("---------------------------------------") #
                        print("-")



            # summarize the different models extrapolation in one list to handle results later
            Extrapolated_Results <- rbind(Extrapolated_Results, c(
                model, 
                DE.variable.TWh, 
                cases_below_lower_limit, 
                mean_below_lower_limit, 
                min_below_lower_limit, 
                max_below_lower_limit, 
                #total_outlier_deviation_bll,
                mean_outlier_caused_error_bll,
                mean_EBF_DIBS_BE_BLL,
                cases_above_upper_limit, 
                mean_above_upper_limit, 
                min_above_upper_limit, 
                max_above_upper_limit,
                #total_outlier_deviation_aul,
                mean_outlier_caused_error_aul,
                mean_EBF_DIBS_BE_AUL
            ))
        }

        col_names <- c(
            "Model",
            "DE_heat_TWh",
            "cases_below_lower_limit",
            "mean_below_lower_limit_kWh_m2_a",
            "min_below_lower_limit_kWh_m2_a",
            "max_below_lower_limit_kWh_m2_a",
            #"total_outlier_deviation_bll",
            "mean_outlier_caused_error_bll",
            "mean_EBF_DIBS_BE_BLL",
            "cases_above_upper_limit",
            "mean_above_upper_limit_kWh_m2_a",
            "min_above_upper_limit_kWh_m2_a",
            "max_above_upper_limit_kWh_m2_a",
            #"total_outlier_deviation_aul",
            "mean_outlier_caused_error_aul",
            "mean_EBF_DIBS_BE_AUL"
        )
        colnames(Extrapolated_Results) <- col_names

        sink() # returns output to the console



# ............................................................................
# ............................................................................
#
# Saving Results ####
#
# ............................................................................
# ............................................................................

    #Round Values
        Extrapolated_Results$mean_below_lower_limit_kWh_m2_a <- round(as.double(Extrapolated_Results$mean_below_lower_limit_kWh_m2_a), digits = 2)
        Extrapolated_Results$min_below_lower_limit_kWh_m2_a <- round(as.double(Extrapolated_Results$min_below_lower_limit_kWh_m2_a), digits = 2)
        Extrapolated_Results$max_below_lower_limit_kWh_m2_a <- round(as.double(Extrapolated_Results$max_below_lower_limit_kWh_m2_a), digits = 2)

        Extrapolated_Results$mean_above_upper_limit_kWh_m2_a <- round(as.double(Extrapolated_Results$mean_above_upper_limit_kWh_m2_a), digits = 2)
        Extrapolated_Results$min_above_upper_limit_kWh_m2_a <- round(as.double(Extrapolated_Results$min_above_upper_limit_kWh_m2_a), digits = 2)
        Extrapolated_Results$max_above_upper_limit_kWh_m2_a <- round(as.double(Extrapolated_Results$max_above_upper_limit_kWh_m2_a), digits = 2)

        Extrapolated_Results$DE_heat_TWh <- round(as.double(Extrapolated_Results$DE_heat_TWh), digits = 6)

        # Extrapolated_Results$total_outlier_deviation_bll <- round(as.double(Extrapolated_Results$total_outlier_deviation_bll), digits = 2)
        # Extrapolated_Results$total_outlier_deviation_aul <- round(as.double(Extrapolated_Results$total_outlier_deviation_aul), digits = 2)

        Extrapolated_Results$mean_outlier_caused_error_bll <- round(as.double(Extrapolated_Results$mean_outlier_caused_error_bll), digits = 0)
        Extrapolated_Results$mean_outlier_caused_error_aul <- round(as.double(Extrapolated_Results$mean_outlier_caused_error_aul), digits = 0)

        Extrapolated_Results$mean_EBF_DIBS_BE_BLL <- round(as.double(Extrapolated_Results$mean_EBF_DIBS_BE_BLL), digits = 2)
        Extrapolated_Results$mean_EBF_DIBS_BE_AUL <- round(as.double(Extrapolated_Results$mean_EBF_DIBS_BE_AUL), digits = 2)
        mean_EBF_DIBS_BE_BLL

    # Rename the colum names
        col_names <- c(
            "Model",
            "DE_heat_TWh",
            "cases_BLL",
            "mean_BLL",
            "min_BLL",
            "max_BLL",
            #"TOD_BLL",
            "MOCPU_BLL",
            "mean_EBF_BLL",
            "cases_AUL",
            "mean_AUL",
            "min_AUL",
            "max_AUL",
            #"TOD_AUL",
            "MOCPU_AUL",
            "mean_EBF_AUL"
        )
        colnames(Extrapolated_Results) <- col_names

    # Save Extrapolated_Results
        con <- file(paste(path, "//", test_name, "Extrapolated_Results", ".csv", sep = ""), encoding = "UTF-8")
        write.csv(Extrapolated_Results, file = con)

        # install.packages("xtable")
        library(xtable)
        print(xtable(Extrapolated_Results, type = "latex"), file = paste(path, "//", test_name, "Extrapolated_Results", ".tex", sep = ""))

        sink(paste(path, "//", test_name, "Extrapolation_Results_Summary", ".txt", sep = ""))
        print(Extrapolated_Results)
        sink()

    # Reduced export of Extraploated_Results
        Extrapolated_Results_Selection <- Extrapolated_Results
        Extrapolated_Results_Selection$DE_heat_TWh <- NULL

        Extrapolated_Results_Selection <-
        subset(Extrapolated_Results_Selection, Extrapolated_Results_Selection$cases_BLL != 0 & 
        Extrapolated_Results_Selection$cases_AUL != 0)

        library(xtable)
        print(xtable(Extrapolated_Results_Selection, type = "latex"), file = paste(path, "//", test_name, "Extrapolated_Results_No_Stock_Results", ".tex", sep = ""))


    # select columns form data frame
        Extrapolated_Results_BLL_AUL <- Extrapolated_Results %>% select(Model, cases_BLL, MOCPU_BLL, mean_EBF_BLL, cases_AUL, MOCPU_AUL, mean_EBF_AUL)


    if (Problem_Type == "Regression") {
        # merge data frames by spalten (dfR are the mean values of the cross-validation)
            Model_k_fold_cross_validation_performance_and_Extrapolation <- merge(x = dfR,
                    y = Extrapolated_Results_BLL_AUL,
                    by.x = "Models",
                    by.y = "Model")

        # merge data frames by spalten (All_regression_models are the types of model category)
            Model_k_fold_cross_validation_performance_and_Extrapolation <- 
            merge(x = Model_k_fold_cross_validation_performance_and_Extrapolation,
                    y = All_regression_models,
                    by.x = "Models",
                    by.y = "caret_models")

        # Order Rows by value 
            Model_k_fold_cross_validation_performance_and_Extrapolation$MeanMAE <- as.double(Model_k_fold_cross_validation_performance_and_Extrapolation$MeanMAE)
            Model_k_fold_cross_validation_performance_and_Extrapolation <- 
            Model_k_fold_cross_validation_performance_and_Extrapolation[order(Model_k_fold_cross_validation_performance_and_Extrapolation$MeanMAE), ]

        # save as tex
            library(xtable)
            print(xtable(Model_k_fold_cross_validation_performance_and_Extrapolation, type = "latex"), 
            file = paste(test_name, "Sorted_by_MAE_Performance", ".tex", 
            sep = ""))
    }

    if (Problem_Type == "Classification") {
        # merge data frames by spalten (dfA are the mean values of the cross-validation)
            Model_k_fold_cross_validation_performance_and_Extrapolation <- merge(x = dfA,
                    y = Extrapolated_Results_BLL_AUL,
                    by.x = "Models",
                    by.y = "Model")

        # merge data frames by spalten (All_regression_models are the types of model category)
            Model_k_fold_cross_validation_performance_and_Extrapolation <- 
            merge(x = Model_k_fold_cross_validation_performance_and_Extrapolation,
                    y = All_Classification_models,
                    by.x = "Models",
                    by.y = "caret_models")

        # Order Rows by value 
            Model_k_fold_cross_validation_performance_and_Extrapolation$Mean_Accuracy <- as.double(Model_k_fold_cross_validation_performance_and_Extrapolation$Mean_Accuracy)
            Model_k_fold_cross_validation_performance_and_Extrapolation <- 
            Model_k_fold_cross_validation_performance_and_Extrapolation[order(Model_k_fold_cross_validation_performance_and_Extrapolation$Mean_Accuracy), ]

        # save as tex
            library(xtable)
            print(xtable(Model_k_fold_cross_validation_performance_and_Extrapolation, type = "latex"), 
            file = paste(test_name, "Sorted_by_Accuracy_Performance", ".tex", 
            sep = ""))
    }

    # ............................................................................
    # Generate Subset of Selected_model_attributes (performance of best "final" models on entire training data set) and add cases of BLL and AUL ####
    # ............................................................................
        # select columns from data frame
            Selected_model_attributes_Abs <- Selected_model_attributes %>% select(Model, R2_Abs, MAE_Abs, RMSE_Abs)
        # Rename Cols again to fit to other tables
            col_names <- c(
                "Model",
                "R2",
                "MAE",
                "RMSE"
            )
            colnames(Selected_model_attributes_Abs) <- col_names
        # merge data frames by spalten
            Selected_model_attributes_Abs <- 
            merge(x = Selected_model_attributes_Abs,
                    y = Extrapolated_Results_BLL_AUL,
                    by.x = "Model",
                    by.y = "Model")
        # Add DIBS to dataframe again for reference, as it was removed by merge
            Model_name <- "DIBS"
            Selected_model_attributes_Abs[nrow(Selected_model_attributes_Abs) + 1, ] <- 
            list(Model_name, Rsquared_DIBS_absolut, MAE_DIBS_absolut, RMSE_DIBS_absolut, 0, 0, "NaN", 0, 0, 0)
        # Order Rows by value 
            Selected_model_attributes_Abs$MAE <- as.double(Selected_model_attributes_Abs$MAE)
            Selected_model_attributes_Abs <- 
            Selected_model_attributes_Abs[order(Selected_model_attributes_Abs$MAE), ]
        # save as tex
            library(xtable)
            print(xtable(Selected_model_attributes_Abs, type = "latex"), 
            file = paste(test_name, "Sorted_by_MAE_Final_Model_Performance", ".tex", 
            sep = ""))


    # ............................................................................
    # Generate Subset of for model selection ####
    # ............................................................................
        # select columns from data frame
            Selected_model_attributes_Abs <- Selected_model_attributes_Abs %>% select(Model, R2, MAE, RMSE)

        # merge data frames by spalten
            DF_model_selection <- 
            merge(x = Selected_model_attributes_Abs,
                    y = Model_k_fold_cross_validation_performance_and_Extrapolation,
                    by.x = "Model",
                    by.y = "Models")

            # Fix data types
            DF_model_selection$cases_AUL <- as.double(DF_model_selection$cases_AUL)
            DF_model_selection$cases_BLL <- as.double(DF_model_selection$cases_BLL)
            
            if (Problem_Type == "Regression") {
            DF_model_selection$MeanMAE <- as.double(DF_model_selection$MeanMAE)
            DF_model_selection$MeanRMSE <- as.double(DF_model_selection$MeanRMSE)
            DF_model_selection$MeanR2 <- as.double(DF_model_selection$MeanR2)
            }
            
            if (Problem_Type == "Classification") {
            DF_model_selection$cases_BLL <- as.double(DF_model_selection$Mean_Accuracy)
            DF_model_selection$cases_BLL <- as.double(DF_model_selection$Mean_Kappa)
            }

  
    # save Environment with all results for independent plotting etc.
    path <- getwd()
    save.image(file = paste(path, "//", test_name, "Training_and_Extrapolation_Environment", ".RData", sep = ""))



# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Model selection
#
# # ...................................
# # ...................................
# ...................................................................................................................................
MOCPU <- as.double(DF_model_selection$MOCPU_BLL) + as.double(DF_model_selection$MOCPU_AUL)

if (Problem_Type == "Regression") {
    DF_model_selection_MAE_smaller_MeanMAE <-
                    subset(DF_model_selection, DF_model_selection$MAE < DF_model_selection$MeanMAE)

    print("The model was selected based on the follwing equally important criteria:")

    print("Criteria 1")
    print("The final model, trained and applied to the small data set should perform better ")
    print("than the algorithms average performance in the 10-fold cross validation.")
    print("As otherwise good performance of the final model was based on pure chance ")
    print("and in application to an unknown set, the final model does not perform well.")
    print("MAE < MeanMAE")

    
    Mean_MAE <- as.double(DF_model_selection$MeanMAE)

    # Plot MOCPU over MeanMAE
    plot(Mean_MAE, MOCPU, main = paste("MOCPU final model applied to large dataset over Mean_MAE, in kWh/a", collapse = ""), 
                ylab = "MOCPU final model applied to large dataset", xlab = paste("Mean_MAE of 10-fold cross-validation", collapse = " "), 
                #xlim = c(0, 1), ylim = c(0, 1000),
                col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
                ) 
                # # abline(0, 1, col = "blue")
                # abline(h=80000, col="#147dd3") 
                # text(200000, 84000,  "MAE final model = 80,000 kWh/a",
                # cex=0.65, pos=4,col="#147dd3")
                # abline(v=115000, col="#18b13e")
                # text(115000, 249000,  "MeanMAE = 115.000 kWh/a",
                # cex=0.65, pos=4,col="#18b13e") 
                text(Mean_MAE, MOCPU, labels=Model, offset = 0.5, cex= 1, pos=4, srt=90)
            # print plot
            print_path <- paste(test_name, "MOCPU_final_over_Mean_MAE_absolut_values", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "MOCPU_final_over_Mean_MAE_absolut_values", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait
            print_path <- paste(test_name, "MOCPU_final_over_Mean_MAE_absolut_values_S", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600) # Potrait

    # Plot MOCPU over MeanMAE PART
    xmin <- min(Mean_MAE) - 1000
    plot(Mean_MAE, MOCPU, main = paste("MOCPU final model applied to large dataset over Mean_MAE, in kWh/a", collapse = ""), 
                ylab = "MOCPU final model applied to large dataset", xlab = paste("Mean_MAE of 10-fold cross-validation", collapse = " "), 
                xlim = c(xmin, 125000), ylim = c(0, 40000),
                col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
                ) 
                # # abline(0, 1, col = "blue")
                # abline(h=80000, col="#147dd3") 
                # text(200000, 84000,  "MAE final model = 80,000 kWh/a",
                # cex=0.65, pos=4,col="#147dd3")
                # abline(v=115000, col="#18b13e")
                # text(115000, 249000,  "MeanMAE = 115.000 kWh/a",
                # cex=0.65, pos=4,col="#18b13e") 
                text(Mean_MAE, MOCPU, labels=Model, offset = 0.5, cex= 1, pos=4, srt=90)
                #install.packages('plotrix')
                library(plotrix)
                # x <- DF_model_selection$MeanMAE[DF_model_selection$Model == "svmPoly"]
                # y <- MOCPU[DF_model_selection$Model == "svmPoly"]
                # draw.circle(x=x, y=y, radius=200, border='red') # col='lightblue', lwd=5, lty='dashed'
                x <- DF_model_selection$MeanMAE[DF_model_selection$Model == "qrf"]
                y <- MOCPU[DF_model_selection$Model == "qrf"]
                draw.circle(x=x, y=y, radius=200, border='#ff0000') # col='lightblue', lwd=5, lty='dashed'
            # print plot
            print_path <- paste(test_name, "MOCPU_final_over_Mean_MAE_absolut_values_Fokus", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "MOCPU_final_over_Mean_MAE_absolut_values_Fokus", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait
            print_path <- paste(test_name, "MOCPU_final_over_Mean_MAE_absolut_values_Fokus_S", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600) # Potrait
}

if (Problem_Type == "Classification") {
    print("Criteria 1 MAE < MeanMAE for the Regression Case does not apply here.")
    # An option would be to calculate the Accuracy and compare to MeanAccuracy

    Mean_Accuracy <- as.double(DF_model_selection$Mean_Accuracy)

    # Plot MOCPU over MeanAccuracy
    plot(Mean_Accuracy, MOCPU, main = paste("MOCPU final model applied to large dataset over Mean_Accuracy, in kWh/a", collapse = ""), 
                ylab = "MOCPU final model applied to large dataset", xlab = paste("Mean_Accuracy of 10-fold cross-validation", collapse = " "), 
                #xlim = c(0, 1), ylim = c(0, 1000),
                col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
                ) 
                # abline(0, 1, col = "blue")
                abline(h=10000, col="#147dd3") 
                text(0.12, 15000,  "MOCPU final model = 10,000 kWh/a",
                cex=0.65, pos=4,col="#147dd3")
                abline(v=0.2, col="#18b13e")
                text(100000, 0.16,  "Mean_Accuracy = 0.2 kWh/a",
                cex=0.65, pos=4,col="#18b13e") 
                text(Mean_Accuracy, MOCPU, labels=Model, cex= 1, pos=4)
            # print plot
            print_path <- paste(test_name, "MOCPU_final_over_Mean_Accuracy_absolut_values", ".pdf", sep = "") # PDF
            dev.print(pdf, print_path)
            print_path <- paste(test_name, "MOCPU_final_over_Mean_Accuracy_absolut_values", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 900, height = 1600) # Potrait
            print_path <- paste(test_name, "MOCPU_final_over_Mean_Accuracy_absolut_values_S", ".png", sep = "") # png
            dev.print(png, file = print_path, width = 600, height = 600) # Potrait
}


    # # Select rows (model) with no BLL and AUL cases where the MAE (final model) is smaler than the MeanMAE (cross-validation)
    #     # DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE <-
    #     #         subset(DF_model_selection, DF_model_selection$cases_BLL == 0 & 
    #     #         DF_model_selection$cases_AUL == 0 & DF_model_selection$MAE < DF_model_selection$MeanMAE)

    #     DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE <-
    #             # subset(DF_model_selection, DF_model_selection$TOD_BLL < DF_model_selection$MeanMAE & 
    #             # DF_model_selection$TOD_AUL < DF_model_selection$MeanMAE & DF_model_selection$MAE < DF_model_selection$MeanMAE)
    #             subset(DF_model_selection, (as.double(DF_model_selection$MOCPU_BLL) + as.double(DF_model_selection$MOCPU_AUL)) < DF_model_selection$MeanMAE & 
    #             DF_model_selection$MAE < DF_model_selection$MeanMAE) # The sum outside the outlier boundaries is to be below the meanMAE
                
    #     DF_model_selection_MAE_smaller_MeanMAE <-
    #             subset(DF_model_selection, DF_model_selection$MAE < DF_model_selection$MeanMAE)
    
    # # Order Rows by value 
    #     # DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE <- 
    #     # DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE[order(DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE$MeanMAE), ]

    #     DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE <- 
    #     DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE[order(DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE$MeanMAE), ]

    # # Select Model
    #     # selected_model <- DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE$Model[DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE$MeanMAE == min(DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE$MeanMAE)]
    #     selected_model <- DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE$Model[DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE$MeanMAE == min(DF_model_selection_TOD_BLL_AUL_below_meanMAE_and_MAE_smaller_MeanMAE$MeanMAE)]
    #     print("The model was selected based on the follwing equally important criteria:")

    #     print("Criteria 1")
    #     print("The final model, trained and applied to the small data set should perform better ")
    #     print("than the algorithms average performance in the 10-fold cross validation.")
    #     print("As otherwise good performance of the final model was based on pure chance ")
    #     print("and in application to an unknown set, the final model does not perform well.")
    #     print("MAE < MeanMAE")
        
    #     print("Criteria 2")
    #     print("The selected model should not predict outliers, above or below the set upper and lower limit, ")
    #     print("which add significant Errors to the individuall cases. Significant Error are predicted in all cases where")
    #     print("the mean outlier caused population uncertainty (MOCPU) is above the MeanMAE (performance of the algorithem in the 10-fold cross validation).")
    #     print("As in these cases, alone the outliers would  allready generate a MAE equivalent of about of the algorithms average performance.")
    #     print("In consequence, the errors of this model application are most likly much higher. Therefore, such models are not considered for the calibration task.")
    #     print("MOCPU < MeanMAE")

    #     print("Final Selection")
    #     print("The models meeting criteria 1 and 2 present the shortlist of generally suitabel models.")
    #     print("The performance of these models, applied to an unknown building stock is best described in the MeanMAE of the 10-fold cross-validation.")
    #     print("The model with lowest MeanMAE will most likely perform best when applied to an unknown data set and is therfore selected.")
    #     print(paste("The selected model for ", test_name, " is: ", selected_model, sep = ""))
    #     print("...")
    #     print(selected_model)
    #     print("...")

    #                 # selected model based on results above
    #                 # The selected model should have no cases above or below the upper and lower limit set and at the same time the model should 
    #                 # perform better on the entire training sample (MAE) than on average in the cross-validation (meanMAE). Of these the one with the lowest meanMAE 
    #                 # is selected.

    # # save as tex
    #     library(xtable)
    #     print(xtable(DF_model_selection_no_BLL_AUL_cases_and_MAE_smaller_MeanMAE, type = "latex"), 
    #     file = paste(test_name, "Selected_Model_Ranking", ".tex", 
    #     sep = "")) 





# ...................................................................................................................................
# # ...................................
# # ...................................
#
# # Calculation of stock value (target) and statistical uncertainties
#
# # ...................................
# # ...................................
# ...................................................................................................................................


# DEFINE SELECTED MODEL!!!
# Default is model with samlest Mean MAE
selected_model <- Model_k_fold_cross_validation_performance_and_Extrapolation[1,1]
#
##
###
####
#####
######
#######
########
#########
##########
###########
############
#############
##############


if (Problem_Type == "Regression") {
    # initiate functions

    n.internal.datanwg <- function(df){
        df$one[df$bl > 0] <- 1
        N <- bretotal(~one, df, Dmats)
        N_value <- N[1, 1]
        N_SE <- N[1, 2]
        N_SE_perc <- N_SE/N_value  
        return(N)
    }

    n.datanwg <- function(df){
        # determination of number of e.g. GEG relevant Buildgs and their uncertainties
        #
        # df - subset of df.gebtab.bre which represents the sub stock for which the number of buildings is to determined
        #
        N <- n.internal.datanwg(df)
        N_value <- N[1, 1]
        return(N_value)
    }

    n.se.datanwg <- function(df){
        # determination of number of e.g. GEG relevant Buildgs and their uncertainties
        #
        # df - subset of df.gebtab.bre which represents the sub stock for which the number of buildings is to determined
        #
        N <- n.internal.datanwg(df)
        N_SE <- N[1, 2]
        return(N_SE)
    }

    n.se.perc.datanwg <- function(df){
        # determination of number of e.g. GEG relevant Buildgs and their uncertainties
        #
        # df - subset of df.gebtab.bre which represents the sub stock for which the number of buildings is to determined
        #
        N <- n.internal.datanwg(df)
        N_value <- N[1, 1]
        N_SE <- N[1, 2]
        N_SE_perc <- N_SE/N_value  
        return(N_SE_perc)
    }






    # Definiton of the sub-sample of GEG relevant Buildings 
    # 4.2 GEG-relevante NWG  
        df.geg <- subset(df.gebtab.bre,enev_rel_nwg_be == 1)

        # Define the vector with the results of the selected model
            XXX <- paste("cal_results_selected_model <- ", "DB_DIBS_BE_HRF$", target_name, "_", Problem_Type, "_", selected_model, sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)

            cal_results_selected_model_name <- paste(target_name, "_", Problem_Type, "_", selected_model, sep = "", collapse = "")


    # prepare reults for extrapolation and determination of uncertainties
        df.result <- as.data.frame(cbind(DB_DIBS_BE_HRF$scr_gebaeude_id, 
                                        cal_results_selected_model)) 
        colnames(df.result)<-c('scr_gebaeude_id',
                            cal_results_selected_model_name)

        # add df.result to df.geg
            df.geg.new <- left_join(df.geg,df.result,by='scr_gebaeude_id')

        #df.geg.new$Heat <- as.double(df.geg.new$Heat)
            XXX <- paste("df.geg.new$", cal_results_selected_model_name, " <- ", "as.double(df.geg.new$", cal_results_selected_model_name, ")", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)

        #df.geg.new$Heat.valid <- df.geg.new$Heat >= 0
            XXX <- paste("df.geg.new$", cal_results_selected_model_name, ".valid", " <- ", "df.geg.new$", cal_results_selected_model_name, ">= 0", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)


            XXX <- paste("df.geg.new$", cal_results_selected_model_name, sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)
            
    # determination of number of GEG relevant Buildgs and their uncertainties
        N_GEG_value <- n.datanwg(df.geg.new)
        N_GEG_SE <- n.se.datanwg(df.geg.new)
        N_GEG_SE_perc <- n.se.perc.datanwg(df.geg.new)


        # #df.geg.new.555<-subset(df.geg.new, is.na(Heat))
        # XXX <- paste("df.geg.new.555 <- subset(df.geg.new, is.na(df.geg.new$", cal_results_selected_model_name, "))", sep = "", collapse = "")
        # XXX <- parse(text = XXX)
        # eval(XXX)
        # HRF_555_tot <- bretotal(~one, df.geg.new.555)
        # HRF_555_tot_perc <- HRF_555_tot[1, 2]/ HRF_555_tot[1, 1]

        # #df.geg.new.not.555<-subset(df.geg.new, !(is.na(Heat)))
        # XXX <- paste("df.geg.new.not.555 <- subset(df.geg.new, !(is.na(df.geg.new$", cal_results_selected_model_name, ")))", sep = "", collapse = "")
        # XXX <- parse(text = XXX)
        # eval(XXX)
        # HRF_not_555_tot <- bretotal(~one, df.geg.new.not.555)
        # HRF_not_555_tot_perc <- HRF_not_555_tot[1, 2] / HRF_not_555_tot[1, 1]

        # Upscaling_for_HRF_555 <- N_GEG_value / HRF_not_555_tot[1, 1]
        # Upscaling_for_HRF_555_SD <- sqrt((HRF_555_tot_perc)^2 + (HRF_not_555_tot_perc)^2) * (N_GEG_value / HRF_not_555_tot[1, 1])
        

    # Remove missing Values
        #df.geg.new.valid <- subset(df.geg.new,Heat >= 0)
        #XXX <- paste("df.geg.new.valid", " <- ", "subset(df.geg.new, ", cal_results_selected_model_name, ">= 0)", sep = "", collapse = "")
        XXX <- paste("df.geg.new.valid", " <- ", "subset(df.geg.new, ", "df.geg.new$", cal_results_selected_model_name, ">= 0)", sep = "", collapse = "")
        XXX <- parse(text = XXX)
        eval(XXX)

    # Calculation of uncertainties
        # This "bretotal" Sum-Esitmator apparoch only for variables that exist in the total GEG-relevant sub-sample (5107 cases)
        # SD must be extrapolated by applying the Gausian Error accumulation (Fehlerfortpflanzung)
            # Heat_extra_uncer <- bretotal(~Heat, df.geg.new.valid, Dmats)
            # Heat_V <- Heat_extra_uncer[1, 1]/1000000000 * Upscaling_for_HRF_555
            # Heat_SD <- Heat_extra_uncer[1, 2]/1000000000
            # Heat_SD_perc <- Heat_extra_uncer[1, 2] / Heat_extra_uncer[1, 1]

        # Here additionally the error of the Upscaling_for_HRF_555 must be considered:
        # Heat_SD_tot <- sqrt((N_GEG_SD_perc)^2 + (Heat_SD_perc)^2 + (Upscaling_for_HRF_555_SD)^2) * N_GEG_value * Heat_extra_uncer[1, 1] * Upscaling_for_HRF_555
        # Therefore the error incresas in case of using the bretotal function as the upscaling is requiered, which increses the error.
        # Only in cases with full datasets (in case of GEG relevant building 5107 cases), as than no upscaling for HRF -555 is requiered.

        # This "bremean" Mean-Estimator appraoch only for variables with missing values (e.g. -555):
        # in such a case the mean values are calculated of the requiered sub-sample (e.g. hk_geb == 1)
        # and multiplied with the fitting number of buildings of this sub-sample.
        # SD must be extrapolated by applying the Gausian Error accumulation (Fehlerfortpflanzung)
        # in case of + and - the main value is simply calculated c = a + b and the SD is calculated by DeltaC = (DeltaA/a + DeltaB/b) * c. 
        # For - the SDs are added as well.
        # in case of * and / the main value is also simply calculated c = a * b and the SD is calculated by DeltaC = sqrt((DeltaA/a)^2 + (DeltaB/b)^2) * c
        # For / the SDs are also added in this way
        # DeltaX/x equals the percentage SD error deviation
        
        #Heat_extra_uncer <- bremean(~Heat, df.geg.new.valid, Dmats)
        XXX <- paste("target_extra_uncer", " <- ", "bremean(~", cal_results_selected_model_name, ", df.geg.new.valid, Dmats)", sep = "", collapse = "")
        XXX <- parse(text = XXX)
        eval(XXX)
        target_V <- target_extra_uncer[1, 1]/1000000000 * N_GEG_value
        target_SE <- target_extra_uncer[1, 2]/1000000000
        target_SE_perc <- target_extra_uncer[1, 2] / target_extra_uncer[1, 1]
        target_SE_tot <- sqrt((N_GEG_SE_perc)^2 + (target_SE_perc)^2) * N_GEG_value * target_extra_uncer[1, 1]
        target_SE_tot <- target_SE_tot/1000000000


        sink(paste(path, "//", test_name, "Stock_Energy_selected_model_cal", ".txt", sep = "")) 
        print(paste("The selected model for ", test_name, " is: ", sep = ""))
        print(selected_model)
        print("of this selected model the extrapolated result is: ")
        print(target_V)
        print("in TWh;")
        print("with an uncertainty of: ")
        print(target_SE_tot)
        print("in TWh.")
        sink()
}






    # ...................................................................................................................................
    # # ...................................
    # # ...................................
    #
    # # Calculation of GHG-emission of target energy values calibrated by selected model and determination of statistical uncertainties
    #
    # # ...................................
    # # ...................................
    # ...................................................................................................................................
        
    if (Problem_Type == "Regression") {    
        # Set GHG-Emission Factors for Target 
            # Heat
            if (target_name == "en_cons_space_heating") {
                DB_DIBS_BE_HRF$Target_f_GHG..g.kWhHi. <- as.double(DB_DIBS_BE_HRF$Heating_f_GHG_DIBS_BE) #$Heating_f_GHG_DIBS_BE
            }

            # OHW
            if(target_name == "en_cons_ohw"){
                DB_DIBS_BE_HRF$Target_f_GHG..g.kWhHi. <- as.double(DB_DIBS_BE_HRF$Hotwater_f_GHG_DIBS_BE) #$Hotwater_f_GHG_DIBS_BE
            }

            # Electricity
            if(target_name == "en_cons_electricity"){
                DB_DIBS_BE_HRF$Target_f_GHG..g.kWhHi. <- as.double(DB_DIBS_BE_HRF$LightAppl_f_GHG_DIBS_BE) 
                #DB_DIBS_BE_HRF$Cooling_f_GHG..g.kWhHi. # only the electricity part of the cooling demand/enery is part of the electricty consideration -> no seperate f_GHG cooling requiered
            }


        # calculation of GHG-Emission values
            #DB_DIBS_BE_HRF$GHG_kg_space_heating_ranger_cal <- (DB_DIBS_BE_HRF$ranger * DB_DIBS_BE_HRF$Heating_f_GHG_DIBS_BE)/1000 #kg CO2eq
            XXX <- paste("DB_DIBS_BE_HRF$GHG_kg_", cal_results_selected_model_name, " <- ", "(DB_DIBS_BE_HRF$", cal_results_selected_model_name, " * ", "DB_DIBS_BE_HRF$Target_f_GHG..g.kWhHi.) / 1000", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)

        # Definiton of the sub-sample of GEG relevant Buildings 
            # 4.2 GEG-relevante NWG  
            df.geg <- subset(df.gebtab.bre,enev_rel_nwg_be == 1)

        # prepare reults for extrapolation and determination of uncertainties
            XXX <- paste("ghg_target", " <- ", "DB_DIBS_BE_HRF$GHG_kg_", cal_results_selected_model_name, sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)

            ghg_target_name <- paste("GHG_kg_", cal_results_selected_model_name, sep = "", collapse = "")

            df.result <- as.data.frame(cbind(DB_DIBS_BE_HRF$scr_gebaeude_id, 
                                            ghg_target)) 
            colnames(df.result)<-c('scr_gebaeude_id',
                                ghg_target_name)

            df.geg.new <- left_join(df.geg, df.result, by = 'scr_gebaeude_id')


            # df.geg.new$GHG_kg_space_heating_ranger_cal <- as.double(df.geg.new$GHG_kg_space_heating_ranger_cal)
            XXX <- paste("df.geg.new$", ghg_target_name, " <- ", "as.double(df.geg.new$", ghg_target_name, ")", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)
        
            # df.geg.new$GHG_kg_space_heating_ranger_cal.valid <- df.geg.new$GHG_kg_space_heating_ranger_cal >= 0
            XXX <- paste("df.geg.new$", ghg_target_name, ".valid <- ", "df.geg.new$", ghg_target_name, " >= 0", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)

        # determination of number of GEG relevant Buildgs and their uncertainties
            N_GEG_value <- n.datanwg(df.geg.new)
            N_GEG_SE <- n.se.datanwg(df.geg.new)
            N_GEG_SE_perc <- n.se.perc.datanwg(df.geg.new)


        # GHG_kg
        #Remove missing Values
            # df.geg.new.valid <- subset(df.geg.new, GHG_kg_space_heating_ranger_cal >= 0)
            XXX <- paste("df.geg.new.valid", " <- ", "subset(df.geg.new, ", ghg_target_name, " >= 0)", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)


                            # GHG_kg_space_heating_ranger_cal_extra_uncer <- bremean(~GHG_kg_space_heating_ranger_cal, df.geg.new.valid, Dmats)
                            # GHG_kg_space_heating_ranger_cal_V <- GHG_kg_space_heating_ranger_cal_extra_uncer[1, 1] * N_GEG_value
                            # GHG_kg_space_heating_ranger_cal_SD <- GHG_kg_space_heating_ranger_cal_extra_uncer[1, 2]
                            # GHG_kg_space_heating_ranger_cal_SD_perc <- GHG_kg_space_heating_ranger_cal_extra_uncer[1, 2] / GHG_kg_space_heating_ranger_cal_extra_uncer[1, 1]
                            # GHG_kg_space_heating_ranger_cal_SD_tot <- sqrt((N_GEG_SD_perc)^2 + (GHG_kg_space_heating_ranger_cal_SD_perc)^2) * N_GEG_value * GHG_kg_space_heating_ranger_cal_extra_uncer[1, 1]


        #Heat_extra_uncer <- bremean(~Heat, df.geg.new.valid, Dmats)
            XXX <- paste("target_extra_uncer", " <- ", "bremean(~", ghg_target_name, ", df.geg.new.valid, Dmats)", sep = "", collapse = "")
            XXX <- parse(text = XXX)
            eval(XXX)
            target_V <- target_extra_uncer[1, 1]/1000000000 * N_GEG_value
            target_SE <- target_extra_uncer[1, 2]/1000000000
            target_SE_perc <- target_extra_uncer[1, 2] / target_extra_uncer[1, 1]
            target_SE_tot <- sqrt((N_GEG_SE_perc)^2 + (target_SE_perc)^2) * N_GEG_value * target_extra_uncer[1, 1]
            target_SE_tot <- target_SE_tot/1000000000

            sink(paste(path, "//", test_name, "Stock_GHG_selected_model_cal", ".txt", sep = "")) 
            print("GHG-Emission")
            print(paste("The selected model for ", test_name, " is: ", sep = ""))
            print(selected_model)
            print("of this selected model the extrapolated result is of the GHG-Emission are: ")
            print(target_V)
            print("in Gt;")
            print("with an uncertainty of: ")
            print(target_SE_tot)
            print("in Gt.")
            sink() # returns output to the console
    }





# save Environment with all results for independent plotting etc.
    path <- getwd()
    save.image(file = paste(path, "//", test_name, "Training_and_Extrapolation_Environment_Finished", ".RData", sep = ""))
    
    #Finished
