options(shiny.reactlog=TRUE)
#access reactive only with X()
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zipcode)
data(zipcode)
library(xlsx)
source("distance_function.R")

scorecard <- xlsx::read.xlsx("Triage_score_card.xlsx",
                 sheetIndex = 1, header = TRUE)
zipcode_info = read.csv("zipcode_info.csv",
                        stringsAsFactors = FALSE, header = TRUE)
facility = read.csv("facility.csv",
                    stringsAsFactors = TRUE, header = TRUE)


model_rapid = readRDS("score_model.rds")
shinyServer(function(input, output) {
#put all fields in reactive  - remeber reactive rules access dataframe with () ***IMPORTANT
   
   Age_out = reactive({
             AgeBin(as.numeric(input$age), as.character(input$units))
     })
   
   AgeBin = function(age_in, units)(
    dplyr::case_when(((units == 'Days' & age_in < 31) |
        (units == 'Months' & age_in < 12)) ~ ("<1"), 
          
        (units == 'Years' & (age_in >=1 & age_in < 6)) ~  ("1-5"), 
            
        (units == 'Years' & (age_in >=6 & age_in < 11)) ~ ("6-10") ,
              
        (units == 'Years' & (age_in >=11 & age_in < 18)) ~ ("11-17"), 
                
        (units == 'Years' & (age_in >=18 & age_in < 35))  ~ ("18-34"), 
                  
        (units == 'Years' & (age_in >=35 & age_in < 50)) ~ ("35-49"), 
                    
        (units == 'Years' & (age_in >=50 & age_in < 65)) ~ ("50-64"), 
        TRUE  ~ ("65+"))
     
   )

  pre_sbp = reactive({
    sbp = as.numeric(input$sbp)
    dplyr::case_when(
               sbp < 100 ~ "<100",
               (sbp >=100 & sbp < 145) ~ "100 - 145",
               (sbp >=145 & sbp < 170) ~ "145 - 170",
                TRUE ~ ">170")
  })
  
  pre_pulse = reactive({
    pulse =as.numeric(input$pulse_rate)
    dplyr::case_when(
            pulse <= 65 ~ "<65",
            (pulse >65 & pulse <= 75) ~  "65 - 75",
            (pulse >75 & pulse <= 100) ~"75 - 100",
              TRUE ~ ">100")
     
  })
  
  pre_oxi = reactive({
    oxi=as.numeric(input$oximetry)
    dplyr::case_when(
                 (oxi < 92) ~ ("<92"),
                 (oxi >=92 & oxi < 95) ~ ("92 - 95"),
                 (oxi >=95 & oxi < 98) ~ ("95 - 98"),
                  TRUE ~ ">98")
    
  })
  
  var1=reactive({
     scorecard$Points[scorecard$Variable == 'Age' & scorecard$Bin == Age_out()]
  })
  
  var2=reactive({
    scorecard$Points[scorecard$Variable == 'gender' & scorecard$Bin == input$Gender]
    
  })
  
  var3=reactive({
    scorecard$Points[scorecard$Variable == 'vehicle accident' & scorecard$Bin == input$Vehicle_Accident]
  })
  
  var4=reactive({
     scorecard$Points[scorecard$Variable == 'GCS motor' & scorecard$Bin == input$gcs_motor]
  })
  
  var5= reactive({
    scorecard$Points[scorecard$Variable == 'GCS verbal' & scorecard$Bin == input$gcs_verbal]
  })
  
  var6=reactive({
    scorecard$Points[scorecard$Variable == 'Pulse oximetry' & scorecard$Bin == pre_oxi()]
  })
  
  var7=reactive({
    scorecard$Points[scorecard$Variable == 'sbp' & scorecard$Bin == pre_sbp()]
  })
    
  var8=reactive({
     scorecard$Points[scorecard$Variable == "pulse rate" & scorecard$Bin == pre_pulse()]
  })
  
  total=reactive({
    base = 334
    total=var1() + var2() + var3() + var4() + var5() + var6() + var7() + var8() + base
  })
  
  prob=reactive({
    new_data=data.frame('score' = total())
    probability = predict(model_rapid, new_data, type="response") * 100
  })
  
  prob_result=reactive({
    new_data=data.frame('score' = total())
    probability = predict(model_rapid, new_data, type="response") * 100
    ifelse(probability > 80, "Need Rapid Transport", "Not needed Rapid Transport")
  })
  
  
  outcome=reactive({
    ifelse (prob_result() == "Need Rapid Transport" & 
        round(distance() * 60 / 30, 0) > 60,  "Recommend HEMS Transport", 
           "Recommend Ground Ambulance")
  })
  
  injury_zip =reactive({
    injury_zip=zipcode_info$rural_ind[zipcode_info$zip_code == input$injury_zip][1]
    
  })
  
  facility_zip = reactive({
    facility_zip=facility$zip_code[facility$facility_name == input$facility]
  })
  
  distance=reactive({
    injury_zip = input$injury_zip
    facility_zip = facility_zip()
    from_lat = zipcode$latitude[zipcode$zip == injury_zip]
    from_long = zipcode$longitude[zipcode$zip == injury_zip]
    to_lat = zipcode$latitude[zipcode$zip == facility_zip]
    to_long = zipcode$longitude[zipcode$zip == facility_zip]
    dist=get_geo_distance(from_long, from_lat, to_long, to_lat)
    distance=round((dist + 1)*1.3, 2)
    
  })
  
  # #---------------------------------------------------------------------------#
  # # Find difference from normal in sbp for all ages                           #
  # #---------------------------------------------------------------------------#
  
  diff_sbp=reactive({
     prehospital_sbp_tr18_67 = as.numeric(input$sbp)
     years=as.numeric(input$age)
     units =input$units
      
      dplyr::case_when( 
                      prehospital_sbp_tr18_67 == 0 ~ 0,
                       (units == 'Days' & years < 31) |
                           (units == 'Months' & years < 12) & prehospital_sbp_tr18_67 < 72 ~ (prehospital_sbp_tr18_67 - 72),
                       (units == 'Days' & years < 31) |
                          (units == 'Months' & years < 12)  & prehospital_sbp_tr18_67 > 104 ~  (prehospital_sbp_tr18_67 - 104),
                       (years >=1 & years < 3) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 < 86) ~  (prehospital_sbp_tr18_67 - 86) ,
                       (years >=1 & years < 3) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 > 106) ~  (prehospital_sbp_tr18_67 - 106) ,
                       (years >=3 & years < 6)  & (units == 'Years') &
                         (prehospital_sbp_tr18_67 < 89) ~  (prehospital_sbp_tr18_67 - 89),
                       (years >=3 & years < 6) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 > 112) ~  (prehospital_sbp_tr18_67 - 112),
                       (years >=6 & years < 10) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 < 97) ~  (prehospital_sbp_tr18_67 - 97),
                       (years >=6 & years < 10) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 > 115) ~  (prehospital_sbp_tr18_67 - 115),
                       (years >=10 & years < 12) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 < 101) ~  (prehospital_sbp_tr18_67 - 101),
                       (years >=10 & years < 12) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 > 120) ~  (prehospital_sbp_tr18_67 - 120),
                       (years >=12& years < 16) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 < 110) ~  (prehospital_sbp_tr18_67 - 110),
                       (years >=12 & years < 16) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 > 131) ~  (prehospital_sbp_tr18_67 - 131),
                       (years >=16) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 < 90) ~  (prehospital_sbp_tr18_67 - 90),
                       (years >=16) & (units == 'Years') &
                         (prehospital_sbp_tr18_67 > 120) ~  (prehospital_sbp_tr18_67 - 120),
                       TRUE ~ 0
             )
  })

  # #---------------------------------------------------------------------------#
  # # Find difference from normal in respiratory for all ages                   #
  # #---------------------------------------------------------------------------#
  # 
  diff_resp=reactive({
    prehospital_respiratory_rate_tr18_70 = as.numeric(input$resp)
    years=as.numeric(input$age)
    units =input$units
    
    dplyr::case_when( 
      prehospital_respiratory_rate_tr18_70 == 0 ~ 0,
                       ((units == 'Days' & years < 31) |
                          (units == 'Months' & years < 12) & prehospital_respiratory_rate_tr18_70 < 30) ~ (prehospital_respiratory_rate_tr18_70 - 30),
                       ((units == 'Days' & years < 31) |
                          (units == 'Months' & years < 12) & prehospital_respiratory_rate_tr18_70 > 53) ~ (prehospital_respiratory_rate_tr18_70 - 53),
                       (years >=1 & years < 3) &
                         (prehospital_respiratory_rate_tr18_70 < 22) ~  (prehospital_respiratory_rate_tr18_70 - 22) ,
                       (years >=1 & years < 3) &
                         (prehospital_respiratory_rate_tr18_70 > 37) ~  (prehospital_respiratory_rate_tr18_70 - 37) ,
                       (years >=3 & years < 6) &
                         (prehospital_respiratory_rate_tr18_70 < 20) ~  (prehospital_respiratory_rate_tr18_70 - 20),
                       (years >=3 & years < 6) &
                         (prehospital_respiratory_rate_tr18_70 > 28) ~  (prehospital_respiratory_rate_tr18_70 - 28),
                       (years >=6 & years < 12) &
                         (prehospital_respiratory_rate_tr18_70 < 18) ~  (prehospital_respiratory_rate_tr18_70 - 18),
                       (years >=6 & years < 12) &
                         (prehospital_respiratory_rate_tr18_70 > 25) ~  (prehospital_respiratory_rate_tr18_70 - 25),
                       (years >=12 & years < 16) &
                         (prehospital_respiratory_rate_tr18_70 < 12) ~  (prehospital_respiratory_rate_tr18_70 - 12),
                       (years >=12 & years < 16) &
                         (prehospital_respiratory_rate_tr18_70 > 20) ~  (prehospital_respiratory_rate_tr18_70 - 20),
                       (years >=16) &
                         (prehospital_respiratory_rate_tr18_70 < 12) ~  (prehospital_respiratory_rate_tr18_70 - 12),
                       (years >=16) &
                         (prehospital_respiratory_rate_tr18_70 > 18) ~  (prehospital_respiratory_rate_tr18_70 - 18),
                       TRUE ~ 0
             )
  })

   
  # #---------------------------------------------------------------------------#
  # # Find the difference from normal of pulse                                  #
  # #---------------------------------------------------------------------------#
  
      diff_pulse=reactive({
        prehospital_pulse_rate_tr18_69 = as.numeric(input$pulse_rate)
        patient_age_tr1_12=as.numeric(input$age)
        patient_age_units_tr1_14 =input$units
        
        dplyr::case_when( 
          (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
        prehospital_pulse_rate_tr18_69 < 100        ~ (prehospital_pulse_rate_tr18_69 - 100),

      (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
        prehospital_pulse_rate_tr18_69 > 205         ~ (prehospital_pulse_rate_tr18_69 - 205),

      ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
         (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
        prehospital_pulse_rate_tr18_69 < 100        ~ (prehospital_pulse_rate_tr18_69 - 100),

      ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
         (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
        prehospital_pulse_rate_tr18_69 > 190      ~ (prehospital_pulse_rate_tr18_69 - 190),

      ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
         (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
        prehospital_pulse_rate_tr18_69 < 98         ~ (prehospital_pulse_rate_tr18_69 - 98),

      ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
         (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
        prehospital_pulse_rate_tr18_69 >140         ~ (prehospital_pulse_rate_tr18_69 - 140),

      (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
        prehospital_pulse_rate_tr18_69 < 80         ~ (prehospital_pulse_rate_tr18_69 - 80),

      (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
        prehospital_pulse_rate_tr18_69 > 120        ~ (prehospital_pulse_rate_tr18_69 - 120),

      (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &
        prehospital_pulse_rate_tr18_69 < 75          ~ (prehospital_pulse_rate_tr18_69 - 75),

      (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &
        prehospital_pulse_rate_tr18_69 > 118        ~ (prehospital_pulse_rate_tr18_69 - 118),

      (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &
        prehospital_pulse_rate_tr18_69 < 60           ~ (prehospital_pulse_rate_tr18_69 - 60),

      (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &
        prehospital_pulse_rate_tr18_69 > 100          ~ (prehospital_pulse_rate_tr18_69 - 100),

      (is.na(prehospital_pulse_rate_tr18_69)) ~ 0,
      TRUE ~ 0)
})
  
    output$Age_points = renderText({
      var1()
    })
    
    output$gender_points = renderText({
      var2()
    })
    
    output$accident_points = renderText({
       var3()
    })
    
    output$motor_points = renderText({
      var4()
    })
    
    output$verbal_points = renderText({
      var5()
    })
    
    output$oxi_points = renderText({
      var6()
    })
    
    output$sbp_points = renderText({
      var7()
    })
    
    output$pulse_points = renderText({
      var8()
    })
    
    
    output$Total_points = renderText({
      total()
      
    })
    
    output$rapid_points = renderText({
       prob_result()
   }) 
    
    output$rural_ind = renderText({
      injury_zip()
    }) 
    
    output$distance = renderText({
      round(distance(),0)
    }) 
    
    output$time = renderText({
      round(distance() * 60 / 30, 0)
    })
    
    output$diff_sbp = renderText({
      round(diff_sbp(), 0)
    })
    
    output$diff_resp = renderText({
      round(diff_resp(), 0)
    })
    
    output$diff_pulse = renderText({
      round(diff_pulse(), 0)
    })
    
    output$Outcome = renderText({
      outcome()
    })
      
   
}) #do not touch   