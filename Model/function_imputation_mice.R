library(dplyr)
library(tidyr)
library(rlist)
sep_gcs = function(df, gcs_eye, gcs_motor, gcs_verbal){
   df = df %>%
      separate(gcs_eye, 
           into = c("gcs_eye_imp"), sep = "\\s+", 
           remove = FALSE, convert = FALSE, extra = "drop") %>%
      separate(gcs_verbal, 
           into = c("gcs_verbal_imp"), sep = "\\s+", 
           remove = FALSE, convert = FALSE, extra = "drop") %>%
      separate(gcs_motor, 
           into = c("gcs_motor_imp"), sep = "\\s+", 
           remove = FALSE, convert = FALSE, extra = "drop") 

df$gcs_eye_imp[is.na(df$gcs_eye_imp) | df$gcs_eye_imp == 'Not'] = '1'
df$gcs_verbal_imp[is.na(df$gcs_verbal_imp) | df$gcs_verbal_imp == 'Not'] = '1'
df$gcs_motor_imp[is.na(df$gcs_motor_imp) | df$gcs_motor_imp == 'Not'] = '1'

df= df %>%
  mutate(gcs_eye_imp = as.numeric(gcs_eye_imp),
         gcs_verbal_imp = as.numeric(gcs_verbal_imp),
         gcs_motor_imp = as.numeric(gcs_motor_imp)) %>%
  mutate(gcs_total_mice_imp = gcs_eye_imp +
                              gcs_motor_imp +
                              gcs_verbal_imp)
return(df)
}

