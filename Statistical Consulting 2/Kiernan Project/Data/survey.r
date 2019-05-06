#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('/Users/timvigers/Documents/GitHub/School/Statistical Consulting 2/Kiernan Project/Data/survey.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$back_pain_in_backpackers_timestamp)="Survey Timestamp"
label(data$agree)="Do you agree to terms above?"
label(data$intro_backpack_in_3_yrs)="Have you gone hiking with a pack weighing 10 pounds or more in the last 3 years?"
label(data$intro_age_18)="Are you older than 18?"
label(data$intro_age_89)="Are you younger than 89?"
label(data$have_went_day_hike_with_pack)="In the last 3 years have you went day hiking with a pack?"
label(data$how_many_days_day_hike)="Please enter the number of times per year you day hike with a pack"
label(data$day_pack_weight)="When you go day hiking, what is the average weight of your pack in pounds?"
label(data$how_many_hours_day_hike)="On average how long do you typically day hike with a pack?"
label(data$years_backpacking)="How many years have you been day hiking with a pack?"
label(data$have_went_bkpackin)="In the last 3 years, have you went backpacking?"
label(data$how_many_times_multi_day)="Please enter the number of backpacking trips you take each year"
label(data$backpack_days)="On average how long are your backpacking trips in days?"
label(data$weight_of_backpack)="When you go backpacking what is the average weight of your backpack in pounds?"
label(data$backpacking_hours)="On average how long do you typically backpack per day on backpacking trips?"
label(data$time_backpacking_years)="How many years have you been backpacking?"
label(data$back_pain_in_3_years)="Over the last 3 years have you experienced back pain? (back pain includes neck, mid back/shoulder blade area and low back)"
label(data$area_of_most_pain)="If you experience back pain, choose the area from the picture below where you have the most pain (choose one)."
label(data$back_pain)="When you experience back pain how would you rate the severity?"
label(data$medical_professionals___1)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=Medical Doctor)"
label(data$medical_professionals___2)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=Physician Assistant or Nurse Practitioner)"
label(data$medical_professionals___3)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=Chiropractor)"
label(data$medical_professionals___4)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=Physical Therapist)"
label(data$medical_professionals___5)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=Emergency Department Visit)"
label(data$medical_professionals___6)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=Other)"
label(data$medical_professionals___7)="Have you seen any of the following medical professionals for treatment of your back pain? (mark all that apply) (choice=None of the above)"
label(data$back_pain_currently)="Do you currently have back pain?"
label(data$job_strenuous)="In the last 3 years how physically strenuous is your job with regards to lifting and/or carrying loads?"
label(data$recreation_strenuous)="In the last 3 years how physically strenuous are the recreational activities you participate in with regards to lifting and/or carrying loads?"
label(data$pain_while_packing___1)="I experience back pain while hiking with : (choice=Day pack)"
label(data$pain_while_packing___2)="I experience back pain while hiking with : (choice=Backpack)"
label(data$pain_while_packing___3)="I experience back pain while hiking with : (choice=Both)"
label(data$pain_while_packing___4)="I experience back pain while hiking with : (choice=Neither)"
label(data$choose_from_picture)="If you experience back pain while day hiking with a pack choose the area from the picture below where you have the most pain. (choose only one)"
label(data$back_pain_location)="If you experience back pain while backpacking, please choose from the picture below where you have the most pain. (choose one)"
label(data$back_pain_frequency)="How frequently do you experience back pain while day hiking with a pack?"
label(data$freq_and_backpack)="How frequently do you experience back pain while backpacking?"
label(data$pn_after)="Do you experience back pain after"
label(data$worse_pain_with_pack___1)="Do any of the following options make your back pain worse?  (choice=Day Hiking with a pack)"
label(data$worse_pain_with_pack___2)="Do any of the following options make your back pain worse?  (choice=Backpacking)"
label(data$worse_pain_with_pack___3)="Do any of the following options make your back pain worse?  (choice=Both)"
label(data$worse_pain_with_pack___4)="Do any of the following options make your back pain worse?  (choice=Neither)"
label(data$pain_limit_distance___1)="Does your back pain limit the distance you can cover: (choice=Day hiking with a pack)"
label(data$pain_limit_distance___2)="Does your back pain limit the distance you can cover: (choice=Backpacking)"
label(data$pain_limit_distance___3)="Does your back pain limit the distance you can cover: (choice=Both)"
label(data$pain_limit_distance___4)="Does your back pain limit the distance you can cover: (choice=Neither)"
label(data$day_pack_features___1)="Please mark all the features your day pack has from the following choices (choice=Hip belt)"
label(data$day_pack_features___2)="Please mark all the features your day pack has from the following choices (choice=Load lifters- (straps connecting shoulder straps to top of pack))"
label(data$day_pack_features___3)="Please mark all the features your day pack has from the following choices (choice=Sternal strap)"
label(data$day_pack_features___4)="Please mark all the features your day pack has from the following choices (choice=Adjustable torso length)"
label(data$day_pack_features___5)="Please mark all the features your day pack has from the following choices (choice=External Frame- metal frame is outside and backpack appears strapped to it)"
label(data$day_pack_features___6)="Please mark all the features your day pack has from the following choices (choice=Internal Frame- frame is inside pack (no metal components outside pack))"
label(data$prof_fit_for_day_pack)="Were you professionally fitted for the pack you carry on day hikes?"
label(data$time_fit_day_pack)="If yes approximately how long ago were you fitted?"
label(data$professionally_fit_education___1)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They went through the feature of my pack)"
label(data$professionally_fit_education___2)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They measured my torso length)"
label(data$professionally_fit_education___3)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They loaded the pack prior to me putting it on)"
label(data$professionally_fit_education___4)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They checked the fit of my hip belt)"
label(data$professionally_fit_education___5)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They checked the fit of my shoulder straps)"
label(data$professionally_fit_education___6)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They checked the fit of my load lifters (small straps in the back that connect shoulder straps to top of pack))"
label(data$professionally_fit_education___7)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They demonstrated how to adjust my pack)"
label(data$professionally_fit_education___8)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=I dont remember what they covered)"
label(data$professionally_fit_education___9)="If you were professionally fitted to the pack you use on day hikes mark all that apply (choice=They did not cover any of this information)"
label(data$day_pack_fit_back_pain)="Did you get professionally fit for your day pack because you had back pain?"
label(data$decrease_fitting_day)="If yes, did your back pain decrease after your fitting?"
label(data$adjust_day_pack___1)="While day hiking with a pack what components do you adjust the most? (choice=Hip belt)"
label(data$adjust_day_pack___2)="While day hiking with a pack what components do you adjust the most? (choice=Shoulder straps)"
label(data$adjust_day_pack___3)="While day hiking with a pack what components do you adjust the most? (choice=Sternal strap)"
label(data$adjust_day_pack___4)="While day hiking with a pack what components do you adjust the most? (choice=Load lifters)"
label(data$adjust_day_pack___5)="While day hiking with a pack what components do you adjust the most? (choice=I dont make adjustments)"
label(data$same_pack)="Do you use the same pack for day hikes and backpacking?"
label(data$pack_features___1)="Please mark all the features your backpack has from the following choices (choice=Hip belt)"
label(data$pack_features___2)="Please mark all the features your backpack has from the following choices (choice=Load lifters- (straps connecting shoulder straps to top of pack))"
label(data$pack_features___3)="Please mark all the features your backpack has from the following choices (choice=Sternal strap-(strap along collar bone that can connect shoulder straps))"
label(data$pack_features___4)="Please mark all the features your backpack has from the following choices (choice=Adjustable torso length)"
label(data$pack_features___5)="Please mark all the features your backpack has from the following choices (choice=External Frame- (metal frame is outside and backpack appears strapped to it))"
label(data$pack_features___6)="Please mark all the features your backpack has from the following choices (choice=Internal Frame- (frame is inside pack (no metal components outside pack))"
label(data$professionally_fit)="Have you ever had someone professionally fit your backpack?"
label(data$how_long_ago_fit)="If yes, approximately long ago were you fitted?"
label(data$fit_features___1)="If you were professionally fitted to your backpack mark all that apply. (choice=They went through the features of my pack)"
label(data$fit_features___2)="If you were professionally fitted to your backpack mark all that apply. (choice=They measured my torso length)"
label(data$fit_features___3)="If you were professionally fitted to your backpack mark all that apply. (choice=They loaded the pack prior to me putting it on)"
label(data$fit_features___4)="If you were professionally fitted to your backpack mark all that apply. (choice=They checked the fit of my hip belt)"
label(data$fit_features___5)="If you were professionally fitted to your backpack mark all that apply. (choice=They checked the fit of my shoulder straps)"
label(data$fit_features___6)="If you were professionally fitted to your backpack mark all that apply. (choice=They checked the fit of my load lifters (small straps in the back that connect shoulder straps to top of pack))"
label(data$fit_features___7)="If you were professionally fitted to your backpack mark all that apply. (choice=They demonstrated how to adjust my pack)"
label(data$fit_features___8)="If you were professionally fitted to your backpack mark all that apply. (choice=I dont remember what they covered)"
label(data$fit_features___9)="If you were professionally fitted to your backpack mark all that apply. (choice=They did not cover any of this information)"
label(data$fit_for_pain)="Did you get professionally fitted for your backpack because you had back pain?"
label(data$decrease_after_fit)="If yes, did your back pain decrease after your fitting?"
label(data$feature_adjustment___1)="While hiking with a backpack what components do you adjust the most? (choice=Hip belt)"
label(data$feature_adjustment___2)="While hiking with a backpack what components do you adjust the most? (choice=Shoulder straps)"
label(data$feature_adjustment___3)="While hiking with a backpack what components do you adjust the most? (choice=Sternal strap)"
label(data$feature_adjustment___4)="While hiking with a backpack what components do you adjust the most? (choice=Load lifters)"
label(data$feature_adjustment___5)="While hiking with a backpack what components do you adjust the most? (choice=I dont make adjustments)"
label(data$pack_load_distribution)="When loading your backpack, where is the majority of the weight?  Refer to picture below"
label(data$load_day_pack)="When loading the pack you use for day hikes where is the majority of the weight? Refer to picture below."
label(data$conditioning)="Do you have a physical conditioning program that you specifically do to prepare for backpacking trips?"
label(data$fitness___1)="If yes, what activities do you do (mark all that apply) (choice=strengthening- (weight lifting))"
label(data$fitness___2)="If yes, what activities do you do (mark all that apply) (choice=cardio- (swimming, running, elliptical, biking etc.))"
label(data$fitness___3)="If yes, what activities do you do (mark all that apply) (choice=fitness classes- (yoga, pilates, HITT, etc.))"
label(data$fitness___4)="If yes, what activities do you do (mark all that apply) (choice=abdominal workout)"
label(data$fitness___5)="If yes, what activities do you do (mark all that apply) (choice=day hike)"
label(data$physical_conditioning)="Do you have a physical conditioning program that you do to prepare for day hiking with a pack?"
label(data$day_hike_activity___1)="If yes, what activities do you do? (mark all that apply) (choice=strengthening- (weight lifting))"
label(data$day_hike_activity___2)="If yes, what activities do you do? (mark all that apply) (choice=cardio- (swimming, running, elliptical, biking etc.))"
label(data$day_hike_activity___3)="If yes, what activities do you do? (mark all that apply) (choice=fitness classes- (yoga, pilates, HITT, etc.))"
label(data$day_hike_activity___4)="If yes, what activities do you do? (mark all that apply) (choice=abdominal workout)"
label(data$day_hike_activity___5)="If yes, what activities do you do? (mark all that apply) (choice=day hike)"
label(data$club)="Do you belong to a hiking or backpacking club?"
label(data$age)="Age"
label(data$gender)="What gender do you identify with?"
label(data$height)="Height (in inches)"
label(data$weight)="Weight (pounds)"
label(data$day_pk_raffle)="Would you like to provide your email address to be entered into a raffle to win a day pack? "
label(data$thank_you)="Thank you for your time and contribution to our research. If you have any additional comments, please leave them here. "
label(data$back_pain_in_backpackers_complete)="Complete?"
label(data$email_redirect_timestamp)="Survey Timestamp"
label(data$email_redirect_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$agree.factor = factor(data$agree,levels=c("1","0"))
data$intro_backpack_in_3_yrs.factor = factor(data$intro_backpack_in_3_yrs,levels=c("1","0"))
data$intro_age_18.factor = factor(data$intro_age_18,levels=c("1","0"))
data$intro_age_89.factor = factor(data$intro_age_89,levels=c("1","0"))
data$have_went_day_hike_with_pack.factor = factor(data$have_went_day_hike_with_pack,levels=c("1","0"))
data$how_many_hours_day_hike.factor = factor(data$how_many_hours_day_hike,levels=c("1","2","3","4","5","6"))
data$have_went_bkpackin.factor = factor(data$have_went_bkpackin,levels=c("1","0"))
data$backpacking_hours.factor = factor(data$backpacking_hours,levels=c("1","2","3","4","5","6","7"))
data$back_pain_in_3_years.factor = factor(data$back_pain_in_3_years,levels=c("1","0"))
data$area_of_most_pain.factor = factor(data$area_of_most_pain,levels=c("1","2","3"))
data$medical_professionals___1.factor = factor(data$medical_professionals___1,levels=c("0","1"))
data$medical_professionals___2.factor = factor(data$medical_professionals___2,levels=c("0","1"))
data$medical_professionals___3.factor = factor(data$medical_professionals___3,levels=c("0","1"))
data$medical_professionals___4.factor = factor(data$medical_professionals___4,levels=c("0","1"))
data$medical_professionals___5.factor = factor(data$medical_professionals___5,levels=c("0","1"))
data$medical_professionals___6.factor = factor(data$medical_professionals___6,levels=c("0","1"))
data$medical_professionals___7.factor = factor(data$medical_professionals___7,levels=c("0","1"))
data$back_pain_currently.factor = factor(data$back_pain_currently,levels=c("1","0"))
data$job_strenuous.factor = factor(data$job_strenuous,levels=c("1","2","3","4"))
data$recreation_strenuous.factor = factor(data$recreation_strenuous,levels=c("1","2","3","4"))
data$pain_while_packing___1.factor = factor(data$pain_while_packing___1,levels=c("0","1"))
data$pain_while_packing___2.factor = factor(data$pain_while_packing___2,levels=c("0","1"))
data$pain_while_packing___3.factor = factor(data$pain_while_packing___3,levels=c("0","1"))
data$pain_while_packing___4.factor = factor(data$pain_while_packing___4,levels=c("0","1"))
data$choose_from_picture.factor = factor(data$choose_from_picture,levels=c("1","2","3","4"))
data$back_pain_location.factor = factor(data$back_pain_location,levels=c("1","2","3","4"))
data$back_pain_frequency.factor = factor(data$back_pain_frequency,levels=c("1","2","3","4"))
data$freq_and_backpack.factor = factor(data$freq_and_backpack,levels=c("1","2","3","4"))
data$pn_after.factor = factor(data$pn_after,levels=c("1","2","3","4"))
data$worse_pain_with_pack___1.factor = factor(data$worse_pain_with_pack___1,levels=c("0","1"))
data$worse_pain_with_pack___2.factor = factor(data$worse_pain_with_pack___2,levels=c("0","1"))
data$worse_pain_with_pack___3.factor = factor(data$worse_pain_with_pack___3,levels=c("0","1"))
data$worse_pain_with_pack___4.factor = factor(data$worse_pain_with_pack___4,levels=c("0","1"))
data$pain_limit_distance___1.factor = factor(data$pain_limit_distance___1,levels=c("0","1"))
data$pain_limit_distance___2.factor = factor(data$pain_limit_distance___2,levels=c("0","1"))
data$pain_limit_distance___3.factor = factor(data$pain_limit_distance___3,levels=c("0","1"))
data$pain_limit_distance___4.factor = factor(data$pain_limit_distance___4,levels=c("0","1"))
data$day_pack_features___1.factor = factor(data$day_pack_features___1,levels=c("0","1"))
data$day_pack_features___2.factor = factor(data$day_pack_features___2,levels=c("0","1"))
data$day_pack_features___3.factor = factor(data$day_pack_features___3,levels=c("0","1"))
data$day_pack_features___4.factor = factor(data$day_pack_features___4,levels=c("0","1"))
data$day_pack_features___5.factor = factor(data$day_pack_features___5,levels=c("0","1"))
data$day_pack_features___6.factor = factor(data$day_pack_features___6,levels=c("0","1"))
data$prof_fit_for_day_pack.factor = factor(data$prof_fit_for_day_pack,levels=c("1","0"))
data$professionally_fit_education___1.factor = factor(data$professionally_fit_education___1,levels=c("0","1"))
data$professionally_fit_education___2.factor = factor(data$professionally_fit_education___2,levels=c("0","1"))
data$professionally_fit_education___3.factor = factor(data$professionally_fit_education___3,levels=c("0","1"))
data$professionally_fit_education___4.factor = factor(data$professionally_fit_education___4,levels=c("0","1"))
data$professionally_fit_education___5.factor = factor(data$professionally_fit_education___5,levels=c("0","1"))
data$professionally_fit_education___6.factor = factor(data$professionally_fit_education___6,levels=c("0","1"))
data$professionally_fit_education___7.factor = factor(data$professionally_fit_education___7,levels=c("0","1"))
data$professionally_fit_education___8.factor = factor(data$professionally_fit_education___8,levels=c("0","1"))
data$professionally_fit_education___9.factor = factor(data$professionally_fit_education___9,levels=c("0","1"))
data$day_pack_fit_back_pain.factor = factor(data$day_pack_fit_back_pain,levels=c("1","0"))
data$decrease_fitting_day.factor = factor(data$decrease_fitting_day,levels=c("1","0"))
data$adjust_day_pack___1.factor = factor(data$adjust_day_pack___1,levels=c("0","1"))
data$adjust_day_pack___2.factor = factor(data$adjust_day_pack___2,levels=c("0","1"))
data$adjust_day_pack___3.factor = factor(data$adjust_day_pack___3,levels=c("0","1"))
data$adjust_day_pack___4.factor = factor(data$adjust_day_pack___4,levels=c("0","1"))
data$adjust_day_pack___5.factor = factor(data$adjust_day_pack___5,levels=c("0","1"))
data$same_pack.factor = factor(data$same_pack,levels=c("1","0"))
data$pack_features___1.factor = factor(data$pack_features___1,levels=c("0","1"))
data$pack_features___2.factor = factor(data$pack_features___2,levels=c("0","1"))
data$pack_features___3.factor = factor(data$pack_features___3,levels=c("0","1"))
data$pack_features___4.factor = factor(data$pack_features___4,levels=c("0","1"))
data$pack_features___5.factor = factor(data$pack_features___5,levels=c("0","1"))
data$pack_features___6.factor = factor(data$pack_features___6,levels=c("0","1"))
data$professionally_fit.factor = factor(data$professionally_fit,levels=c("1","0"))
data$fit_features___1.factor = factor(data$fit_features___1,levels=c("0","1"))
data$fit_features___2.factor = factor(data$fit_features___2,levels=c("0","1"))
data$fit_features___3.factor = factor(data$fit_features___3,levels=c("0","1"))
data$fit_features___4.factor = factor(data$fit_features___4,levels=c("0","1"))
data$fit_features___5.factor = factor(data$fit_features___5,levels=c("0","1"))
data$fit_features___6.factor = factor(data$fit_features___6,levels=c("0","1"))
data$fit_features___7.factor = factor(data$fit_features___7,levels=c("0","1"))
data$fit_features___8.factor = factor(data$fit_features___8,levels=c("0","1"))
data$fit_features___9.factor = factor(data$fit_features___9,levels=c("0","1"))
data$fit_for_pain.factor = factor(data$fit_for_pain,levels=c("1","0"))
data$decrease_after_fit.factor = factor(data$decrease_after_fit,levels=c("1","0"))
data$feature_adjustment___1.factor = factor(data$feature_adjustment___1,levels=c("0","1"))
data$feature_adjustment___2.factor = factor(data$feature_adjustment___2,levels=c("0","1"))
data$feature_adjustment___3.factor = factor(data$feature_adjustment___3,levels=c("0","1"))
data$feature_adjustment___4.factor = factor(data$feature_adjustment___4,levels=c("0","1"))
data$feature_adjustment___5.factor = factor(data$feature_adjustment___5,levels=c("0","1"))
data$pack_load_distribution.factor = factor(data$pack_load_distribution,levels=c("1","2","3","4","5"))
data$load_day_pack.factor = factor(data$load_day_pack,levels=c("1","2","3","4","5"))
data$conditioning.factor = factor(data$conditioning,levels=c("1","0"))
data$fitness___1.factor = factor(data$fitness___1,levels=c("0","1"))
data$fitness___2.factor = factor(data$fitness___2,levels=c("0","1"))
data$fitness___3.factor = factor(data$fitness___3,levels=c("0","1"))
data$fitness___4.factor = factor(data$fitness___4,levels=c("0","1"))
data$fitness___5.factor = factor(data$fitness___5,levels=c("0","1"))
data$physical_conditioning.factor = factor(data$physical_conditioning,levels=c("1","0"))
data$day_hike_activity___1.factor = factor(data$day_hike_activity___1,levels=c("0","1"))
data$day_hike_activity___2.factor = factor(data$day_hike_activity___2,levels=c("0","1"))
data$day_hike_activity___3.factor = factor(data$day_hike_activity___3,levels=c("0","1"))
data$day_hike_activity___4.factor = factor(data$day_hike_activity___4,levels=c("0","1"))
data$day_hike_activity___5.factor = factor(data$day_hike_activity___5,levels=c("0","1"))
data$club.factor = factor(data$club,levels=c("1","0"))
data$gender.factor = factor(data$gender,levels=c("1","2","3"))
data$day_pk_raffle.factor = factor(data$day_pk_raffle,levels=c("1","0"))
data$back_pain_in_backpackers_complete.factor = factor(data$back_pain_in_backpackers_complete,levels=c("0","1","2"))
data$email_redirect_complete.factor = factor(data$email_redirect_complete,levels=c("0","1","2"))

levels(data$agree.factor)=c("Yes","No")
levels(data$intro_backpack_in_3_yrs.factor)=c("Yes","No")
levels(data$intro_age_18.factor)=c("Yes","No")
levels(data$intro_age_89.factor)=c("Yes","No")
levels(data$have_went_day_hike_with_pack.factor)=c("Yes","No")
levels(data$how_many_hours_day_hike.factor)=c("1-2 hours","3-4 hours","5-6 hours","7-8 hours","9-10 hours","11-12 hours")
levels(data$have_went_bkpackin.factor)=c("Yes","No")
levels(data$backpacking_hours.factor)=c("1-2 hours","3-4 hours","5-6 hours","7-8 hours","9-10 hours","11-12 hours","Not applicable")
levels(data$back_pain_in_3_years.factor)=c("Yes","No")
levels(data$area_of_most_pain.factor)=c("Neck- 1","Mid back/shoulder blade- 2","Low back- 3")
levels(data$medical_professionals___1.factor)=c("Unchecked","Checked")
levels(data$medical_professionals___2.factor)=c("Unchecked","Checked")
levels(data$medical_professionals___3.factor)=c("Unchecked","Checked")
levels(data$medical_professionals___4.factor)=c("Unchecked","Checked")
levels(data$medical_professionals___5.factor)=c("Unchecked","Checked")
levels(data$medical_professionals___6.factor)=c("Unchecked","Checked")
levels(data$medical_professionals___7.factor)=c("Unchecked","Checked")
levels(data$back_pain_currently.factor)=c("Yes","No")
levels(data$job_strenuous.factor)=c("Low","Medium","High","Not Applicable")
levels(data$recreation_strenuous.factor)=c("Low","Medium","High","Not Applicable")
levels(data$pain_while_packing___1.factor)=c("Unchecked","Checked")
levels(data$pain_while_packing___2.factor)=c("Unchecked","Checked")
levels(data$pain_while_packing___3.factor)=c("Unchecked","Checked")
levels(data$pain_while_packing___4.factor)=c("Unchecked","Checked")
levels(data$choose_from_picture.factor)=c("Neck- 1","Mid back/shoulder blade-2","Low back-3","I do not experience back pain while day hiking with a pack")
levels(data$back_pain_location.factor)=c("Neck-1","Mid back/ shoulder blade- 2","Low back-3","I do not experience back pain while backpacking")
levels(data$back_pain_frequency.factor)=c("Never","Rarely","Frequently","Always")
levels(data$freq_and_backpack.factor)=c("Never","Rarely","Frequently","Always")
levels(data$pn_after.factor)=c("Day hiking with a pack","Backpacking","Both","Neither")
levels(data$worse_pain_with_pack___1.factor)=c("Unchecked","Checked")
levels(data$worse_pain_with_pack___2.factor)=c("Unchecked","Checked")
levels(data$worse_pain_with_pack___3.factor)=c("Unchecked","Checked")
levels(data$worse_pain_with_pack___4.factor)=c("Unchecked","Checked")
levels(data$pain_limit_distance___1.factor)=c("Unchecked","Checked")
levels(data$pain_limit_distance___2.factor)=c("Unchecked","Checked")
levels(data$pain_limit_distance___3.factor)=c("Unchecked","Checked")
levels(data$pain_limit_distance___4.factor)=c("Unchecked","Checked")
levels(data$day_pack_features___1.factor)=c("Unchecked","Checked")
levels(data$day_pack_features___2.factor)=c("Unchecked","Checked")
levels(data$day_pack_features___3.factor)=c("Unchecked","Checked")
levels(data$day_pack_features___4.factor)=c("Unchecked","Checked")
levels(data$day_pack_features___5.factor)=c("Unchecked","Checked")
levels(data$day_pack_features___6.factor)=c("Unchecked","Checked")
levels(data$prof_fit_for_day_pack.factor)=c("Yes","No")
levels(data$professionally_fit_education___1.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___2.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___3.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___4.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___5.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___6.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___7.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___8.factor)=c("Unchecked","Checked")
levels(data$professionally_fit_education___9.factor)=c("Unchecked","Checked")
levels(data$day_pack_fit_back_pain.factor)=c("Yes","No")
levels(data$decrease_fitting_day.factor)=c("Yes","No")
levels(data$adjust_day_pack___1.factor)=c("Unchecked","Checked")
levels(data$adjust_day_pack___2.factor)=c("Unchecked","Checked")
levels(data$adjust_day_pack___3.factor)=c("Unchecked","Checked")
levels(data$adjust_day_pack___4.factor)=c("Unchecked","Checked")
levels(data$adjust_day_pack___5.factor)=c("Unchecked","Checked")
levels(data$same_pack.factor)=c("Yes","No")
levels(data$pack_features___1.factor)=c("Unchecked","Checked")
levels(data$pack_features___2.factor)=c("Unchecked","Checked")
levels(data$pack_features___3.factor)=c("Unchecked","Checked")
levels(data$pack_features___4.factor)=c("Unchecked","Checked")
levels(data$pack_features___5.factor)=c("Unchecked","Checked")
levels(data$pack_features___6.factor)=c("Unchecked","Checked")
levels(data$professionally_fit.factor)=c("Yes","No")
levels(data$fit_features___1.factor)=c("Unchecked","Checked")
levels(data$fit_features___2.factor)=c("Unchecked","Checked")
levels(data$fit_features___3.factor)=c("Unchecked","Checked")
levels(data$fit_features___4.factor)=c("Unchecked","Checked")
levels(data$fit_features___5.factor)=c("Unchecked","Checked")
levels(data$fit_features___6.factor)=c("Unchecked","Checked")
levels(data$fit_features___7.factor)=c("Unchecked","Checked")
levels(data$fit_features___8.factor)=c("Unchecked","Checked")
levels(data$fit_features___9.factor)=c("Unchecked","Checked")
levels(data$fit_for_pain.factor)=c("Yes","No")
levels(data$decrease_after_fit.factor)=c("Yes","No")
levels(data$feature_adjustment___1.factor)=c("Unchecked","Checked")
levels(data$feature_adjustment___2.factor)=c("Unchecked","Checked")
levels(data$feature_adjustment___3.factor)=c("Unchecked","Checked")
levels(data$feature_adjustment___4.factor)=c("Unchecked","Checked")
levels(data$feature_adjustment___5.factor)=c("Unchecked","Checked")
levels(data$pack_load_distribution.factor)=c("1","2","3","4","5")
levels(data$load_day_pack.factor)=c("1","2","3","4","5")
levels(data$conditioning.factor)=c("Yes","No")
levels(data$fitness___1.factor)=c("Unchecked","Checked")
levels(data$fitness___2.factor)=c("Unchecked","Checked")
levels(data$fitness___3.factor)=c("Unchecked","Checked")
levels(data$fitness___4.factor)=c("Unchecked","Checked")
levels(data$fitness___5.factor)=c("Unchecked","Checked")
levels(data$physical_conditioning.factor)=c("Yes","No")
levels(data$day_hike_activity___1.factor)=c("Unchecked","Checked")
levels(data$day_hike_activity___2.factor)=c("Unchecked","Checked")
levels(data$day_hike_activity___3.factor)=c("Unchecked","Checked")
levels(data$day_hike_activity___4.factor)=c("Unchecked","Checked")
levels(data$day_hike_activity___5.factor)=c("Unchecked","Checked")
levels(data$club.factor)=c("Yes","No")
levels(data$gender.factor)=c("Male","Female","Non-binary")
levels(data$day_pk_raffle.factor)=c("Yes","No")
levels(data$back_pain_in_backpackers_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$email_redirect_complete.factor)=c("Incomplete","Unverified","Complete")
