data WORK.PD_DATA    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile '\\Mac\Home\Desktop\PD.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2;
   informat Participant best32. ;
   informat Group $3. ;
   informat Gender $6. ;
   informat Age best32. ;
   informat YearsDx best32. ;
   informat HYstage0 best32. ;
   informat HYstage4 best32. ;
   informat HYstage10 best32. ;
   informat HYstage16 best32. ;
   informat FiveM_Wk0 best32. ;
   informat FiveM_Wk4 best32. ;
   informat FiveM_Wk10 best32. ;
   informat FiveM_Wk16 best32. ;
   informat FiveM_Tm0 best32. ;
   informat FiveM_Tm4 best32. ;
   informat FiveM_Tm10 best32. ;
   informat FiveM_Tm16 best32. ;
   informat TUG0 best32. ;
   informat TUG4 best32. ;
   informat TUG10 best32. ;
   informat TUG16 best32. ;
   informat UPDRS0 best32. ;
   informat UPDRS4 best32. ;
   informat UPDRS10 best32. ;
   informat UPDRS16 best32. ;
   informat SixMn_Wk0 best32. ;
   informat SixMn_Wk4 best32. ;
   informat SixMn_Wk10 best32. ;
   informat SixMn_Wk16 best32. ;
   informat LEDD0 best32. ;
   informat LEDD4 best32. ;
   informat LEDD10 best32. ;
   informat LEDD16 best32. ;
   informat updrsbaseto4mon best32. ;
   informat updrsbaseto10mon best32. ;
   format Participant best12. ;
   format Group $3. ;
   format Gender $6. ;
   format Age best12. ;
   format YearsDx best12. ;
   format HYstage0 best12. ;
   format HYstage4 best12. ;
   format HYstage10 best12. ;
   format HYstage16 best12. ;
   format FiveM_Wk0 best12. ;
   format FiveM_Wk4 best12. ;
   format FiveM_Wk10 best12. ;
   format FiveM_Wk16 best12. ;
   format FiveM_Tm0 best12. ;
   format FiveM_Tm4 best12. ;
   format FiveM_Tm10 best12. ;
   format FiveM_Tm16 best12. ;
   format TUG0 best12. ;
   format TUG4 best12. ;
   format TUG10 best12. ;
   format TUG16 best12. ;
   format UPDRS0 best12. ;
   format UPDRS4 best12. ;
   format UPDRS10 best12. ;
   format UPDRS16 best12. ;
   format SixMn_Wk0 best12. ;
   format SixMn_Wk4 best12. ;
   format SixMn_Wk10 best12. ;
   format SixMn_Wk16 best12. ;
   format LEDD0 best12. ;
   format LEDD4 best12. ;
   format LEDD10 best12. ;
   format LEDD16 best12. ;
   format updrsbaseto4mon best12. ;
   format updrsbaseto10mon best12. ;
input
            Participant
            Group  $
            Gender  $
            Age
            YearsDx
            HYstage0
            HYstage4
            HYstage10
            HYstage16
            FiveM_Wk0
            FiveM_Wk4
            FiveM_Wk10
            FiveM_Wk16
            FiveM_Tm0
            FiveM_Tm4
            FiveM_Tm10
            FiveM_Tm16
            TUG0
            TUG4
            TUG10
            TUG16
            UPDRS0
            UPDRS4
            UPDRS10
            UPDRS16
            SixMn_Wk0
            SixMn_Wk4
            SixMn_Wk10
            SixMn_Wk16
            LEDD0
            LEDD4
            LEDD10
            LEDD16
            updrsbaseto4mon
            updrsbaseto10mon
;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;


data pd_data;
set pd_data;
if group = "CON" then con = 1; else con = 0;
IF group = "AE" then ae = 1; else ae = 0;
IF group = "FBF" then fbf = 1; else fbf = 0;
RUN;

proc reg data = pd_data;
model updrsbaseto4mon = con fbf;
run;

proc reg data = pd_data;
model updrsbaseto10mon = con fbf;
run;
