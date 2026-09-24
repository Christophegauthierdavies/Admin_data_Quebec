#Mathématique: Technico-sciences - 4e secondaire
df_appr <- df_appr %>%
  mutate (Math_4e_ts = case_when(
    CD_COURS_SOM %in% c("064426", "564426") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Culture, société et technique - 4e secondaire
df_appr <- df_appr %>%
  mutate (Math_4e_cst = case_when(
    CD_COURS_SOM %in% c("063414", "563414") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Sciences naturelles - 4e secondaire
df_appr <- df_appr %>%
  mutate (Math_4e_sn = case_when(
    CD_COURS_SOM %in% c("065426", "565426") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Technico-sciences - 4e secondaire - anglais
df_appr <- df_appr %>%
  mutate (Math_4e_ts_ang = case_when(
    CD_COURS_SOM %in% c("064426") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Technico-sciences - 4e secondaire - français
df_appr <- df_appr %>%
  mutate (Math_4e_ts_fr = case_when(
    CD_COURS_SOM %in% c("564426") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Culture, société et technique - 4e secondaire - anglais
df_appr <- df_appr %>%
  mutate (Math_4e_cst_ang = case_when(
    CD_COURS_SOM %in% c("063414") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Culture, société et technique - 4e secondaire - français
df_appr <- df_appr %>%
  mutate (Math_4e_cst_fr = case_when(
    CD_COURS_SOM %in% c("563414") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Sciences naturelles - 4e secondaire - anglais
df_appr <- df_appr %>%
  mutate (Math_4e_sn_ang = case_when(
    CD_COURS_SOM %in% c("065426") ~ NOTE_MINST_BRUT
  ))
#Mathématique: Sciences naturelles - 4e secondaire - français
df_appr <- df_appr %>%
  mutate (Math_4e_sn_fr = case_when(
    CD_COURS_SOM %in% c("565426") ~ NOTE_MINST_BRUT
  ))
#Science et technologie - 4e secondaire
df_appr <- df_appr %>%
  mutate (Sc_4e_ST = case_when(
    CD_COURS_SOM %in% c("055444", "555444") ~ NOTE_MINST_BRUT
  ))
#Applications technologiques et scientifiques - 4e secondaire
df_appr <- df_appr %>%
  mutate (Sc_4e_ATS = case_when(
    CD_COURS_SOM %in% c("057416", "557416") ~ NOTE_MINST_BRUT
  ))
#Science et technologie - 4e secondaire - anglais
df_appr <- df_appr %>%
  mutate (Sc_4e_ST_eng = case_when(
    CD_COURS_SOM %in% c("055444") ~ NOTE_MINST_BRUT
  ))
#Science et technologie - 4e secondaire - français
df_appr <- df_appr %>%
  mutate (Sc_4e_ST_fr = case_when(
    CD_COURS_SOM %in% c("555444") ~ NOTE_MINST_BRUT
  ))
#Applications technologiques et scientifiques - 4e secondaire - anglais
df_appr <- df_appr %>%
  mutate (Sc_4e_ATS_eng = case_when(
    CD_COURS_SOM %in% c("057416") ~ NOTE_MINST_BRUT
  ))
#Applications technologiques et scientifiques - 4e secondaire - français
df_appr <- df_appr %>%
  mutate (Sc_4e_ATS_fr = case_when(
    CD_COURS_SOM %in% c("557416") ~ NOTE_MINST_BRUT
  ))
#Français, langue d'enseignement - 5e secondaire
df_appr <- df_appr %>%
  mutate (Fr_5e = case_when(
    CD_COURS_SOM %in% c("132506") ~ NOTE_MINST_BRUT
  ))
#Français, langue seconde, programme de base - 5e secondaire
df_appr <- df_appr %>%
  mutate (Fr_5e_ls_base = case_when(
    CD_COURS_SOM %in% c("634504") ~ NOTE_MINST_BRUT
  ))
#Français, langue seconde, programme enrichi - 5e secondaire
df_appr <- df_appr %>%
  mutate (Fr_5e_ls_enr = case_when(
    CD_COURS_SOM %in% c("635506") ~ NOTE_MINST_BRUT
  ))
#Français, langue seconde (base + enrichi) - 5e secondaire
df_appr <- df_appr %>%
  mutate (Fr_ls = case_when(
    CD_COURS_SOM %in% c("634504", "635506") ~ NOTE_MINST_BRUT
  ))
#English Language Arts - 5e secondaire
df_appr <- df_appr %>%
  mutate (Ang_5e = case_when(
    CD_COURS_SOM %in% c("612536") ~ NOTE_MINST_BRUT
  ))
#Anglais, langue seconde, programme de base - 5e secondaire
df_appr <- df_appr %>%
  mutate (Ang_5e_ls_base = case_when(
    CD_COURS_SOM %in% c("134504") ~ NOTE_MINST_BRUT
  ))
#Anglais, langue seconde, programme enrichi - 5e secondaire
df_appr <- df_appr %>%
  mutate (Ang_5e_ls_enr = case_when(
    CD_COURS_SOM %in% c("136506") ~ NOTE_MINST_BRUT
  ))
#Anglais, langue seconde (base + enrichi) - 5e secondaire
df_appr <- df_appr %>%
  mutate (Ang_ls = case_when(
    CD_COURS_SOM %in% c("134504", "136506") ~ NOTE_MINST_BRUT
  ))
#Histoire - 4e secondaire
df_appr <- df_appr %>%
  mutate (hist_4e = case_when(
    CD_COURS %in% c("085404", "585404") ~ NOTE_MINST_BRUT
  ))
#Histoire - 4e secondaire - anglais
df_appr <- df_appr %>%
  mutate (hist_4e_eng = case_when(
    CD_COURS %in% c("085404") ~ NOTE_MINST_BRUT
  ))
#Histoire - 4e secondaire - français
df_appr <- df_appr %>%
  mutate (hist_4e_fr = case_when(
    CD_COURS %in% c("585404") ~ NOTE_MINST_BRUT
  ))
