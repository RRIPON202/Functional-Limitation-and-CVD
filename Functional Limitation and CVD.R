# 1. LIBRARIES ----
library(haven)
library(dplyr)
library(survey)
library(DiagrammeR)

# 2. LOAD/MERGE ----
fnq <- read_xpt("~/Downloads/FNQ_L.xpt")
mcq <- read_xpt("~/Downloads/MCQ_L.xpt")
pbc <- read_xpt("~/Downloads/PBCD_L.xpt")
demo <- read_xpt("~/Downloads/DEMO_L.xpt")
bmx <- read_xpt("~/Downloads/BMX_L (1).xpt")
smq <- read_xpt("~/Downloads/SMQ_L.xpt")
alq <- read_xpt("~/Downloads/ALQ_L.xpt")
paq <- read_xpt("~/Downloads/PAQ_L (1).xpt")
diet <- read_xpt("~/Downloads/DSQTOT_L.xpt")
bp <- read_xpt("~/Downloads/BPQ_L.xpt")

nhanes <- fnq %>%
  inner_join(mcq, by = "SEQN") %>%
  inner_join(pbc, by = "SEQN") %>%
  inner_join(demo, by = "SEQN") %>%
  inner_join(bmx, by = "SEQN") %>%
  inner_join(smq, by = "SEQN") %>%
  inner_join(alq, by = "SEQN") %>%
  inner_join(paq, by = "SEQN") %>%
  inner_join(diet, by = "SEQN") %>%
  inner_join(bp, by = "SEQN")
n_merge <- nrow(nhanes)
nhanes <- nhanes %>% filter(RIDAGEYR >= 18, !is.na(WTMEC2YR), !is.na(SDMVPSU), !is.na(SDMVSTRA))
n_age_survey <- nrow(nhanes)

# 3. VARIABLE CREATION ----
nhanes <- nhanes %>%
  mutate(
    cognition_bin = case_when(
      FNQ460 == 1 ~ "no_difficulty",
      FNQ460 %in% 2:4 ~ "any_difficulty",
      TRUE ~ NA_character_
    ),
    mobility_bin  = case_when(
      FNQ440 == 1 ~ "no_difficulty",
      FNQ440 %in% 2:4 ~ "any_difficulty",
      TRUE ~ NA_character_
    ),
    seeing_bin    = case_when(
      FNQ410 == 1 ~ "no_difficulty",
      FNQ410 %in% 2:4 ~ "any_difficulty",
      TRUE ~ NA_character_
    ),
    cognition_bin = factor(cognition_bin, levels = c("no_difficulty", "any_difficulty")),
    mobility_bin  = factor(mobility_bin, levels = c("no_difficulty", "any_difficulty")),
    seeing_bin    = factor(seeing_bin, levels = c("no_difficulty", "any_difficulty")),
    CVD = ifelse(
      MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | MCQ160E == 1 | MCQ160F == 1, 1,
      ifelse(
        MCQ160B %in% c(2,7,9) & MCQ160C %in% c(2,7,9) & MCQ160D %in% c(2,7,9) &
          MCQ160E %in% c(2,7,9) & MCQ160F %in% c(2,7,9), 0, NA
      )
    ),
    
    # ----------- NEW VARIABLES ADDED HERE -----------
    alcohol = case_when(
      ALQ111 == 1 ~ "Yes",
      ALQ111 == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    smoke_status = case_when(
      SMQ020 == 1 & SMQ040 %in% c(1,2) ~ "Current",
      SMQ020 == 1 & SMQ040 == 3        ~ "Former",
      SMQ020 == 2                      ~ "Never",
      TRUE ~ NA_character_
    ),
    modact = ifelse(PAD790U == "W", as.numeric(PAD790Q), NA_real_),
    sedentary = as.numeric(PAD680),
    
    hypertension = case_when(
      BPQ020 == 1 ~ "Yes",
      BPQ020 == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    married = case_when(
      DMDMARTZ == 1 ~ "Married/Living with partner",
      DMDMARTZ == 2 ~ "Widowed/Divorced/Separated",
      DMDMARTZ == 3 ~ "Never married",
      TRUE ~ NA_character_
    )
    # -----------------------------------------------
  )

lead_q90      <- quantile(nhanes$LBXBPB, 0.9, na.rm=TRUE)
cadmium_q90   <- quantile(nhanes$LBXBCD, 0.9, na.rm=TRUE)
mercury_q90   <- quantile(nhanes$LBXTHG, 0.9, na.rm=TRUE)
manganese_q90 <- quantile(nhanes$LBXBMN, 0.9, na.rm=TRUE)
selenium_q90  <- quantile(nhanes$LBXBSE, 0.9, na.rm=TRUE)
nhanes <- nhanes %>%
  mutate(
    highlead      = factor(ifelse(is.na(LBXBPB), NA, ifelse(LBXBPB >= lead_q90, "high", "low")), levels = c("low", "high")),
    highcadmium   = factor(ifelse(is.na(LBXBCD), NA, ifelse(LBXBCD >= cadmium_q90, "high", "low")), levels = c("low", "high")),
    highmercury   = factor(ifelse(is.na(LBXTHG), NA, ifelse(LBXTHG >= mercury_q90, "high", "low")), levels = c("low", "high")),
    highmanganese = factor(ifelse(is.na(LBXBMN), NA, ifelse(LBXBMN >= manganese_q90, "high", "low")), levels = c("low", "high")),
    highselenium  = factor(ifelse(is.na(LBXBSE), NA, ifelse(LBXBSE >= selenium_q90, "high", "low")), levels = c("low", "high"))
  )

race_labels <- c("Mex-American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Multi-Race")
levels_vector <- sort(unique(nhanes$RIDRETH3))
race_labels <- race_labels[match(levels_vector, c(1,2,3,4,6,7))]
nhanes <- nhanes %>%
  mutate(
    age      = RIDAGEYR,
    sex      = factor(RIAGENDR, levels = c(1,2), labels = c("Male","Female")),
    race_eth = factor(RIDRETH3, levels = levels_vector, labels = race_labels),
    modact      = ifelse(PAD790U == "W", as.numeric(PAD790Q), NA_real_),
    sedentary   = as.numeric(PAD680),
    educ        = factor(DMDEDUC2, levels = c(1,2,3,4,5), labels = c("<9th", "9-11th", "HS/GED", "SomeCol/AA", "College+"))
  )


# 4. COMPLETE CASES and SURVEY DESIGN ----
all_vars <- c(
  "CVD", "cognition_bin", "mobility_bin", "seeing_bin",
  "highlead", "highcadmium", "highmercury", "highmanganese", "highselenium",
  "age", "sex", "race_eth",
  "alcohol", "smoke_status", "modact", "sedentary", "married", "hypertension","modact", "sedentary", "educ"
)

nhanes_cc <- nhanes %>% filter(complete.cases(select(., all_of(all_vars))))
n_cc <- nrow(nhanes_cc)
n_cases <- sum(nhanes_cc$CVD == 1, na.rm=TRUE)
cat("Merged:", n_merge, "\nFiltered adult/surveyvars:", n_age_survey, "\nComplete cases:", n_cc, "\nCVD=1:", n_cases, "\n")
grViz(paste0("
digraph nhanes_flow {
  graph [layout = dot, rankdir = TB]
  node [shape = box, style=filled, fillcolor=LightBlue, fontname=Arial]
  nmerge [label='Merged\\nN = ", n_merge, "']
  nage [label='18+ & survey vars\\nN = ", n_age_survey, "']
  ncc [label='Complete case\\nN = ", n_cc, "']
  ncase [label='CVD cases\\nN = ", n_cases, "']
  nmerge -> nage -> ncc -> ncase
}"))
survey_design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, data=nhanes_cc, nest=TRUE)

# 5. TABLE 1: DESCRIPTIVE ----
categorical_vars <- c(
  "CVD", "cognition_bin", "mobility_bin", "seeing_bin",
  "highlead", "highcadmium", "highmercury", "highmanganese", "highselenium",
  "sex", "race_eth", "alcohol", "smoke_status", "married", "hypertension", "educ"
)

desc_cat <- data.frame(variable=character(), level=character(), n=integer(),
                       pct=double(), ci_low=double(), ci_high=double(), stringsAsFactors=F)
for (var in categorical_vars) {
  lvls <- levels(as.factor(nhanes_cc[[var]]))
  tab <- table(nhanes_cc[[var]])
  for (lv in lvls) {
    n_val <- ifelse(lv %in% names(tab), tab[lv], 0)
    prop_res <- svymean(~I(get(var)==lv), survey_design)
    prop <- as.numeric(prop_res)
    prop_ci <- as.numeric(confint(prop_res)[1,])
    desc_cat <- rbind(desc_cat, data.frame(variable=var, level=lv, n=n_val, 
                                           pct=round(100*prop,2), 
                                           ci_low=round(100*prop_ci[1],2), ci_high=round(100*prop_ci[2],2)))
  }
}
cat("\n===== Table 1 (n, weighted %, 95% CI): =====\n")
print(desc_cat)

# Define your continuous variables here:
cont_vars <- c("age","BMXBMI","DSQTKCAL","LBXBPB","LBXBCD","LBXTHG","LBXBMN","LBXBSE","modact", "sedentary")  # Example; use your actual names

cat("\n=== DESCRIPTIVE ANALYSIS FOR CONTINUOUS VARIABLES (MEAN, 95% CI, MEDIAN, IQR, Weighted) ===\n")

cont_desc <- data.frame(variable=character(), mean=double(), ci_low=double(), ci_high=double(),
                        median=double(), q1=double(), q3=double(), stringsAsFactors = FALSE)
for (v in cont_vars) {
  # Mean and 95% CI
  # If all NA, skip
  if (all(is.na(nhanes_cc[[v]]))) next
  # Weighted mean and 95% CI
  mn <- tryCatch(as.numeric(svymean(as.formula(paste0("~",v)), survey_design, na.rm=TRUE)[1]), error=function(e) NA)
  ci <- tryCatch(as.numeric(confint(svymean(as.formula(paste0("~",v)), survey_design, na.rm=TRUE))[1,]), error=function(e) c(NA,NA))
  # Weighted median and IQR
  qu <- tryCatch(as.numeric(svyquantile(as.formula(paste0("~",v)), survey_design, c(0.25,0.5,0.75), na.rm=TRUE)), error=function(e) c(NA,NA,NA))
  cat(sprintf("%-15s: Mean = %.2f [%.2f, %.2f], Median = %.2f, IQR = %.2f – %.2f\n",
              v, mn, ci[1], ci[2], qu[2], qu[1], qu[3]))
  cont_desc <- rbind(cont_desc, data.frame(variable=v, mean=mn, ci_low=ci[1], ci_high=ci[2],
                                           median=qu[2], q1=qu[1], q3=qu[3]))
}
# Print as a table
cat("\nSummary Table (continuous):\n")
print(cont_desc)

# Optionally, save as CSV for publication
# write.csv(cont_desc, "continuous_desc_table.csv", row.names=FALSE)


# 6. REGRESSION (UNADJUSTED, ADJUSTED, INTERACTION) ----
domains <- c("cognition_bin", "mobility_bin", "seeing_bin")
metals  <- c("highlead","highcadmium","highmercury","highmanganese","highselenium")
adjusters <- c("age","sex","race_eth","married","hypertension")
#"age","sex","race_eth","alcohol","married", "smoke_status","modact","sedentary","hypertension"
model_results <- data.frame(
  analysis=character(),   # unadj/adj/interact
  domain=character(), domain_level=character(),
  metal=character(), metal_level=character(),
  n=integer(), wt_pct=double(), wt_cil=double(), wt_ciu=double(),
  OR=double(), CI_low=double(), CI_high=double(), p=double(),
  stringsAsFactors=FALSE
)

# --- UNADJUSTED ---
for (var in c(domains, metals)) {
  lvls <- levels(nhanes_cc[[var]])
  for (lvl in lvls[-1]) { # Skip reference
    nval <- sum(nhanes_cc[[var]]==lvl)
    pr <- svymean(~I(get(var)==lvl), survey_design)
    pct <- as.numeric(pr)
    ctl <- as.numeric(confint(pr)[1])
    ctu <- as.numeric(confint(pr)[2])
    form <- as.formula(paste0("CVD ~ ", var))
    fit <- svyglm(form, design=survey_design, family=quasibinomial())
    sm <- summary(fit)
    or <- exp(sm$coefficients[paste0(var, lvl),1])
    ci <- exp(confint(fit)[paste0(var, lvl),])
    pval <- sm$coefficients[paste0(var, lvl),4]
    model_results <- rbind(model_results, data.frame(
      analysis="Unadjusted", domain=ifelse(var %in% domains,var,""), domain_level=ifelse(var %in% domains,lvl,""),
      metal=ifelse(var %in% metals,var,""), metal_level=ifelse(var %in% metals,lvl,""),
      n=nval, wt_pct=round(100*pct,2), wt_cil=round(100*ctl,2), wt_ciu=round(100*ctu,2),
      OR=round(or,3), CI_low=round(ci[1],3), CI_high=round(ci[2],3), p=signif(pval,3)
    ))
  }
}

# --- ADJUSTED ---
for (d in domains) {
  for (m in metals) {
    # domain
    dlvls <- levels(nhanes_cc[[d]])
    for (dlvl in dlvls[-1]) {
      nval <- sum(nhanes_cc[[d]]==dlvl)
      pr <- svymean(~I(get(d)==dlvl), survey_design)
      pct <- as.numeric(pr); ctl <- as.numeric(confint(pr)[1]); ctu <- as.numeric(confint(pr)[2])
      form <- as.formula(paste0("CVD ~ ", d, " + ", m, " + ", paste(adjusters,collapse="+")))
      fit <- svyglm(form, design=survey_design, family=quasibinomial())
      sm <- summary(fit)
      or <- exp(sm$coefficients[paste0(d,dlvl),1])
      ci <- exp(confint(fit)[paste0(d,dlvl),])
      pval <- sm$coefficients[paste0(d,dlvl),4]
      model_results <- rbind(model_results, data.frame(
        analysis="Adjusted", domain=d, domain_level=dlvl,
        metal=m, metal_level="", n=nval, wt_pct=round(100*pct,2),
        wt_cil=round(100*ctl,2), wt_ciu=round(100*ctu,2),
        OR=round(or,3), CI_low=round(ci[1],3), CI_high=round(ci[2],3), p=signif(pval,3)
      ))
    }
    # metal
    mlvls <- levels(nhanes_cc[[m]])
    for (mlvl in mlvls[-1]) {
      nval <- sum(nhanes_cc[[m]]==mlvl)
      pr <- svymean(~I(get(m)==mlvl), survey_design)
      pct <- as.numeric(pr); ctl <- as.numeric(confint(pr)[1]); ctu <- as.numeric(confint(pr)[2])
      form <- as.formula(paste0("CVD ~ ", d, " + ", m, " + ", paste(adjusters,collapse="+")))
      fit <- svyglm(form, design=survey_design, family=quasibinomial())
      sm <- summary(fit)
      or <- exp(sm$coefficients[paste0(m,mlvl),1])
      ci <- exp(confint(fit)[paste0(m,mlvl),])
      pval <- sm$coefficients[paste0(m,mlvl),4]
      model_results <- rbind(model_results, data.frame(
        analysis="Adjusted", domain=d, domain_level="",
        metal=m, metal_level=mlvl, n=nval, wt_pct=round(100*pct,2),
        wt_cil=round(100*ctl,2), wt_ciu=round(100*ctu,2),
        OR=round(or,3), CI_low=round(ci[1],3), CI_high=round(ci[2],3), p=signif(pval,3)
      ))
    }
  }
}

# --- INTERACTION ---
for (d in domains) {
  for (m in metals) {
    dlvls <- levels(nhanes_cc[[d]])
    mlvls <- levels(nhanes_cc[[m]])
    form <- as.formula(paste0("CVD ~ ", d, "*", m, " + ", paste(adjusters,collapse="+")))
    fit <- svyglm(form, design=survey_design, family=quasibinomial())
    sm <- summary(fit)
    for (dlvl in dlvls[-1]) {
      for (mlvl in mlvls[-1]) {
        termname <- paste0(d,dlvl,":",m,mlvl)
        if (termname %in% rownames(sm$coefficients)) {
          or <- exp(sm$coefficients[termname,1]);
          ci <- exp(confint(fit)[termname,])
          pval <- sm$coefficients[termname,4]
          model_results <- rbind(model_results, data.frame(
            analysis="Interaction", domain=d, domain_level=dlvl,
            metal=m, metal_level=mlvl, n=NA, wt_pct=NA, wt_cil=NA, wt_ciu=NA,
            OR=round(or,3), CI_low=round(ci[1],3), CI_high=round(ci[2],3), p=signif(pval,3)
          ))
        }
      }
    }
  }
}

cat("\n===== UNADJUSTED, ADJUSTED, INTERACTION Results: (showing top 20 for brevity) =====\n")
print(head(model_results, n=Inf))

# This code assumes you have:
# survey_design, nhanes_cc, domains, metals already defined

cat("\n=== CROSSTABS WITH n, %, 95% CI + Survey-weighted p-values ===\n")

for (dom in domains) {
  for (met in metals) {
    cat(sprintf("\nCrosstab: %s by %s\n", dom, met))
    tab_counts <- table(nhanes_cc[[dom]], nhanes_cc[[met]])
    dom_lvls <- levels(nhanes_cc[[dom]])
    met_lvls <- levels(nhanes_cc[[met]])
    # Survey Rao-Scott test (global association)
    fmla <- as.formula(sprintf("~%s+%s", dom, met))
    chi_test <- svychisq(fmla, design=survey_design)
    cat(sprintf("Survey-weighted Rao-Scott p-value: %.4g\n", chi_test$p.value))
    # Print header
    cat(sprintf("%-20s", dom))
    for (ml in met_lvls) cat(sprintf("%22s", ml))
    cat("\n")
    # Print crosstab with n (% [95CI])
    for (dl in dom_lvls) {
      cat(sprintf("%-20s", dl))
      for (ml in met_lvls) {
        # Subset: domain == dl
        rowsub <- nhanes_cc[[dom]] == dl
        # Unweighted n for this cell
        cell_n <- sum(rowsub & nhanes_cc[[met]] == ml)
        # Survey-weighted %
        f_prop <- as.formula(sprintf("~I(%s==%s & %s==%s)", dom,
                                     if (is.numeric(nhanes_cc[[dom]])) dl else shQuote(dl),
                                     met, if (is.numeric(nhanes_cc[[met]])) ml else shQuote(ml)))
        f_row <- as.formula(sprintf("~I(%s==%s)", dom, if (is.numeric(nhanes_cc[[dom]])) dl else shQuote(dl)))
        # % among domain group
        cell_mean <- tryCatch({
          svymean(~I(get(met)==ml), subset(survey_design, get(dom)==dl))
        }, error=function(e) NA)
        # 95% CI for %
        pct   <- if (!is.na(cell_mean[1])) as.numeric(cell_mean[1])*100 else NA
        ci    <- if (!is.na(cell_mean[1])) as.numeric(confint(cell_mean)[1,])*100 else c(NA, NA)
        cat(sprintf("%7d (%.1f%%) [%.1f, %.1f]", cell_n, pct, ci[1], ci[2]))
      }
      cat("\n")
    }
  }
}
library(tibble)
library(dplyr)
library(ggplot2)
library(forcats)

forest_data <- tribble(
  ~Domain,    ~Metal,          ~Analysis,     ~OR,   ~CI_low, ~CI_high, ~p,
  "Cognition","High Lead",     "Unadj",       1.73,  1.12,    2.68,     0.018,
  "Cognition","High Lead",     "Adjusted",    1.73,  1.07,    2.79,     0.036,
  "Cognition","High Lead",     "Interaction", 0.71,  0.09,    5.57,     0.55,
  
  "Cognition","High Cadmium",  "Unadj",       1.76,  1.16,    2.66,     0.011,
  "Cognition","High Cadmium",  "Adjusted",    1.71,  1.07,    2.73,     0.036,
  "Cognition","High Cadmium",  "Interaction", 1.65,  0.18,   15.42,     0.44,
  
  "Cognition","High Mercury",  "Unadj",       0.71,  0.40,    1.28,     0.24,
  "Cognition","High Mercury",  "Adjusted",    1.70,  1.04,    2.77,     0.041,
  "Cognition","High Mercury",  "Interaction", 0.98,  0.11,    8.89,     0.97,
  
  "Cognition","High Manganese","Unadj",       1.18,  0.75,    1.87,     0.45,
  "Cognition","High Manganese","Adjusted",    1.75,  1.06,    2.87,     0.037,
  "Cognition","High Manganese","Interaction", 0.72,  0.05,   11.54,     0.66,
  
  "Cognition","High Selenium", "Unadj",       0.70,  0.42,    1.17,     0.16,
  "Cognition","High Selenium", "Adjusted",    1.72,  1.06,    2.79,     0.038,
  "Cognition","High Selenium", "Interaction", 2.80,  0.49,   15.96,     0.13,
  
  "Mobility","High Lead",      "Unadj",       4.62,  3.25,    6.55,     0.001,
  "Mobility","High Lead",      "Adjusted",    2.04,  1.14,    3.65,     0.030,
  "Mobility","High Lead",      "Interaction", 1.18,  0.20,    6.84,     0.73,
  
  "Mobility","High Cadmium",   "Unadj",       4.62,  3.25,    6.55,     0.001,
  "Mobility","High Cadmium",   "Adjusted",    1.99,  1.12,    3.55,     0.032,
  "Mobility","High Cadmium",   "Interaction", 0.80,  0.11,    6.00,     0.68,
  
  "Mobility","High Mercury",   "Unadj",       0.71,  0.40,    1.28,     0.24,
  "Mobility","High Mercury",   "Adjusted",    1.98,  1.11,    3.54,     0.033,
  "Mobility","High Mercury",   "Interaction", 0.63,  0.07,    5.93,     0.46,
  
  "Mobility","High Manganese", "Unadj",       1.18,  0.75,    1.87,     0.45,
  "Mobility","High Manganese", "Adjusted",    2.01,  1.14,    3.57,     0.030,
  "Mobility","High Manganese", "Interaction", 2.38,  0.48,   11.86,     0.15,
  
  "Mobility","High Selenium",  "Unadj",       0.70,  0.42,    1.17,     0.16,
  "Mobility","High Selenium",  "Adjusted",    2.01,  1.11,    3.64,     0.033,
  "Mobility","High Selenium",  "Interaction", 0.77,  0.06,    9.21,     0.69,
  
  "Seeing","High Lead",        "Unadj",       2.26,  1.51,    3.38,     0.001,
  "Seeing","High Lead",        "Adjusted",    1.70,  0.82,    3.53,     0.10,
  "Seeing","High Lead",        "Interaction", 1.54,  0.34,    7.10,     0.34,
  
  "Seeing","High Cadmium",     "Unadj",       2.26,  1.51,    3.38,     0.001,
  "Seeing","High Cadmium",     "Adjusted",    1.69,  0.82,    3.47,     0.11,
  "Seeing","High Cadmium",     "Interaction", 0.71,  0.14,    3.53,     0.46,
  
  "Seeing","High Mercury",     "Unadj",       0.71,  0.40,    1.28,     0.24,
  "Seeing","High Mercury",     "Adjusted",    1.58,  0.77,    3.53,     0.80,
  "Seeing","High Mercury",     "Interaction", 1.16,  0.13,   10.63,     0.80,
  
  "Seeing","High Manganese",   "Unadj",       1.18,  0.75,    1.87,     0.45,
  "Seeing","High Manganese",   "Adjusted",    1.68,  0.81,    3.53,     0.11,
  "Seeing","High Manganese",   "Interaction", 1.21,  0.11,   13.02,     0.76,
  
  "Seeing","High Selenium",    "Unadj",       0.70,  0.42,    1.17,     0.16,
  "Seeing","High Selenium",    "Adjusted",    1.68,  0.81,    3.48,     0.11,
  "Seeing","High Selenium",    "Interaction", 1.60,  0.15,   17.24,     0.49
)

forest_data <- forest_data %>%
  mutate(
    Model = dplyr::recode(Analysis,
                          "Unadj" = "Unadjusted",
                          "Adjusted" = "Adjusted",
                          "Interaction" = "Interaction"),
    p_label = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    Group = paste(Domain, Metal, sep=": ")
  )

ggplot(
  forest_data,
  aes(
    y = fct_rev(factor(Group, levels=rev(unique(Group)))), # forest plot style
    x = OR,
    xmin = CI_low, xmax = CI_high,
    color = Model,
    shape = Model
  )
) +
  geom_vline(xintercept = 1, linetype = "dashed", color="gray60") +
  geom_errorbarh(height=0.22, size=0.8, position=position_dodge(width=0.6)) +
  geom_point(size=2.4, position=position_dodge(width=0.6)) +
  scale_x_log10(
    breaks = c(0.1, 0.3, 1, 3, 10, 30), 
    labels = c("0.1", "0.3", "1", "3", "10", "30"),
    limits = c(0.08,40)
  ) +
  labs(
    x = "Odds Ratio (log scale)",
    y = "",
    title = " Metal Exposure and Functional Limitation"
  ) +
  theme_bw(base_size=14) +
  theme(
    axis.text.y = element_text(size=10),
    legend.position = "right"
  ) +
  facet_grid(Domain ~ ., scales = "free_y", space = "free_y") +
  guides(shape = guide_legend(order=1), color=guide_legend(order=1))


# 5. TABLE 1: DESCRIPTIVE ----

library(survey)

categorical_vars <- c(
  "CVD", "cognition_bin", "mobility_bin", "seeing_bin",
  "highlead", "highcadmium", "highmercury", "highmanganese", "highselenium",
  "sex", "race_eth", "alcohol", "smoke_status", "married", "hypertension", "educ"
)

desc_cat <- data.frame(
  variable=character(),
  level=character(),
  n=integer(),
  pct=double(),
  ci_low=double(),
  ci_high=double(),
  stringsAsFactors=FALSE
)

for (var in categorical_vars) {
  factor_var <- as.factor(nhanes_cc[[var]])
  lvls <- levels(factor_var)
  n_tab <- table(factor_var)
  prop_res <- svymean(~as.factor(nhanes_cc[[var]]), survey_design)
  prop_ci <- confint(prop_res)
  for (i in seq_along(lvls)) {
    lvl <- lvls[i]
    lvl_count <- as.integer(n_tab[lvl])
    pct <- as.numeric(prop_res[i]) * 100
    ci_low <- prop_ci[i, 1] * 100
    ci_high <- prop_ci[i, 2] * 100
    desc_cat <- rbind(desc_cat, data.frame(
      variable = var,
      level    = lvl,
      n        = lvl_count,
      pct      = round(pct,2),
      ci_low   = round(ci_low,2),
      ci_high  = round(ci_high,2),
      stringsAsFactors = FALSE
    ))
  }
}

cat("\n===== Table 1 (n, weighted %, 95% CI): =====\n")
print(desc_cat)

# Optional: add a summary column for presentation
desc_cat$summary <- sprintf("%i (%.1f%%, %.1f–%.1f%%)", desc_cat$n, desc_cat$pct, desc_cat$ci_low, desc_cat$ci_high)
print(desc_cat[,c("variable", "level", "summary")])


