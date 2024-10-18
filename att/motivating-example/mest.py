import numpy as np
import pandas as pd

import delicatessen
from delicatessen import MEstimator
from delicatessen.estimating_equations import ee_regression
from delicatessen.utilities import inverse_logit

filename = "../../data/motivating-example/adstroke_dummy.csv"
d = pd.read_csv(filename)
d['intercept'] = 1 # Intercept value
a = np.asarray(d['tpa'])
W = np.asarray(d[["intercept", "ps_vars_gender_aje_binary" ,"ps_vars_paresis_aje" ,"ps_vars_aphasia_aje" ,"ps_vars_htn_aje","ps_vars_diab_aje","ps_vars_afib_aje","ps_vars_hxstroke_aje", "ps_vars_agecat_aje45_49", "ps_vars_agecat_aje50_54", "ps_vars_agecat_aje55_59", "ps_vars_agecat_aje60_64", "ps_vars_agecat_aje65_69", "ps_vars_agecat_aje70_74", "ps_vars_agecat_aje75_79", "ps_vars_agecat_aje80_84", "ps_vars_agecat_aje85_89", "ps_vars_agecat_aje90_94", "ps_vars_agecat_aje95_99", "ps_vars_home_care_ajeIncareathome", "ps_vars_home_care_ajeIndependentathome", "ps_vars_rankin_aje4_5", "ps_vars_sxhours_aje_1_3hours", "ps_vars_sxhours_aje3_4hours", "ps_vars_sxhours_aje_4hours", "ps_vars_conscious_ajeSomnolent_Comatose", "ps_vars_ward_ajeICU", "ps_vars_ward_ajeGeneral"]])
Y = np.asarray(d['death'])


def psi(theta):
    # Dividing parameters into corresponding parts and labels from slides
    alpha = theta[0:28]              # Logistic model coefficients
    mu_hajek_1, mu_hajek_0, mu_smr_1, mu_smr_0 = theta[28], theta[29], theta[30], theta[31]   # HT Causal risks
    delta_hajek_lnrr, delta_hajek_lnor, delta_smr_lnrr, delta_smr_lnor = theta[32], theta[33], theta[34], theta[35]
    # Logistic regression model for propensity score
    ee_logit = ee_regression(theta=alpha,       # Regression model
                            y=a,               # ... for exposure
                            X=W,               # ... given confounders
                            model='logistic')  # ... logistic model
    
    pscore = inverse_logit(np.dot(W, alpha))    # Propensity score
    ipt = a/pscore + (1-a)/(1-pscore)  # Corresponding IPT weights
    smr = a*1 + (1-a)*pscore/(1-pscore)

    # Hajek IPT Estimating function for causal risk under a=1
    ee_hajek_r1 = a*ipt*(Y-mu_hajek_1) # Weighted conditional mean
    # Hajek IPT Estimating function for causal risk under a=0
    ee_hajek_r0 = (1-a)*ipt*(Y-mu_hajek_0)  # Weighted conditional mean

    # SMR Estimating function for causal risk under a=1
    ee_smr_r1 = a*smr*(Y-mu_smr_1) # Weighted conditional mean
    # SMR Estimating function for causal risk under a=0
    ee_smr_r0 = (1-a)*smr*(Y-mu_smr_0)  # Weighted conditional mean

    # IPT log RR and log OR
    ef_ipt_lnrr = np.ones(d.shape[0])*((np.log(mu_hajek_1) - np.log(mu_hajek_0)) - delta_hajek_lnrr)
    ef_ipt_lnor = np.ones(d.shape[0])*((np.log(mu_hajek_1*(1-mu_hajek_0)) - np.log(mu_hajek_0*(1-mu_hajek_1))) - delta_hajek_lnor)

     # SMR log RR and log OR
    ef_smr_lnrr = np.ones(d.shape[0])*((np.log(mu_smr_1) - np.log(mu_smr_0)) - delta_smr_lnrr)
    ef_smr_lnor = np.ones(d.shape[0])*((np.log(mu_smr_1*(1-mu_smr_0)) - np.log(mu_smr_0*(1-mu_smr_1))) - delta_smr_lnor)

    # Returning stacked estimating functions in order of parameters
    return np.vstack([ee_logit, ee_hajek_r1, ee_hajek_r0, ee_smr_r1, ee_smr_r0, ef_ipt_lnrr, ef_ipt_lnor, ef_smr_lnrr, ef_smr_lnor])   

estr = MEstimator(psi, init = np.array([-0.94668997, -0.09358322,  1.04992415,  0.65388860,  0.14329938, -0.04301547, -1.50966856, -0.65952770, -0.16262071, -0.37015059, -0.15013694, -0.36553431, -0.51457072, -0.23789579, -0.45736246, -0.23221438, -0.21315443, -0.33316786, -1.12213584, -0.01820426,  0.70665858,  1.11381661, -0.34743885, -1.23589520, -4.04627689, -0.43997071,  1.17918883, -1.75644999, 0.5, 0.5, 0.5, 0.5, 0, 0, 0, 0]))
estr.estimate()
estr.theta

result = pd.DataFrame()
result['Param'] = ["intercept", "ps_vars_gender_aje_binary" ,"ps_vars_paresis_aje" ,"ps_vars_aphasia_aje" ,
                   "ps_vars_htn_aje","ps_vars_diab_aje","ps_vars_afib_aje","ps_vars_hxstroke_aje", 
                   "ps_vars_agecat_aje45_49", "ps_vars_agecat_aje50_54", "ps_vars_agecat_aje55_59", 
                   "ps_vars_agecat_aje60_64", "ps_vars_agecat_aje65_69", "ps_vars_agecat_aje70_74", 
                   "ps_vars_agecat_aje75_79", "ps_vars_agecat_aje80_84", "ps_vars_agecat_aje85_89", 
                   "ps_vars_agecat_aje90_94", "ps_vars_agecat_aje95_99", "ps_vars_home_care_ajeIncareathome",
                    "ps_vars_home_care_ajeIndependentathome", "ps_vars_rankin_aje4_5", "ps_vars_sxhours_aje_1_3hours",
                    "ps_vars_sxhours_aje3_4hours", "ps_vars_sxhours_aje_4hours", "ps_vars_conscious_ajeSomnolent_Comatose",
                    "ps_vars_ward_ajeICU", "ps_vars_ward_ajeGeneral", "ipt_r1", "ipt_r0", "smr_r1", "smr_r0", "ipt_lnrr", 
                    "ipt_lnor", "smr_lnrr", "smr_lnor"]
result['Coef'] = estr.theta
result['Var'] = np.diag(estr.variance)
result['SE'] = np.sqrt(np.diag(estr.variance))
ci = estr.confidence_intervals()
result['LCL'] = ci[:, 0]
result['UCL'] = ci[:, 1]

outfile = "../../data/motivating-example/results/mestimates_full.csv"
result.to_csv(outfile, index = False)


# 6000 person sample
filename = "../../data/motivating-example/adstroke_6000_dummy.csv"
d = pd.read_csv(filename)
d['intercept'] = 1 # Intercept value
a = np.asarray(d['tpa'])
W = np.asarray(d[["intercept", "ps_vars_gender_aje_binary" ,"ps_vars_paresis_aje" ,"ps_vars_aphasia_aje" ,"ps_vars_htn_aje","ps_vars_diab_aje","ps_vars_afib_aje","ps_vars_hxstroke_aje", "ps_vars_agecat_aje45_49", "ps_vars_agecat_aje50_54", "ps_vars_agecat_aje55_59", "ps_vars_agecat_aje60_64", "ps_vars_agecat_aje65_69", "ps_vars_agecat_aje70_74", "ps_vars_agecat_aje75_79", "ps_vars_agecat_aje80_84", "ps_vars_agecat_aje85_89", "ps_vars_agecat_aje90_94", "ps_vars_agecat_aje95_99", "ps_vars_home_care_ajeIncareathome", "ps_vars_home_care_ajeIndependentathome", "ps_vars_rankin_aje4_5", "ps_vars_sxhours_aje_1_3hours", "ps_vars_sxhours_aje3_4hours", "ps_vars_sxhours_aje_4hours", "ps_vars_conscious_ajeSomnolent_Comatose", "ps_vars_ward_ajeICU", "ps_vars_ward_ajeGeneral"]])
Y = np.asarray(d['death'])


def psi(theta):
    # Dividing parameters into corresponding parts and labels from slides
    alpha = theta[0:28]              # Logistic model coefficients
    mu_hajek_1, mu_hajek_0, mu_smr_1, mu_smr_0 = theta[28], theta[29], theta[30], theta[31]   # HT Causal risks
    delta_hajek_lnrr, delta_hajek_lnor, delta_smr_lnrr, delta_smr_lnor = theta[32], theta[33], theta[34], theta[35]
    # Logistic regression model for propensity score
    ee_logit = ee_regression(theta=alpha,       # Regression model
                            y=a,               # ... for exposure
                            X=W,               # ... given confounders
                            model='logistic')  # ... logistic model
    
    pscore = inverse_logit(np.dot(W, alpha))    # Propensity score
    ipt = a/pscore + (1-a)/(1-pscore)  # Corresponding IPT weights
    smr = a*1 + (1-a)*pscore/(1-pscore)

    # Hajek IPT Estimating function for causal risk under a=1
    ee_hajek_r1 = a*ipt*(Y-mu_hajek_1) # Weighted conditional mean
    # Hajek IPT Estimating function for causal risk under a=0
    ee_hajek_r0 = (1-a)*ipt*(Y-mu_hajek_0)  # Weighted conditional mean

    # SMR Estimating function for causal risk under a=1
    ee_smr_r1 = a*smr*(Y-mu_smr_1) # Weighted conditional mean
    # SMR Estimating function for causal risk under a=0
    ee_smr_r0 = (1-a)*smr*(Y-mu_smr_0)  # Weighted conditional mean

    # IPT log RR and log OR
    ef_ipt_lnrr = np.ones(d.shape[0])*((np.log(mu_hajek_1) - np.log(mu_hajek_0)) - delta_hajek_lnrr)
    ef_ipt_lnor = np.ones(d.shape[0])*((np.log(mu_hajek_1*(1-mu_hajek_0)) - np.log(mu_hajek_0*(1-mu_hajek_1))) - delta_hajek_lnor)

     # SMR log RR and log OR
    ef_smr_lnrr = np.ones(d.shape[0])*((np.log(mu_smr_1) - np.log(mu_smr_0)) - delta_smr_lnrr)
    ef_smr_lnor = np.ones(d.shape[0])*((np.log(mu_smr_1*(1-mu_smr_0)) - np.log(mu_smr_0*(1-mu_smr_1))) - delta_smr_lnor)

    # Returning stacked estimating functions in order of parameters
    return np.vstack([ee_logit, ee_hajek_r1, ee_hajek_r0, ee_smr_r1, ee_smr_r0, ef_ipt_lnrr, ef_ipt_lnor, ef_smr_lnrr, ef_smr_lnor])   

estr = MEstimator(psi, init = np.concatenate([np.zeros(28), np.array([0.5, 0.5, 0.5, 0.5, 0, 0, 0, 0])]))
estr.estimate()
estr.theta

result = pd.DataFrame()
result['Param'] = ["intercept", "ps_vars_gender_aje_binary" ,"ps_vars_paresis_aje" ,"ps_vars_aphasia_aje" ,
                   "ps_vars_htn_aje","ps_vars_diab_aje","ps_vars_afib_aje","ps_vars_hxstroke_aje", 
                   "ps_vars_agecat_aje45_49", "ps_vars_agecat_aje50_54", "ps_vars_agecat_aje55_59", 
                   "ps_vars_agecat_aje60_64", "ps_vars_agecat_aje65_69", "ps_vars_agecat_aje70_74", 
                   "ps_vars_agecat_aje75_79", "ps_vars_agecat_aje80_84", "ps_vars_agecat_aje85_89", 
                   "ps_vars_agecat_aje90_94", "ps_vars_agecat_aje95_99", "ps_vars_home_care_ajeIncareathome",
                    "ps_vars_home_care_ajeIndependentathome", "ps_vars_rankin_aje4_5", "ps_vars_sxhours_aje_1_3hours",
                    "ps_vars_sxhours_aje3_4hours", "ps_vars_sxhours_aje_4hours", "ps_vars_conscious_ajeSomnolent_Comatose",
                    "ps_vars_ward_ajeICU", "ps_vars_ward_ajeGeneral", "ipt_r1", "ipt_r0", "smr_r1", "smr_r0", "ipt_lnrr", 
                    "ipt_lnor", "smr_lnrr", "smr_lnor"]
result['Coef'] = estr.theta
result['Var'] = np.diag(estr.variance)
result['SE'] = np.sqrt(np.diag(estr.variance))
ci = estr.confidence_intervals()
result['LCL'] = ci[:, 0]
result['UCL'] = ci[:, 1]

outfile = "../../data/motivating-example/results/mestimates_6000.csv"
result.to_csv(outfile, index = False)