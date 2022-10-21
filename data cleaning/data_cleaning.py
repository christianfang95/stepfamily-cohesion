#Import required packages
import pandas as pd
import numpy as np


#Load data
data = pd.read_stata("/Volumes/webdav.uu.nl/Data/Research/FSW/Research_data/SOC/Anne-Rigt Poortman/Nieuwe Families NL/Zielinski/Cohesion paper/NFN_Main_Refresh_Sample_wave_3_v1.dta", convert_categoricals=False)

#remove singles and LAT people
def remove_singles(data):
    """This function removes single respondents"""
    data['single'] = data['A3L02a'].map({1:1, 2:0})
    filter1 = data['single'] == 0
    data = data[filter1]
    return data

#Remove people with LAT partner
def remove_LAT(data):
    """This function removes respondents with LAT partners"""
    data['LAT'] = data['A3L02b'].map({1:1, 2:0, 3:0})
    filter = data['LAT'] == 0
    data = data[filter]
    return data

#Remove nonresident residence arrangements
def remove_nonresidence(data):
    """This function removes respondents who only have nonresident children"""
    step = data["A3L06"].map({1:1, 2:0})
    nonresstepchild = data['A3P10'].map({1:0, 2:1, 3:0, 4:1}) #0: Res, 1: nonres
    conditions = [
    (step == 0) & (nonresstepchild == 0), #No step, res
    (step == 0) & (nonresstepchild == 1), #No step, nonres 
    (step == 1) & (nonresstepchild == 0), #Step, res
    (step == 1) & (nonresstepchild == 1)] #Step, nonres
    # 0 means no step, 1 means res step, 2 means nonres step
    choicelist = [0, 0, 1, 2]
    nonresstepchild_filter = np.select(conditions, choicelist) 

    #Create filter for nonresident focal child where 1 means nonres
    nonresbiochild = data['A3I08'].map({1:0, 2:1, 3:0, 4:1}) #0: Res, 1: nonres
    conditions = [
        (nonresbiochild == 0.0) & (nonresstepchild_filter == 0), #resident - no step -> 0
        (nonresbiochild == 0.0) & (nonresstepchild_filter == 1), #res - res -> 1
        (nonresbiochild == 0.0) & (nonresstepchild_filter == 2), #resident - non resident -> 2
        (nonresbiochild == 1.0) & (nonresstepchild_filter == 1), #non-resident - resident -> 2
        (nonresbiochild == 1.0) & (nonresstepchild_filter == 2), #nonresident - nonresident -> 3
    ]
    choices = [0, 1, 2, 2, 3]
    data["combinations"] = np.select(conditions, choices)
    filter = data["combinations"] != 3
    data = data[filter]
    return data

#Define function to exclude rows
def exclusion(data):
    """This function exludes rows on the defined criteria"""
    N_total = len(data)
    d1 = remove_singles(data)
    print("Excluding singles: " + str(N_total - len(d1)))
    d2 = remove_LAT(d1)
    print("Excluding LATs :" + str(len(d1) - len(d2)))
    d3 = remove_nonresidence(d2)
    print("Excluding nonres-nonres: " + str(len(d2) - len(d3)))
    print("Remaining sample size: " + str(len(d3)))
    return d3


#### Define function to create new variables

def make_vars(data):
    """This function creates all remaining variables relevant for the analyses, 
    so that the data frame is ready for imputation"""
    #Cohesion scale
    #Recode 88 to missing
    cohes_a = data["A3T01_a"].replace(88.0, np.nan)
    cohes_b = data["A3T01_b"].replace(88.0, np.nan)
    data["cohes_c"] = data["A3T01_c"].replace(88.0, np.nan)
    cohes_d = data["A3T01_d"].replace(88.0, np.nan)
    data["cohes_a"] = cohes_a.map({1:5, 2:4, 3:3, 4:2, 5:1})
    data["cohes_b"] = cohes_b.map({1:5, 2:4, 3:3, 4:2, 5:1})
    data["cohes_d"] = cohes_d.map({1:5, 2:4, 3:3, 4:2, 5:1})

    #Concrete baby
    concrete = data["A3L04x"].map({1:0, 2:1})
    data["sharedchild"] = concrete

    #Part-time
    conditions = [
                (data["A3I08"] == 3),
                (data["A3P10"] == 3)]
    choices = [1, 1]
    data["parttime"] = np.select(conditions, choices)

    #Control variables

    #Age child
    data["age_child"] = data["age_child_w3"]

    #Gender child
    data["female_child"] = data["A3I04"].map({1:0, 2:1})

    #Age parent
    data["age_parent"] = data["age_respondent_w3"]

    #Gender parent
    data["female_respondent"] = data["A3A01"].map({1:0, 2:1})

    #Union duration
    date = np.where((data["A3P16_b"] < 2009), 2009, data["A3P16_b"])
    data["duration"] = 2020 - date

    #Education parent
    data["educ_par"] = data["A3A03"]

    #Education current partner
    data["educ_partner"] = data["A3P03"]

    #Age current partner
    data["age_partner"] = data["A3P02"]

    return data 


#Define main function

def main(dataset):
    """This function executes all functions defined above sequentially
    and returns the final data frame"""
    d1 = exclusion(dataset)
    d2 = make_vars(d1)
    return d2

frame = main(data)
