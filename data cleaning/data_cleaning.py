# Import required packages
import pandas as pd
import numpy as np


# Load data
data = pd.read_stata("/Users/christianfang/Downloads/NFN_Main_Refresh_Sample_wave_3_v1.dta", convert_categoricals=False)

# remove singles and LAT people


def remove_singles(df):
    """This function removes single respondents"""
    df['single'] = df['A3L02a'].map({1: 1, 2: 0})
    filter1 = df['single'] == 0
    df = df[filter1]
    return df

# Remove people with LAT partner


def remove_lat(df):
    """This function removes respondents with LAT partners"""
    df['LAT'] = df['A3L02b'].map({1: 1, 2: 0, 3: 0})
    filterlat = df['LAT'] == 0
    df = df[filterlat]
    return df

# Remove nonresident residence arrangements


def remove_nonresidence(df):
    """This function removes respondents who only have nonresident children"""

    conditions = [
        df['resstepchild'] == 1,  # resident step -> in
        df['resstepchild'] == 3,  # shared res step -> in
        (df['resstepchild'] == 0) & (df['resbiochild'] == 2),  # No step, nonres bio -> out
        (df['resstepchild'] == 0) & (df['resbiochild'] == 4),  # No step, bio alone -> out
        (df['resstepchild'] == 0) & ((df['resbiochild'] != 4) | (df['resbiochild'] != 2)),  # no step, bio other -> in
        (df['resstepchild'] == 2) & ((df['resbiochild'] == 2) | (df['resbiochild'] == 4)),  # Nonres step, nonres/alone bio -> out
        (df['resstepchild'] == 2) & ((df['resbiochild'] != 2) | (df['resbiochild'] != 4)),  # Nonres step & bio other -> in
        (df['resstepchild'] == 4) & ((df['resbiochild'] == 2) | (df['resbiochild'] == 4)),  # Alone step, bio nonres -> out
        (df['resstepchild'] == 4) & ((df['resbiochild'] != 2) | (df['resbiochild'] != 4)), # Alone step & bio other -> in
    ]
    choices = [1, 1, 0, 0, 1, 0, 1, 0, 1]
    nonres_filter = np.select(conditions, choices)
    nonres_filter = nonres_filter == 1
    filtered_df = df[nonres_filter]

    return filtered_df

    #nonresstepchild = df['A3P10'].map({1: 0, 2: 1, 3: 0, 4: 1})  # 0: Res, 1: nonres
    #conditions = [
    #    (df['step'] == 0) & (nonresstepchild == 0),  # No step, res
    #    (df['step'] == 0) & (nonresstepchild == 1),  # No step, nonres
    #    (df['step'] == 1) & (nonresstepchild == 0),  # Step, res
    #    (df['step'] == 1) & (nonresstepchild == 1)]  # Step, nonres
    # 0 means no step, 1 means res step, 2 means nonres step
    #choicelist = [0, 0, 1, 2]
    #nonresstepchild_filter = np.select(conditions, choicelist)

    # Create filter for nonresident focal child where 1 means nonres
    #nonresbiochild = df['A3I08'].map({1:0, 2:1, 3:0, 4:1}) # 0: Res, 1: nonres
    #conditions = [
    #    (nonresbiochild == 0.0) & (nonresstepchild_filter == 0), # resident - no step -> 0
    #    (nonresbiochild == 0.0) & (nonresstepchild_filter == 1), # res - res -> 1
    #    (nonresbiochild == 0.0) & (nonresstepchild_filter == 2), # resident - non resident -> 2
    #    (nonresbiochild == 1.0) & (nonresstepchild_filter == 1), # non-resident - resident -> 2
    #    (nonresbiochild == 1.0) & (nonresstepchild_filter == 2), # nonresident - nonresident -> 3
    #]
    #choices = [0, 1, 2, 2, 3]
    #df["combinations"] = np.select(conditions, choices)
    #filter_combinations = df["combinations"] != 3
    #df = df[filter_combinations]
    #return df

# Define function to exclude rows


def exclusion(df):
    """This function exludes rows on the defined criteria"""
    n_total = len(df)
    d1 = remove_singles(df)
    print("Excluding singles: " + str(n_total - len(d1)))
    d2 = remove_lat(d1)
    print("Excluding LATs :" + str(len(d1) - len(d2)))
    d3 = remove_nonresidence(d2)
    print("Excluding nonres-nonres: " + str(len(d2) - len(d3)))
    print("Remaining sample size: " + str(len(d3)))
    return d3


# Define function to create new variables

def make_vars(df):
    """This function creates all remaining variables relevant for the analyses, 
    so that the data frame is ready for imputation"""
    # Filter variables

    df['single'] = df['A3L02a'].map({1: 1, 2: 0})

    df['LAT'] = df['A3L02b'].map({1: 1, 2: 0, 3: 0})

    # Cohesion scale
    # Recode 88 to missing
    cohes_a = df["A3T01_a"].replace(88.0, np.nan)
    cohes_b = df["A3T01_b"].replace(88.0, np.nan)
    df["cohes_c"] = df["A3T01_c"].replace(88.0, np.nan)
    cohes_d = df["A3T01_d"].replace(88.0, np.nan)
    df["cohes_a"] = cohes_a.map({1: 5, 2: 4, 3: 3, 4: 2, 5: 1})
    df["cohes_b"] = cohes_b.map({1: 5, 2: 4, 3: 3, 4: 2, 5: 1})
    df["cohes_d"] = cohes_d.map({1: 5, 2: 4, 3: 3, 4: 2, 5: 1})

    # Concrete baby
    concrete = df["A3L04x"].map({1: 0, 2: 1})
    df["sharedchild"] = concrete

    # Part-time
    conditions = [
                (df["A3I08"] == 3),
                (df["A3P10"] == 3)]
    choices = [1, 1]
    df["parttime"] = np.select(conditions, choices)

    # Residence focal child
    df["resbiochild"] = df['A3I08'].map({1: 1, 2: 2, 3: 3, 4: 4, 5: np.nan})

    # Residence stepchild

    df['step'] = df["A3L06"].map({1: 1, 2: 0})
    conditions = [
        df['step'] == 0,   # No stepchild -> 0
        df['A3P10'] == 1,  # Resident stepchild -> 1
        df['A3P10'] == 2,  # Nonresident stepchild -> 2
        df['A3P10'] == 3,  # Shared resident stepchild -> 3
        df['A3P10'] == 4   # By themselves -> 4
        ]
    # 0 means no step, 1 means res step, 2 means nonres step
    choicelist = [0, 1, 2, 3, 2]
    df['resstepchild'] = np.select(conditions, choicelist)

    # Residence bio child

    conditions = [
        df['resbiochild'] == 1,
        df['resbiochild'] == 2,
        df['resbiochild'] == 3,
        df['resbiochild'] == 4,
        df['resbiochild'] == np.nan,
    ]

    choices = [1, 2, 3, 2, np.nan]

    df['resbiochild_nonres_recode'] = np.select(conditions, choices)

    # Control variables

    # Age child
    df["age_child"] = df["age_child_w3"]

    # Gender child
    df["female_child"] = df["A3I04"].map({1: 0, 2: 1})

    # Age parent
    df["age_parent"] = df["age_respondent_w3"]

    # Gender parent
    df["female_respondent"] = df["A3A01"].map({1: 0, 2: 1})

    # Union duration
    date = np.where((df["A3P16_b"] < 2009), 2009, df["A3P16_b"])
    df["duration"] = 2020 - date

    # Education parent
    df["educ_par"] = df["A3A03"]

    # Education current partner
    df["educ_partner"] = df["A3P03"]

    # Age current partner
    df["age_partner"] = df["A3P02"]

    # Relationship quality w. child
    df['relqual_child'] = df['A3J02']

    # Relationship quality w. partner
    df['relqual_partner'] = df['A3P12']

    # Relationship quality child -partner
    df['relqual_child_partner'] = df['A3J29']

    return data 


# Define main function




frame1 = make_vars(data)

frame1.to_csv("/Users/christianfang/Downloads/data_cleaned.csv")