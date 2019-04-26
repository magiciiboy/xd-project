import os
import math
from statistics import stdev, mean
import pandas as pd

df_scholars = pd.read_csv('../../data/Faculty_Googlescholar_lambda_mu_N4190.csv')
df_papers = pd.read_csv('../../data/GoogleScholar_paper_stats.csv', names=['i', 't', 'c', 'coauthors'])

shape_scholars = df_scholars.shape  # (4190, 25)
shape_papers = df_papers.shape      # (424827, 4)

df_papers_2017 = df_papers[df_papers['t'] <= 2017]
# print(df_papers_2017.shape)
# print(df_scholars.columns)

# Cached mu, sigma for y,t
cached_mu_sigma = dict()
idx = 0

# Map scholars to dict
scholars = dict()
for index, s_row in df_scholars.iterrows():
    scholars[s_row['google_id']] = s_row

def compute_logc(p):
    return math.log(1 + (p['c'] or 0))

def compute_dept(p):
    return scholars[p['i']]['dept']

def compute_z_score(p):
    c_ipst = p['c']
    t = p['t']
    s = p['dept']

    # Year t, disciplinary s
    str_st = '%s_%s' % (t, s)
    if str_st in cached_mu_sigma:
        (mu_t, sigma_t) = cached_mu_sigma[str_st]
    else:
        df_papers_st = df_papers[(df_papers['dept']==s) & (df_papers['t']==t)].copy()
        df_papers_st['ln_c'] = df_papers_st.apply(lambda row: compute_logc(row), axis=1)

        mu_t = df_papers_st['ln_c'].mean()
        sigma_t = df_papers_st['ln_c'].std()

        cached_mu_sigma[str_st] = (mu_t, sigma_t)

        del df_papers_st

    # Compare the paper p of scholar i with other papers in same department
    # in the same year
    z_ip = (math.log(1 + c_ipst) - mu_t) / sigma_t
    return z_ip

def compute_coauthor(p):
    coauthors = (p.get('coauthors') or '').split(',')
    return len(coauthors) or 1

def compute_career_age(p):
    gid = p['i']
    career_age = int(p['t']) - int(scholars[gid]['min_year']) + 1 if gid in scholars else 1
    return career_age

def compute_paper_orientation(p):
    coauthors = (p.get('coauthors') or '').split(',')
    dept_map = {'CS': '1', 'BIO': '0'}
    depts = []
    for a in coauthors:
        if a not in ['0', '1', '2']:
            a_dept = scholars[a]['dept']
            if dept_map[a_dept] not in depts:
                depts.append(dept_map[a_dept])
        else:
            if a not in depts:
                depts.append(a)
    return 1 if '0' in depts and '1' in depts else 0

def compute_scholar_xd(p):
    gid = p['i']
    # return 1 if scholars[gid]['XDIndicator'] == 'XD' else 0
    return 1 if scholars[gid]['XDGSREFINED'] == 'XD' else 0

def compute_pagerank(p):
    gid = p['i']
    pr = scholars[gid]['PRCentrality']
    return pr

def compute_bridge_ratio(p):
    gid = p['i']
    ratio = scholars[gid]['Lambda'] or 0.00000001
    return ratio

# Add new column to process papers data
print('Computing dept ...')
df_papers['dept'] = df_papers.apply(lambda row: compute_dept(row), axis=1)

print('Computing a ...')
df_papers['a'] = df_papers.apply(lambda row: compute_coauthor(row), axis=1)

print('Computing tau ...')
df_papers['tau'] = df_papers.apply(lambda row: compute_career_age(row), axis=1)

print('Computing I ...')
df_papers['I'] = df_papers.apply(lambda row: compute_paper_orientation(row), axis=1)

print('Computing ln(a) ...')
df_papers['ln_a'] = df_papers.apply(lambda row: math.log(row['a']) if row['a'] > 0 else 0, axis=1)

print('Computing XD ...')
df_papers['XD'] = df_papers.apply(lambda row: compute_scholar_xd(row), axis=1)

print('Computing PR ...')
df_papers['PR'] = df_papers.apply(lambda row: compute_pagerank(row), axis=1)

print('Computing Bridge ratio ...')
df_papers['lambda'] = df_papers.apply(lambda row: compute_bridge_ratio(row), axis=1)

print('Computing z-score ...')
df_papers['z'] = df_papers.apply(lambda row: compute_z_score(row), axis=1)

print(df_papers.columns)
# help(pd.read_csv)

# df_papers_o1 = df_papers[df_papers['I']==1]
# print(df_papers_o1.shape)

df_papers_xd = df_papers[df_papers['XD']==1]

df_papers.to_csv('./panel_model_paper_citations_data_all_A.csv')
df_papers_xd.to_csv('./panel_model_paper_citations_data_xd_A.csv')



