import os
import pandas as pd

df_scholars = pd.read_csv('../../data/Faculty_GoogleScholar_Funding_Data_N4190.csv')
df_papers = pd.read_csv('../../data/GoogleScholar_paper_stats.csv', names=['s', 't', 'z', 'coauthors'])

shape_scholars = df_scholars.shape  # (4190, 25)
shape_papers = df_papers.shape      # (424827, 4)

df_papers_2017 = df_papers[df_papers['t'] <= 2017]
print(df_papers_2017.shape)

# print(df_scholars.columns)

# exit()
# Map scholars to dict
scholars = dict()
for index, s_row in df_scholars.iterrows():
    scholars[s_row['google_id']] = s_row

def compute_coauthor(p):
    coauthors = (p.get('coauthors') or '').split(',')
    return len(coauthors) - 1

def compute_career_age(p):
    gid = p['s']
    career_age = int(p['t']) - int(scholars[gid]['min_year']) + 1 if gid in scholars else 1
    return career_age

def compute_paper_orientation(p):
    coauthors = (p.get('coauthors') or '').split(',')
    depts = []
    o = 0
    for a in coauthors:
        if a not in ['0', '1', '2']:
            a_dept = scholars[a]['dept']
            dept_map = {'CS': '1', 'BIO': '0'}
            if dept_map[a_dept] not in depts:
                depts.append(dept_map[a_dept])
        else:
            if a not in depts:
                depts.append(a)
    return 1 if '0' in depts and '1' in depts else 0

def compute_dummy_year(p):
    return 0

# Add new column to process papers data
df_papers['a'] = df_papers.apply(lambda row: compute_coauthor(row), axis=1)
df_papers['tau'] = df_papers.apply(lambda row: compute_career_age(row), axis=1)
df_papers['I'] = df_papers.apply(lambda row: compute_paper_orientation(row), axis=1)
# df_papers['Dt'] = df_papers.apply(lambda row: compute_dummy_year(row), axis=1)

print(df_papers.columns)
# help(pd.read_csv)

df_papers_o1 = df_papers[df_papers['I']==1]
print(df_papers_o1.shape)

df_papers.to_csv('./panel_model_paper_citations_data.csv')
