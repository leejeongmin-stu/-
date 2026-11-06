"""Build tidy CSVs: US aid to Korea by year (1946-2025) + Korean economy (1946-2000)."""
import json, os, sys
import pandas as pd

RAW, OUT = sys.argv[1], sys.argv[2]
os.makedirs(OUT, exist_ok=True)

# ---------------- US aid (Foreign Aid Explorer / Greenbook, obligations) ----------------
df = pd.read_csv(os.path.join(RAW, 'kor_complete.csv'))
ob = df[df['Transaction Type Name'] == 'Obligations'].copy()
# transition quarter "1976tq" -> counted in 1976
ob['year'] = ob['Fiscal Year'].astype(str).str[:4].astype(int)
ob['amt'] = ob['Current Dollar Amount'] / 1e6
ob['amt_c'] = ob['Constant Dollar Amount'] / 1e6

GROUP = {
    "Gov't & Relief in Occupied Areas (Grants)": ('relief', 'grant'),
    'UNRRA (Post) Interim Aid': ('relief', 'grant'),
    'Civilian Relief in Korea (CRIK)': ('relief', 'grant'),
    'USAID Grants': ('usaid_grant', 'grant'),
    'USAID Loans': ('usaid_loan', 'loan'),
    'Title I Food for Peace Loans ($US)': ('pl480', 'loan'),
    'Title I Food For Peace Loans (LCU)': ('pl480', 'loan'),
    'Title I Food for Peace Grants (LCU)': ('pl480', 'grant'),
    'Title II Emergency Programs': ('pl480', 'grant'),
    'Title II Development Programs': ('pl480', 'grant'),
    'US Surplus Property': ('other_econ', 'grant'),
    'Peace Corps': ('other_econ', 'grant'),
    'ESF': (None, None),  # duplicate label of USAID grants/loans, cancelled by the adjustment row
    'ESF adjustment for USAID Loans and Grants': (None, None),
}
MIL_LOAN = ['Foreign Military Financing (FMF) Program, DOD Guaranty', 'FMF) Program, DOD Direct', 'FMS Loans']

def classify(r):
    a = r['Activity Name']
    if r['Foreign Assistance Objective Name'] == 'Military':
        return ('mil', 'loan' if any(k in a for k in MIL_LOAN) else 'grant')
    if a in GROUP:
        return GROUP[a]
    return ('other_econ', 'grant')  # small modern items (disaster aid etc.)

cls = ob.apply(classify, axis=1, result_type='expand'); ob['grp'], ob['kind'] = cls[0], cls[1]

years = pd.DataFrame({'year': range(1946, 2026)})
def pv(mask, col='amt'):
    s = ob[mask].groupby('year')[col].sum(); return years['year'].map(s).fillna(0.0)

aid = years.copy()
econ = ob['Foreign Assistance Objective Name'] == 'Economic'
mil = ob['Foreign Assistance Objective Name'] == 'Military'
aid['econ_aid'] = pv(econ)
aid['mil_aid'] = pv(mil)
aid['total_aid'] = aid['econ_aid'] + aid['mil_aid']
aid['econ_grant'] = pv(econ & (ob['kind'] == 'grant'))
aid['econ_loan'] = pv(econ & (ob['kind'] == 'loan'))
aid['relief'] = pv(ob['grp'] == 'relief')
aid['usaid_grant'] = pv(ob['grp'] == 'usaid_grant')
aid['usaid_loan'] = pv(ob['grp'] == 'usaid_loan')
aid['pl480'] = pv(ob['grp'] == 'pl480')
aid['mil_grant'] = pv(mil & (ob['kind'] == 'grant'))
aid['mil_loan'] = pv(mil & (ob['kind'] == 'loan'))
aid['econ_aid_2025usd'] = pv(econ, 'amt_c')
aid['total_aid_2025usd'] = pv(econ | mil, 'amt_c')
# sanity: grant+loan == econ
chk = (aid['econ_grant'] + aid['econ_loan'] - aid['econ_aid']).abs().max()
assert chk < 0.05, chk
aid = aid.round(2)
aid.to_csv(os.path.join(OUT, 'us_aid_korea_1946_2025.csv'), index=False, encoding='utf-8-sig')

# ---------------- Korean economy ----------------
mad = pd.read_csv(os.path.join(RAW, 'maddison_kor.csv'))
eco = pd.DataFrame({'year': range(1946, 2001)})
eco['gdppc_2011usd'] = eco['year'].map(mad.set_index('year')['gdppc'])
eco['pop_thousand'] = eco['year'].map(mad.set_index('year')['pop'])
eco['gdp_growth_maddison'] = eco['year'].map(mad.set_index('year')['growth'])
def wb(ind):
    d = json.load(open(os.path.join(RAW, f'wb_{ind}.json')))
    return {int(r['date']): r['value'] for r in d[1] if r['value'] is not None}
for col, ind, scale in [('gdp_growth_wb', 'NY.GDP.MKTP.KD.ZG', 1), ('manuf_share', 'NV.IND.MANF.ZS', 1),
                        ('agri_share', 'NV.AGR.TOTL.ZS', 1), ('export_share', 'NE.EXP.GNFS.ZS', 1),
                        ('export_usd_million', 'NE.EXP.GNFS.CD', 1e-6)]:
    m = wb(ind); eco[col] = eco['year'].map(lambda y: m.get(y) * scale if m.get(y) is not None else None)
eco = eco.round(2)
eco.to_csv(os.path.join(OUT, 'korea_economy_1946_2000.csv'), index=False, encoding='utf-8-sig')

# ---------------- merged 1946-1990 ----------------
mg = aid.merge(eco, on='year', how='inner')
mg = mg[mg['year'] <= 1990]
mg.to_csv(os.path.join(OUT, 'aid_and_economy_1946_1990.csv'), index=False, encoding='utf-8-sig')

# quick facts for the report
e = aid.set_index('year')
facts = {
    'first_year': int(e[e['total_aid'] > 0].index.min()),
    'econ_peak_year': int(e['econ_aid'].idxmax()), 'econ_peak': round(float(e['econ_aid'].max()), 1),
    'last_econ_year_over_1m': int(e[e['econ_aid'] >= 1].index.max()),
    'last_mil_year_over_1m': int(e[e['mil_aid'] >= 1].index.max()),
    'econ_total_1946_1981': round(float(e.loc[1946:1981, 'econ_aid'].sum()), 1),
    'mil_total_1946_1986': round(float(e.loc[1946:1986, 'mil_aid'].sum()), 1),
    'econ_grant_1946_1981': round(float(e.loc[1946:1981, 'econ_grant'].sum()), 1),
    'econ_loan_1946_1981': round(float(e.loc[1946:1981, 'econ_loan'].sum()), 1),
    'first_econ_loan_year': int(e[e['econ_loan'] > 0].index.min()),
    'loan_share_1946_60': round(float(e.loc[1946:1960, 'econ_loan'].sum() / e.loc[1946:1960, 'econ_aid'].sum() * 100), 1),
    'loan_share_1961_70': round(float(e.loc[1961:1970, 'econ_loan'].sum() / e.loc[1961:1970, 'econ_aid'].sum() * 100), 1),
    'loan_share_1971_81': round(float(e.loc[1971:1981, 'econ_loan'].sum() / e.loc[1971:1981, 'econ_aid'].sum() * 100), 1),
}
json.dump(facts, open(os.path.join(OUT, '_facts.json'), 'w'), ensure_ascii=False, indent=1)
print(json.dumps(facts, ensure_ascii=False, indent=1))
print(aid[aid['year'].between(1946, 1985)][['year', 'econ_aid', 'econ_grant', 'econ_loan', 'relief', 'usaid_grant', 'usaid_loan', 'pl480', 'mil_aid']].to_string(index=False))
