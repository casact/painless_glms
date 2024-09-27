import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
import re

def fetch_triangle(line, url_stem = 'https://www.pirategrunt.com/paw/'):
    
    url = url_stem + line + '_pos.csv'
    df_out = pd.read_csv(url)
    df_out['line'] = line

    df_out.columns = [re.sub(r'(?<!^)(?=[A-Z])', '_', col).lower() for col in df_out.columns]

    return df_out

def augment_triangle(df):
    
    # Group by accident_year and sort by lag within each group
    df = df.sort_values(by=['accident_year', 'lag'])
    
    # Mutate columns
    df = df.assign(
        lag_factor = df['lag'].astype('category'),
        ay_factor = df['accident_year'].astype('category'),
        prior_paid = df.groupby('accident_year')['cumulative_paid'].shift(1),
        incremental_paid = lambda x: x['cumulative_paid'] - x['prior_paid'],
    )
    
    df['incremental_paid'] = df['incremental_paid'].fillna(df['cumulative_paid'])
    df['ldf_paid'] = df['incremental_paid'] / df['prior_paid']
    df['upper'] = df['development_year'] <= 1997
    
    return df

df_taylor_mcguire = fetch_triangle('wkcomp')
df_taylor_mcguire = augment_triangle(df_taylor_mcguire)
df_taylor_mcguire = df_taylor_mcguire[df_taylor_mcguire['group_code'] == 7080]

df_upper = df_taylor_mcguire[df_taylor_mcguire['upper']]

# Create the plot
plt.figure(figsize = (10, 6))
sns.scatterplot(data = df_upper, x='lag_factor', y='cumulative_paid', hue='ay_factor', palette='viridis')
sns.lineplot(data = df_upper, x='lag_factor', y='cumulative_paid', hue='ay_factor', palette='viridis', legend=False)

# Customize the plot
plt.title("Cumulative paid loss development")
plt.xlabel("Development lag")
plt.ylabel("Cumulative paid loss")
plt.gca().yaxis.set_major_formatter(plt.FuncFormatter(lambda x, loc: "{:,}".format(int(x))))
plt.legend(title='Accident Year', loc='upper right')
plt.show()

df_model = df_taylor_mcguire.query('upper & lag > 1')

model_one = smf.glm(
    formula = 'cumulative_paid ~ 0 + prior_paid:lag_factor',
    data = df_model,
    family = sm.families.Gaussian()
).fit()

print(model_one.summary())
