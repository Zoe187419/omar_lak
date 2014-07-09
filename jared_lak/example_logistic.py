# This code was taken from here:
# http://blog.yhathq.com/posts/logistic-regression-and-python.html
import pandas as pd
pd.set_option('display.height', 1000)
pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

import pandas as pd
import statsmodels.api as sm
import pylab as pl
import numpy as np
 
# read the data in
df = pd.read_csv("../../data_lak_2015/ucla.csv")
 
# take a look at the dataset
print df.head()

# rename the 'rank' column because there is also a DataFrame method called 'rank'
df.columns = ["admit", "gre", "gpa", "prestige"]
#print df.columns
print df.describe()
#print df.std()

# table counts using crosstab
print pd.crosstab(df['admit'], df['prestige'], rownames=['admit'])
print pd.crosstab(df['admit'], df['gre'], rownames=['admit'])
#print pd.crosstab(df['admit'], df['prestige'], rownames=['admit'])

# historgrams
#df.hist()
#pl.show()

dummy_ranks = pd.get_dummies(df['prestige'], prefix='prestige')
print dummy_ranks.head()

cols_to_keep = ['admit', 'gre', 'gpa']
data = df[cols_to_keep].join(dummy_ranks.ix[:, 'prestige_2':])
print data.head()
data['intercept'] = 1.0

train_cols = data.columns[1:]
# Index([gre, gpa, prestige_2, prestige_3, prestige_4], dtype=object)
exit()
 
logit = sm.Logit(data['admit'], data[train_cols])
 
# fit the model
result = logit.fit()

print result.summary()
print result.conf_int()
print data.head()
print np.exp(result.params)

params = result.params
conf = result.conf_int()
conf['OR'] = params
conf.columns = ['2.5%', '97.5%', 'OR']
print np.exp(conf)

gres = np.linspace(data['gre'].min(), data['gre'].max(), 10)
print gres

gpas = np.linspace(data['gpa'].min(), data['gpa'].max(), 10)
print gpas



