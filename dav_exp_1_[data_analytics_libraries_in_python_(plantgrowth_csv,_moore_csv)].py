# -*- coding: utf-8 -*-
"""DAV Exp 1 [Data Analytics Libraries in Python (PlantGrowth.csv, Moore.csv)].ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1j_hVflkVKaPr71Lzv6WmsWfLHIzwsSTx

# Central Tendency, Variability, and Distribution Shape
"""

import warnings
warnings.filterwarnings('ignore')
import pandas as pd
import numpy as np
import math
import statistics
from scipy import stats
import scipy
from scipy.stats import skew
import statsmodels.api as sm
from statsmodels.formula.api import ols
from matplotlib import pyplot as plt

x=[5, 5, 4, 12, -10, 0, 32, 4, 3, 4, 6, 7, 2]

print(np.mean(x))
print(np.median(x))

stats.mode(x)

q1 = np.percentile(x, 25)
q2 = np.percentile(x, 50)
q3 = np.percentile(x, 75)
q1, q2, q3

IQR = q3-q1
IQR

np.var(x)

statistics.pstdev(x)

np.std(x)

skew(x)

"""# z-test"""

def z_and_p(x1, x2, s1, s2, n1, n2):
    z = (x1-x2)/(math.sqrt(((s1**2)/n1)+((s2**2)/n2)))
    if(z<0):
        p = stats.norm.cdf(z)
    else:
        p = 1-stats.norm.cdf(z)
    print(z, p)

z_and_p(121, 112, 8, 8, 10, 10)

"""# t-test"""

def samplesize(a, b, mu1, mu2, s):
    z1 = -1*stats.norm.ppf(a)
    z2 = -1*stats.norm.ppf(b)
    n = (((z1+z2**2)*(s**2))/((mu1-mu2)**2))
    print(n)

samplesize(0.05, 0.1, 12, 12.75, 3.2)

def two_sample_proportion(p1, p2, n1, n2):
    p_pool = ((p1*n1)*(p2*n2))/(n1*n2)
    s_sq = (p_pool*(1-p_pool)*((1/n1)*(1/n2)))
    s = math.sqrt(s_sq)
    z = -(p1-p2)/s
    if(z>0):
        p_val = stats.norm.cdf(z)
    else:
        p_val = 1-stats.norm.cdf(z)
    return z, p_val*2

two_sample_proportion(0.27, 0.19, 100, 100)

scipy.stats.f.ppf(q=1-0.05, dfn=15, dfd=10)

scipy.stats.f.ppf(q=0.05, dfn=15, dfd=10)

X = [3, 7, 25, 10, 15, 6, 12, 25, 15, 7]
Y = [48, 44, 40, 38, 33, 21, 20, 12, 1, 18]

F = np.var(X)/np.var(Y)
dfn = len(X)-1
dfd = len(Y)-1

p_value = scipy.stats.f.cdf(F, dfn, dfd)
print(p_value)

"""# ANOVA test"""

data = pd.read_csv("PlantGrowth.csv") # To use StatsModel's in-built library, use [data = sm.datasets.get_rdataset("PlantGrowth").data]
print(data.head())

model = ols('weight ~ C(group)', data=data).fit()
anova_table = sm.stats.anova_lm(model, typ=2)
print(anova_table)

data = pd.read_csv("Moore.csv") # To use StatsModel's in-built library, use [data = sm.datasets.get_rdataset("Moore").data]
data = data.rename(columns={"partner.status": "partner_status"})

model = ols('conformity ~ C(fcategory)*C(partner_status)', data=data).fit()
anova_table = sm.stats.anova_lm(model, typ=2)
print(anova_table)