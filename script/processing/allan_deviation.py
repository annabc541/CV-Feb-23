# -*- coding: utf-8 -*-
"""
Created on Thu Mar 21 15:45:11 2024

@author: anna_
"""
import pandas as pd
import allantools
import matplotlib.pyplot as plt
import numpy as np

#reading in all the zeroes
hono_zeroes = pd.read_csv(r"C:/Users/anna_/Documents/Cape Verde/peroxy_campaign/allan_test.csv")
hono_zeroes.index = pd.to_datetime(hono_zeroes['date'],format = '%Y-%m-%d %H:%M:%S')
plt.plot(hono_zeroes['ch1'])

#allan deviation for all the zereos
fig, ax = plt.subplots()
t = np.logspace(1, 3, 1000)
r = 0.1
(t2, ad, ade, adn) = allantools.oadev(np.array(hono_zeroes['ch1']), rate=r, data_type="freq", taus=None)
ax.plot(t2, ad)
ax.set_yscale('log')
ax.set_xscale('log')
ax.set_ylabel("1 sigma allan deviation (ppt)", fontsize=16)
ax.set_xlabel("Averaging time (seconds)", fontsize=16)

#looking at individual zero cycles
hono_zero = hono_zeroes[hono_zeroes['id'] == 13]['ch2']
plt.plot(hono_zero)

#allan deviation for individual zero cycles
fig, ax = plt.subplots()
t = np.logspace(1, 3, 1000)
r = 0.1
(t2, ad, ade, adn) = allantools.oadev(np.array(hono_zero), rate=r, data_type="freq", taus=None)
ax.plot(t2, ad)
ax.set_yscale('log')
ax.set_xscale('log')
ax.set_ylabel("1 sigma allan deviation (ppt)", fontsize=16)
ax.set_xlabel("Averaging time (seconds)", fontsize=16)
ax.annotate('10 s adev = %.1f ppt' % ad[99], xycoords='axes fraction', xy=(0.1,0.1), fontsize=12)
ax.axes.tick_params(axis='both', labelsize=14)

#x_arr = np.arange(0.1, 500)
poisson = ad[0] / np.sqrt(10 * t2)
ax.plot(t2, poisson, linestyle='dashed')
ax.legend(['Allan deviation of 16 minute zero', 'Poisson limit'], fontsize=14)


start_date = '2022-01-01 00:00:00'
num_steps = 1716

#no_zeroes = nox_zeroes[nox_zeroes['id'] != 16 | nox_zeroes['id'] != 25]['no']
no_zeroes = nox_zeroes[~nox_zeroes['id'].isin([1,3,16,25])]['no']
no_zeroes = no_zeroes.reset_index(drop = True)
no_zeroes.index = pd.date_range(start = start_date,periods=num_steps,freq = '10S')
plt.plot(nox_zeroes)