""" Analyze data to detect water leaks.

    First argument should be a CSV file
    with header fields ts and value.
"""

import csv
import datetime
import dateutil
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
import sys


def read_data(fname):
    with open(fname) as csvfile:
        data = csv.DictReader(csvfile)
        ts = []
        value = []
        for row in data:
            ts.append(dateutil.parser.parse(row['ts']))
            value.append(float(row['value']))

    return (ts, value)


def preprocess(data):
    ts, value = data

    # Convert to numpy arrays
    ts = np.array(ts)
    value = np.array(value, float)

    # Calculate elapsed time in seconds for gradient calculation
    secs = [(np.datetime64(a)-np.datetime64(ts[0])).item().total_seconds() for a in ts]

    # Use forward difference since np.gradient(y) assumes uniformly spaced x-axis
    gradient = np.append(np.diff(value)/np.diff(secs), 0)

    # Record the rounded hour of data collection
    hour = [(a.hour + (a.minute+30)//60)%24 for a in ts]

    return (ts, value, secs, hour, gradient)


def save_data(data, fname):
    ts, value, secs, hour, gradient = data
    with open(fname, 'w') as csvfile:
        fieldnames = ['ts', 'value', 'secs', 'hour', 'gradient']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for i, tsval in enumerate(ts):
            writer.writerow({'ts': tsval, 'value': value[i], 'secs': secs[i], 'hour': hour[i], 'gradient': gradient[i]})


def plot(data, img_name):
    ts, value, secs, hour, gradient = data

    plt.style.use('ggplot')
    plt.figure(figsize=(10,10))

    axes = plt.gca()
    axes.xaxis.set_major_formatter(mdates.DateFormatter('%d-%m-%Y %H:%M'))

    plt.subplot(3, 1, 1)
    plt.plot(ts, value, 'b')
    plt.title('Water Leakage')
    plt.ylabel('ylabel', fontsize=10)
    plt.ylabel('Water Consumption (litres)')

    plt.subplot(3, 1, 2)
    plt.plot(ts, gradient, 'g')
    plt.ylabel('ylabel', fontsize=10)
    plt.ylabel('Rate of Consumption (litres/sec)')
    plt.xlabel('xlabel', fontsize=10)
    plt.xlabel('Date/Time')

    plt.subplot(3, 1, 3)
    plt.scatter(hour, gradient, c='g')
    plt.ylabel('ylabel', fontsize=10)
    plt.ylabel('Time of Day Rate of Consumption (litres/sec)')
    plt.xlabel('xlabel', fontsize=10)
    plt.xlabel('Hour')

    plt.savefig(img_name)


if __name__ == '__main__':
    csvfile = sys.argv[1]
    data = preprocess(read_data(csvfile))
    save_data(data, 'alldetails.csv')
    plot(data, 'leakages.png')

