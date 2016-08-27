"""Analyze data to detect water leaks."""

import csv
import dateutil
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np


def read_data(fname):
    with open(fname) as csvfile:
        data = csv.DictReader(csvfile)
        x = []
        y = []
        i = 0
        for row in data:
            i += 1
            if i>1200: break
            x.append(dateutil.parser.parse(row['ts']))
            y.append(float(row['data']))

    return (x, y)


def preprocess(data):
    x, y = data

    # Convert to numpy arrays
    x = np.array(x)
    y = np.array(y, float)

    # Calculate elapsed time in seconds for gradient calculation
    xsecs = [(np.datetime64(a)-np.datetime64(x[0])).item().total_seconds() for a in x]

    # Use forward difference since np.gradient(y) assumes uniformly spaced x-axis
    gradient = np.append(np.diff(y)/np.diff(xsecs), 0)

    return (x, y, gradient)


def save_data(data, fname):
    x, y, gradient = data
    with open(fname, 'w') as csvfile:
        fieldnames = ['ts', 'val']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for ts, val in zip(x, gradient):
            writer.writerow({'ts': ts, 'val': val})


def plot(data, img_name):
    x, y, gradient = data

    plt.style.use('ggplot')
    plt.figure(figsize=(10,10))

    axes = plt.gca()
    axes.xaxis.set_major_formatter(mdates.DateFormatter('%d-%m-%Y %H:%M'))

    plt.subplot(2, 1, 1)
    plt.plot(x, y, 'b')
    plt.title('Water Leakage')
    plt.ylabel('ylabel', fontsize=10)
    plt.ylabel('Water Consumption (litres)')

    plt.subplot(2, 1, 2)
    plt.plot(x, gradient, 'g')
    plt.ylabel('ylabel', fontsize=10)
    plt.ylabel('Rate of Consumption (litres/sec)')
    plt.xlabel('xlabel', fontsize=10)
    plt.xlabel('Date/Time')

    plt.savefig(img_name)


if __name__ == '__main__':
    data = preprocess(read_data('wisight.aws.water.vs17.csv'))
    save_data(data, 'leakdata.csv')
    plot(data, 'leakages.png')

