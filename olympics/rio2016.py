""" Read and analyze data from the Rio Olympics 2016.

    Data is picked up from the Guardian.
"""

import csv
import datetime
import json
import re
import requests


def read_country_codes(fname):
    """ Read three-letter country codes. """
    with open(fname) as csvfile:
        data = csv.DictReader(csvfile)
        return sorted(x['Code'] for x in data)


def download_data_by_date(start_date = "2016-08-03", end_date = "2016-08-21"):
    """ Download schedule and results by date. """
    # Convert from str to datetime and make the list of dates
    sdate = datetime.datetime.strptime(start_date, "%Y-%m-%d")
    edate = datetime.datetime.strptime(end_date, "%Y-%m-%d")
    numdays = (edate-sdate).days + 1
    dates = [sdate + datetime.timedelta(days=x) for x in range(0, numdays)]

    for dt in dates:
        currdt = dt.strftime("%Y-%m-%d")
        print('Fetching data for {:s} ... '.format(currdt))
        urls = (('Schedule', 'https://interactive.guim.co.uk/2016/09/olympics-2016/ubuntu/days/schedule-results-{:s}.html'),
                ('Results', 'https://interactive.guim.co.uk/2016/09/olympics-2016/ubuntu/days/results-{:s}.json'))
        for url in urls:
            print('  {:s} ... '.format(url[0], currdt), end='')
            r = requests.get(url[1].format(currdt))

            if r.status_code != 200:
                print('FAILED')
                break

            if 'application/json' in r.headers['content-type']:
                rspdata = r.json()
                fname = 'data/rio2016/results.{:s}.json'.format(currdt)
                with open(fname, 'w') as of:
                    json.dump(rspdata, of)
            else:
                rspdata = r.text
                fname = 'data/rio2016/schedule.{:s}.html'.format(currdt)
                with open(fname, 'w') as of:
                    of.write(rspdata)

            print('OK')


def download_data_by_country(codes):
    """ Download medals won by country. """
    for code in codes:
        print('Fetching data for {:s} ... '.format(code), end='')
        url = 'https://interactive.guim.co.uk/2016/09/olympics-2016/ubuntu/countries/medals/countryMedals-{:s}.html'.format(code)
        r = requests.get(url)

        if r.status_code != 200:
            print('FAILED')
            continue

        rspdata = r.text
        fname = 'data/rio2016/medals.{:s}.html'.format(code)
        with open(fname, 'w') as of:
            of.write(rspdata)

        print('OK')


if __name__ == '__main__':
    # This is done only for countries with medals
    # Don't really match with ISO codes at https://countrycode.org/
    codes = read_country_codes('data/rio2016/medalsTable.csv')

    # download_data_by_country(codes)

    # download_data_by_date()

