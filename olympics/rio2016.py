""" Read and analyze data from the Rio Olympics 2016.

    Data is picked up from the Guardian.
"""

import csv
import datetime
import json
from lxml import etree
import os.path
import re
import requests


def read_country_codes(fname):
    """ Read three-letter country codes. 

        Return a dict of code:country.
    """
    with open(fname) as csvfile:
        data = csv.DictReader(csvfile)
        codes = {}
        for item in data:
            codes[item['Code']] = item['Country']
    return codes


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
    for code in sorted(codes.keys()):
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


def export_medals_by_country(codes):
    """ Export as CSV medals won by country and sport. """
    allmedals = []
    for code in sorted(codes.keys()):
        fname = 'data/rio2016/medals.{:s}.html'.format(code)
        print('Processing {:s} ...'.format(fname))
        try:
            parser = etree.HTMLParser()
            tree = etree.parse(fname, parser)
            root = tree.getroot()

            medals = root.xpath("//li[contains(@class,'om-country-medal-entry')]")
            for medal in medals:
                items = medal.xpath("div[contains(@class,'om-country-timestamp')]")
                if len(items)!=1:
                    print("Multiple items inside a medal entry ... taking first one.")
                timestamp = clean_text(items[0].text)

                items = medal.xpath("div[contains(@class,'om-country-medal')]/span[contains(@class,'om-medal-type')]")
                if len(items)!=1:
                    print("Multiple items inside a medal entry ... taking first one.")
                medaltype = clean_text(items[0].text)

                items = medal.xpath("div[contains(@class,'om-country-eventname')]")
                if len(items)!=1:
                    print("Multiple items inside a medal entry ... taking first one.")
                evtnamefields = [x.strip(" \t\n,") for x in items[0].text.strip().split("\n")]
                if len(evtnamefields)!=2:
                    print("Two fields expected for event name. Got this: {:s}".format(str(evtnamefields)))

                items = medal.xpath("div[contains(@class,'om-recent-medalist')]/span[contains(@class,'om-medalist-name')]")
                if len(items)!=1:
                    print("Multiple items inside a medal entry ... taking first one.")
                medalist = clean_text(items[0].text)

                allmedals.append({
                    'country': codes[code],
                    'countrycode': code,
                    'ts': timestamp,
                    'type': medaltype,
                    'sport': evtnamefields[0],
                    'event': evtnamefields[1],
                    'medalist': medalist})

        except (IOError, OSError):
            print('Unable to open/parse file {:s} ... skipping.'.format(fname))           

    with open('data/rio2016/medalsData.csv', 'w', newline='') as csvfile:
        fieldnames = ['country', 'countrycode', 'ts', 'type', 'sport', 'event', 'medalist']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for medal in allmedals:
            writer.writerow(medal)


def clean_text(txt):
    txt = re.sub(r'\n', '', txt)
    txt = re.sub(r'\s+', ' ', txt).strip()
    return txt


if __name__ == '__main__':
    # Don't really match with ISO codes at https://countrycode.org/
    codes = read_country_codes('data/rio2016/allCountries.Guardian.csv')

    download_data_by_country(codes)

    download_data_by_date()

    export_medals_by_country(codes)

