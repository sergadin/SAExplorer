# -*- coding: utf-8 -*-
'''
Script for loading all DOI records from crossref server.
Be default loads records deposited the day before yesterday.
'''
import urllib.request
import urllib.parse
import urllib.error
import json
import datetime
from datetime import date, timedelta
import time
import logging
import sqlite3
from elasticsearch import Elasticsearch

CROSSREF_ENDPOINT = 'https://api.crossref.org'
CROSSREF_MAILTO = 'serg@msu.ru'

ES_SERVER = 'http://188.44.51.15:9200'
INDEX = 'crossref'
DOCTYPE = 'works'

DATABASE = 'es_doi.sqlite'

logging.basicConfig(filename='doi_fetcher.log', level=logging.WARNING)

'''
CREATE TABLE missingdoi (
    missingdoi_id INTEGER PRIMARY KEY AUTOINCREMENT,
    doi_value VARCHAR(128) -- DOI
);
'''

CREATE_TABLE = '''
CREATE TABLE doi (
    doi_id INTEGER PRIMARY KEY AUTOINCREMENT,
    doi_value VARCHAR(128), -- DOI
    es_docid VARCHAR(128), -- Elasticsearch ID
    doi_deposited DATE, -- When the record was updated at crossref
    doi_updated DATE
);
'''
CREATE_INDEX = '''
CREATE INDEX doi_value ON doi(doi_value);
'''

def fetch_url(url, params):
    '''
    Load specified url with get parameters.
    '''
    post_args = urllib.parse.urlencode(params)
    actual_url = '{base}?{params}'.format(base=url, params=post_args)
    request = urllib.request.Request(actual_url, headers={"Accept" : "application/json"})

    for attempt in range(1, 4):
        timeout = attempt * 10
        try:
            with urllib.request.urlopen(request) as response:
                message_bytes = response.read()
                message = message_bytes.decode('utf-8')
                return message
        except urllib.error.URLError as err:
            logging.warning('Fetch error: %s', err)
            logging.warning('Complete URL: %s', actual_url)
            logging.warning('Waiting for %s seconds before next download attempt.', timeout)
            time.sleep(timeout)
    logging.error('Unable to download URL: %s', actual_url)
    return None


class DOIFetcher:
    '''
    Crossref loader.
    '''
    es = Elasticsearch([ES_SERVER])
    conn = sqlite3.connect(DATABASE)

    def __init__(self):
        if False: # Reset databases
            cursor = self.conn.cursor()
            cursor.execute(CREATE_TABLE)
            cursor.execute(CREATE_INDEX)
            cursor.close()

            self.es.indices.delete(index='crossref')

    def find_docid(self, doi):
        'FInd elasticsearch document id for the given DOI.'
        cursor = self.conn.cursor()
        cursor.execute('SELECT es_docid FROM doi WHERE doi_value = ?', (doi,))
        result = cursor.fetchone()
        cursor.close()
        if result is not None:
            (docid, ) = result
            return docid
        return None

    def max_deposited_date(self):
        cursor = self.conn.cursor()
        cursor.execute("SELECT max(doi_deposited) FROM doi")
        result = cursor.fetchone()
        cursor.close()
        if result is not None:
            (max_date, ) = result
            return max_date
        return None

    def save_item(self, item, cursor):
        'Store single record in both elasticsearch and relational databases.'
        doi = item.get('DOI')
        def get_date():
            for name in ['deposited', 'indexed']:
                try:
                    [year, month, day] = item[name]['date-parts'][0]
                    return datetime.date(year=year, month=month, day=day)
                except KeyError:
                    pass
        deposited_date = get_date()
        docid = self.find_docid(doi)
        res = self.es.index(index=INDEX, doc_type=DOCTYPE, body=item, id=docid)
        docid = res['_id']
        cursor.execute("DELETE FROM doi WHERE doi_value = ? AND es_docid = ?", (doi, docid))
        cursor.execute("INSERT INTO doi (doi_value, es_docid, doi_updated, doi_deposited) " +
                       "VALUES (?, ?, date('now'), ?)",
                       (doi, docid, deposited_date))
        return (doi, docid)


    def process_response_chunk(self, content):
        '''
        Save all records into Elasticsearch and RDBMS. Returns next
        crossref cursor or None, if there are no items in the current
        chunk.
        '''
        data = json.loads(content)
        items_found = False
        next_crossref_cursor = None
        cursor = self.conn.cursor()
        for item in data['message']['items']:
            item_doi = item.get('DOI')
            try:
                self.save_item(item, cursor)
                items_found = True
            except Exception as err:
                logging.error('Error while processing DOI %s', item_doi)
                logging.error('%s', err)
                print(err)
                if item_doi is not None:
                    cursor.execute("INSERT INTO missingdoi (doi_value) VALUES (?)", (item_doi,))
        if items_found:
            cursor.execute("COMMIT")
            next_crossref_cursor = data['message'].get('next-cursor')
        cursor.close()
        return next_crossref_cursor


    def retrieve_new_dois(self, start_date, end_date, initial_cursor='*'):
        '''
        Process all records aupdated between start_date and end_date (inclusive).
        Initial cursor may be supplied to resume suspended loading.
        '''
        chunk_size = 500 # How many records will be retrieved from crossref as a chunk
        def is_a_rerun(threshold=chunk_size):
            cursor = self.conn.cursor()
            cursor.execute("SELECT count(*) FROM doi WHERE doi_deposited >= ? AND doi_deposited <= ?",
                           (start_date, end_date))
            (cnt,) = cursor.fetchone()
            return cnt > threshold

        if is_a_rerun():
            logging.error('DB contains records for the specified time interval [%s, %s]', start_date, end_date)
            return
        return
        crossref_cursor = initial_cursor
        post_params = {
            'filter' : 'from-update-date:{start},until-update-date:{end}'.format(start=start_date, end=end_date),
            'rows': chunk_size,
            'mailto' : CROSSREF_MAILTO,
            'cursor' : crossref_cursor
            }
        url = '{base}/works'.format(base=CROSSREF_ENDPOINT)
        while crossref_cursor is not None:
            post_params['cursor'] = crossref_cursor
            json_response = fetch_url(url, post_params)
            if not json_response:
                break
            crossref_cursor = self.process_response_chunk(json_response)
            logging.info('Next cursor: %s', crossref_cursor)


def main():
    'Main function.'
    today = date.today()
    yesterday = today - timedelta(1)
    the_day_before_yesterday = today - timedelta(2)

    fetcher = DOIFetcher()

    fetcher.retrieve_new_dois(start_date=the_day_before_yesterday, end_date=the_day_before_yesterday)

main()
