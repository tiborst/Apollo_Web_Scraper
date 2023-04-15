#!/opt/miniconda3/envs/ApolloPy_db/bin/python

import psycopg2
from config import config

def connect():
    """ Connect to PostgreSQL database server """
    
    conn = None
    try:
        # read conn params
        params = config()

        # connect
        print('Connecting to PostgreSQL database...')
        conn = psycopg2.connect(**params)

        # create cursor
        cursor = conn.cursor()

        # execute version statement
        print('PostgreSQL database version:')
        cursor.execute('SELECT version()')

        # display server version
        db_version = cursor.fetchone()
        print(db_version)

        # close cursor / communcation with server
        cursor.close()

    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    
    finally:
        if conn is not None:
            
            # close connection 
            conn.close()
            print('Database connection closed.')

if __name__ == '__main__':
    connect()