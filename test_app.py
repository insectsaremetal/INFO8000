# I could not get the below code to work. 
# I attempted a tutorial I found online, but I created my own database.  This is in files labeled Attempt 2.
# I was going to build on this and create a new one using my entertainment industry data, but realized ...
# This was via a local host and is only assessible when the connection is open. 

from flask import Flask, escape, request, jsonify, make_response
import sqlite3
from flask import g
from flask_httpauth import HTTPBasicAuth


app = Flask(__name__)

auth = HTTPBasicAuth()

USER_DATA = {
    "star": "entertainmentindustries"
}

DATABASE = '/home/munro_holly/test_app/entertainmentindustries.db'

def connect_db():
    return sqlite3.connect(DATABASE)

def query_db(query, args=(), one=False):
    cur = g.db.execute(query, args)
    rv = [dict((cur.description[idx][0], value)
               for idx, value in enumerate(row)) for row in cur.fetchall()]
    return (rv[0] if rv else None) if one else rv

@app.route('/', methods=['GET'])

def home():
    return "<h1>Hi<h1>"

@app.route('/class')

def hello():
    name = request.args.get("name", "World")
    response = {"name":name + " Hello"}
    return jsonify(response)
