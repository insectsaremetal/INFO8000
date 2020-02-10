# Code for web app
from flask import Flask, escape, request, jsonify

app = Flask(__name__)

@app.route('/', methods=['GET', 'POST'])
def hello():
    name = request.args.get('name', 'World')
    response = {"name":name}
    return jsonify(response)
