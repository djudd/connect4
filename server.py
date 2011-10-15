#!/usr/bin/python

import simplejson, sys
from subprocess import check_output
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

C_EXECUTABLE = "/home/david/ConnectFour/src/c4"

global moves
moves = []

class MyHandler(BaseHTTPRequestHandler):

    def do_GET(self):
        raise Exception("Not Supported")

    def do_POST(self):
        self.rfile.readline() # skip header
        post_body = self.rfile.readline()
        data = simplejson.loads(post_body)

        move = get_move(data)  

        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()

        self.wfile.write('{"move":%d}' % move)

def score(move):
    global moves
    f = open(C_EXECUTABLE, "r")
    out = check_output(map(str, [C_EXECUTABLE, 10] + moves + [move]))  
    return float(out)    

def get_move(data):
    global moves
    if data["moveNumber"] < 2:
        moves = []
    if data["moveNumber"] > 0:
        moves.append(data["lastMove"]["col"])

    print " ".join(str(move) for move in moves)
    scores = map(score, range(7))    
    print scores
    
    move = max(range(7), key=lambda x: scores[x])
    moves.append(move)
    return move

def serve(port):
    try:
        server = HTTPServer(('',port), MyHandler)
        print 'Starting server...'
        server.serve_forever()
    except KeyboardInterrupt:
        print '^C received, shutting down server...'
        server.socket.close()

if __name__ == '__main__':
    try:
        port = int(sys.argv[1])
    except:
        print "Usage: server.py <port>"    
        exit(-1)

    serve(port)

