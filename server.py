#!/usr/bin/python

import simplejson, sys, time
from subprocess import check_output
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

MAX_SECONDS_PER_MOVE = 2.0

global engine_path
global moves
moves = []

class MyHandler(BaseHTTPRequestHandler):

    def do_GET(self):
        raise Exception("Not Supported")

    def do_POST(self):
        self.rfile.readline() # Skip header
        post_body = self.rfile.readline()
        data = simplejson.loads(post_body)

        move = get_move(data)  

        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()

        self.wfile.write('{"move":%d}' % move)

def get_move(data):
    set_prev_moves(data)
    move = find_best_move()
    moves.append(move)
    return move

def score(move, depth = 0):
    global moves
    args = [engine_path, depth] + moves + [move]
    out = check_output(map(str, args)) 
    assert(out.count("\n") == 1) 
    return float(out)    

def set_prev_moves(data):
    global moves
    if data["moveNumber"] < 2:
        moves = []
    if data["moveNumber"] > 0:
        moves.append(data["lastMove"]["col"])

    print " ".join(str(move) for move in moves)

def find_best_move():
    global moves
    if len(moves) == 0:
        return 3 # Known best initial move for player 1

    start = time.time()
    halfway = start + (MAX_SECONDS_PER_MOVE/2)
    end = start + MAX_SECONDS_PER_MOVE

    depth = 0
    search_order = [3,2,4,1,5,0,6]

    while True:
        scores = {}
        for move in search_order:
            scores[move] = score(move, depth)
            if time.time() > end or scores[move] >= 1: # Definite win
                break

        search_order.sort(key = lambda x: scores[x] if x in scores else -1, reverse = True) # Sort by score descending

        max_score = scores[search_order[0]]
        if time.time() > halfway or max_score >= 1 or max_score <= -1: # Definite win or definite loss
            print "in %.2fs @ depth %d got %s" % (time.time()-start, depth, scores)
            break

        depth += 1

    return search_order[0] # Best scoring move

def serve(port):
    try:
        server = HTTPServer(('',port), MyHandler)
        print 'Starting server...'
        server.serve_forever()
    except KeyboardInterrupt:
        print '^C received, shutting down server...'
        server.socket.close()

def validate_engine(engine):
    global engine_path
    engine_path = engine

    try:
        score(3)
    except:
        print "Engine executable at %s does not exist or is not valid" % engine
        exit(-1)

if __name__ == '__main__':
    try:
        engine = sys.argv[1]
        port = int(sys.argv[2])
    except:
        print "Usage: server.py <path_to_engine> <port>"    
        exit(-1)

    validate_engine(engine)

    serve(port)

