#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <sys/time.h>

#define i64 unsigned long long
#define i32 unsigned long
#define i8 signed char

#define Player signed char
#define P1 1
#define P2 (-1)

#define NUM_ROWS 6
#define NUM_COLS 7

#define Depth short

#define Score float
#define SCORE_MAX 1
#define SCORE_INVALID -2 // ensure lower score than a loss

// can't use enum because need negatives
#define Owner signed char
#define OWNER_PLAYER 4
#define OWNER_OPPONENT -4
#define OWNER_NONE_NOT_PLAYABLE 0
#define OWNER_NONE_PLAYABLE 1
#define OWNER_PLAYER_ALL_3 12
#define OWNER_OPPONENT_ALL_3 -12
#define OWNER_PLAYER_2_NOT_PLAYABLE_1 8
#define OWNER_PLAYER_2_PLAYABLE_1 9

typedef enum outcome {
    OUTCOME_FORCE_0 = 0,
    OUTCOME_FORCE_1 = 1,
    OUTCOME_FORCE_2 = 2,
    OUTCOME_FORCE_3 = 3,
    OUTCOME_FORCE_4 = 4,
    OUTCOME_FORCE_5 = 5,
    OUTCOME_FORCE_6 = 6,
    OUTCOME_WIN = 7,
    OUTCOME_LOSS = 8,
    OUTCOME_UNKNOWN = 9
} Outcome;

bool isForce(Outcome outcome) {
    return outcome < OUTCOME_WIN;
}

typedef struct state {
    i64 p1_owned_bitfield;
    i64 p2_owned_bitfield;
    i8 p1_threats;
    i8 p2_threats;
    i32 playable_ys;
    Outcome outcome;
} State;

i64 bit(i8 x, i8 y) {
    return ((i64) 1) << ((y*8) + x);
}

bool isOwned(i64 bitfield, i8 x, i8 y) {
    return bitfield & bit(x, y);
}

i64 setOwned(i64 bitfield, i8 x, i8 y) {
    return bitfield | bit(x, y);
}

i8 getPlayableY(i32 playable_ys, i8 x) {
    i32 pow16 = ((i32) 16) << (4*x);
    return (playable_ys / pow16) % 16;
}

i32 incrementPlayableY(i32 playable_ys, i8 x) {
    i32 pow16 = ((i32) 16) << (4*x);
    return playable_ys + pow16;
}

// see http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
i8 bitsSet(i64 v) {
    v = v - ((v >> 1) & (i64)~(i64)0/3);                            // temp
    v = (v & (i64)~(i64)0/15*3) + ((v >> 2) & (i64)~(i64)0/15*3);   // temp
    v = (v + (v >> 4)) & (i64)~(i64)0/255*15;                       // temp
    v = (i64)(v * ((i64)~(i64)0/255)) >> (sizeof(i64) - 1) * 8;     // count
    return (i8) v;
}

long currentTimeMillis() {
    struct timeval now;
    gettimeofday(&now, NULL);
    return (now.tv_sec*1000) + (now.tv_usec/1000);
}

void printBoard(State* state) {
    for (int i=5; i>=0; i--) {
        for (int j=0; j<7; j++) {
            if (isOwned(state->p1_owned_bitfield, j, i))
                fprintf(stderr, "1 ");
            else if (isOwned(state->p2_owned_bitfield, j, i))
                fprintf(stderr,"2 ");
            else
                fprintf(stderr,"0 ");
        }
        fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");
}

void printDebuggingInfo(Player player, i8 x, i8 y, State* parent, State* child, Score res) {
    i8 tops[7];
    for (i8 i=0; i<7; i++) {
        tops[i] = getPlayableY(child->playable_ys, i);
    }

    i8 p2_moves = bitsSet(child->p2_owned_bitfield);

//    printBoard(parent);
    printBoard(child);

    fprintf(stderr,"playable Ys: %d %d %d %d %d %d %d\t", tops[0], tops[1], tops[2], tops[3], tops[4], tops[5], tops[6]);
    fprintf(stderr,"player: %d\tx: %d\ty: %d\tp1 moves: %d\tp2 moves: %d\n", player, x, y, bitsSet(child->p1_owned_bitfield), p2_moves);
    fprintf(stderr,"p1 threats=%d; p2 threats=%d; outcome=%d; score=%f\n", child->p1_threats, child->p2_threats, child->outcome, res);
    fprintf(stderr,"\n\n");
}

#define MIN_DEPTH_FOR_TIME_CUTOFF 4

#define CacheIdx unsigned int
#define CACHE_CAPACITY 262144 // 2^19 - needs to be power of two
#define MAX_CACHE_LOAD 183500 // ~0.7*CACHE_CAPACITY
#define TARGET_CACHE_LOAD 131072 // ~0.5*CACHE_CAPACITY
#define MIN_DEPTH_TO_CACHE 3

typedef enum cacheflags {
    EXACT = 1,
    AT_MOST = 2,
    AT_LEAST = 4
} CacheFlags;

typedef struct cacheval {
    i64 p1_bits;
    i64 p2_bits;
    Score score;
    struct cacheval* nextUsed;
    struct cacheval* prevUsed;
    CacheFlags flags;
    Depth depth;
    Depth start_depth;
    i8 best_move;
} CacheVal;

Depth startDepth;
CacheIdx cacheSize;
CacheVal** cache;
CacheVal* mostRecentlyUsed;

i32 hash(i64 key) {
    // See http://www.concentric.net/~ttwang/tech/inthash.htm
    key = (~key) + (key << 18);
    key = key ^ (key >> 31);
    key = key * 21;
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return key;
}

bool isCacheValSet(CacheVal* val) {
    return val->flags > 0;
}

bool cacheValMatches(CacheVal* val, i64 p1_bits, i64 p2_bits) {
    return (val->p1_bits == p1_bits) && (val->p2_bits == p2_bits);
}

// We hash based only on which cells are owned, not who owns them
CacheIdx initialAddress(i64 p1_bits, i64 p2_bits) {
    return hash(p1_bits ^ p2_bits) & (CACHE_CAPACITY - 1);
}

void setCacheValBlank(CacheVal* val) {
    val->score = SCORE_INVALID;
    val->prevUsed = NULL;
    val->nextUsed = NULL;
    val->p1_bits = -1;
    val->p2_bits = -1;
    val->flags = 0;
    val->depth = 0;
    val->start_depth = startDepth;
    val->best_move = -1;
}

void removeValFromUsedList(CacheVal* val) {
    CacheVal* oldPrev = val->prevUsed;
    CacheVal* oldNext = val->nextUsed;

    assert(oldPrev == NULL || oldPrev != oldNext);

    if (oldNext != NULL)
        oldNext->prevUsed = oldPrev;
    if (oldPrev != NULL)
        oldPrev->nextUsed = oldNext;
}

void addValToFrontOfUsedList(CacheVal* val) {
    assert(val != mostRecentlyUsed);

    val->nextUsed = NULL;
    val->prevUsed = mostRecentlyUsed;
    if (mostRecentlyUsed != NULL)
        mostRecentlyUsed->nextUsed = val;
    mostRecentlyUsed = val;
}

void recordCacheValUse(CacheVal* val) {
    if (val == mostRecentlyUsed)
        return;

    removeValFromUsedList(val);
    addValToFrontOfUsedList(val);
}

CacheVal* getLeastRecentlyUsed() {
    CacheVal* val = mostRecentlyUsed;
    while (val->prevUsed != NULL) {
        assert(val != val->prevUsed);
        val = val->prevUsed;
    }
    return val;
}

void cleanCacheBelowDepth(Depth depth) {
    assert(cacheSize >= TARGET_CACHE_LOAD);

    CacheVal* val = getLeastRecentlyUsed();

    while (cacheSize > TARGET_CACHE_LOAD && val != NULL) {
        CacheVal* next = val->nextUsed;
        if (val->depth <= depth) {
            removeValFromUsedList(val);
            setCacheValBlank(val);
            cacheSize--;
        }
        val = next;
    }
}

void cleanCache() {
    fprintf(stderr, "Cleaning cache...\n");

    assert(cacheSize >= MAX_CACHE_LOAD);

    Depth depth = MIN_DEPTH_TO_CACHE;
    do {
        cleanCacheBelowDepth(depth);
        depth++;
    } while (cacheSize > TARGET_CACHE_LOAD);
}

CacheVal* getCacheValue(i64 p1_bits, i64 p2_bits) {
    CacheIdx addr = initialAddress(p1_bits, p2_bits);
    CacheVal* val = cache[addr];
    while (isCacheValSet(val) && !cacheValMatches(val, p1_bits, p2_bits)) {
        addr = (addr+1) & (CACHE_CAPACITY - 1);
        val = cache[addr];
    }
    return val;
}

void setCacheValue(i64 p1_bits, i64 p2_bits, Score score, CacheFlags flags, Depth depth, i8 best_move) {
    CacheVal* val = getCacheValue(p1_bits, p2_bits);

    assert(isCacheValSet(val) == cacheValMatches(val, p1_bits, p2_bits));

    val->p1_bits = p1_bits;
    val->p2_bits = p2_bits;
    val->score = score;
    val->flags = flags;
    val->depth = depth;
    val->start_depth = startDepth;
    val->best_move = best_move;

    recordCacheValUse(val);

    cacheSize++;

    assert(val->score == getCacheValue(p1_bits, p2_bits)->score);

    if (cacheSize > MAX_CACHE_LOAD)
        cleanCache();
}

CacheVal* checkCache(i64 p1_bits, i64 p2_bits) {
    CacheVal* val = getCacheValue(p1_bits, p2_bits);

    assert(isCacheValSet(val) == cacheValMatches(val, p1_bits, p2_bits));

    if (isCacheValSet(val))
        recordCacheValUse(val);

    return val;
}

Owner owner(i64 p_owned_bitfield, i64 o_owned_bitfield, i32 playable_ys, i8 x, i8 y) {
    if (isOwned(p_owned_bitfield, x, y))
        return OWNER_PLAYER;
    else if (isOwned(o_owned_bitfield, x, y))
        return OWNER_OPPONENT;
    else if (y == getPlayableY(playable_ys, x))
        return OWNER_NONE_PLAYABLE;
    else
        return OWNER_NONE_NOT_PLAYABLE;
}

i8 findPlayableCol(i8 x, i8 offset, Owner o1, Owner o2, Owner o3) {
    if (o1 == OWNER_NONE_PLAYABLE)
        return offset > 0 ? x-offset : x+1-offset;
    else if (o2 == OWNER_NONE_PLAYABLE)
        return offset > 1 ? x+1-offset : x+2-offset;
    else
        return offset > 2 ? x+2-offset : x+3-offset;
}

void incrementThreats(
    i8 x,
    i8 offset,
    Owner o1,
    Owner o2,
    Owner o3,
    i8 *player_connect3s,
    i8 *player_2_none_1s,
    i8 *opponent_connect3s,
    i8 *force_cols_bitfield
) {
    Owner sum = o1+o2+o3;

    if (sum == OWNER_PLAYER_ALL_3) {
        (*player_connect3s)++;
    }
    else if (sum == OWNER_PLAYER_2_NOT_PLAYABLE_1) {
        (*player_2_none_1s)++;
    }
    else if (sum == OWNER_PLAYER_2_PLAYABLE_1) {
        (*player_2_none_1s)++;

        i8 playable_col = findPlayableCol(x, offset, o1, o2, o3);
        i8 force_already_found_here = (*force_cols_bitfield) & (1 << playable_col);
        if (!force_already_found_here) {
            (*force_cols_bitfield) |= (1 << playable_col);
        }
    }
    else if (sum == OWNER_OPPONENT_ALL_3) {
        (*opponent_connect3s)++;
    }
}

#define PROCESS_CELL(cell, i, j) cell = owner(p_owned_bitfield, o_owned_bitfield, playable_ys, x+(i), y+(j))
#define PROCESS_LINE(i, a, b, c) incrementThreats(x, i, a, b, c, player_connect3s, player_2_none_1s, opponent_connect3s, force_cols_bitfield)

void countThreats(
    i64 p_owned_bitfield,
    i64 o_owned_bitfield,
    i32 playable_ys,
    i8 x,
    i8 y,
    i8 *player_connect3s,
    i8 *player_2_none_1s,
    i8 *opponent_connect3s,
    i8 *force_cols_bitfield
) {
    Owner ul_1, ul_2, ul_3, l_1, l_2, l_3, bl_1, bl_2, bl_3, b_1, b_2, b_3, br_1, br_2, br_3, r_1, r_2, r_3, ur_1, ur_2, ur_3;

    if (x >= 1 && y < (NUM_ROWS-1)) {
        PROCESS_CELL(ul_1, -1, 1);
        if (x >= 2 && y < (NUM_ROWS-2)) {
            PROCESS_CELL(ul_2, -2, 2);
            if (x >= 3 && y < (NUM_ROWS-3)) {
                PROCESS_CELL(ul_3, -3, 3);
                PROCESS_LINE(3, ul_3, ul_2, ul_1);
            }
        }
    }

    if (x < (NUM_COLS-1) && y >= 1) {
        PROCESS_CELL(br_1, 1, -1);
        if (x < (NUM_COLS-2) && y >= 2) {
            PROCESS_CELL(br_2, 2, -2);
            if (x < (NUM_COLS-3) && y >= 3) {
                PROCESS_CELL(br_3, 3, -3);
                PROCESS_LINE(0, br_1, br_2, br_3);
            }

            if (x >= 1 && y < (NUM_ROWS-1)) {
                PROCESS_LINE(1, ul_1, br_1, br_2);
            }
        }

        if (x >= 2 && y < (NUM_ROWS-2)) {
             PROCESS_LINE(2, ul_2, ul_1, br_1);
        }
    }

    if (x >= 1 && y >= 1) {
        PROCESS_CELL(bl_1, -1, -1);
        if (x >= 2 && y >= 2) {
            PROCESS_CELL(bl_2, -2, -2);
            if (x >= 3 && y >= 3) {
                PROCESS_CELL(bl_3, -3, -3);
                PROCESS_LINE(3, bl_3, bl_2, bl_1);
            }

        }
    }

    if (x < (NUM_COLS-1) && y < (NUM_ROWS-1)) {
        PROCESS_CELL(ur_1, 1, 1);
        if (x < (NUM_COLS-2) && y < (NUM_ROWS-2)) {
            PROCESS_CELL(ur_2, 2, 2);
            if (x < (NUM_COLS-3) && y < (NUM_ROWS-3)) {
                PROCESS_CELL(ur_3, 3, 3);
                PROCESS_LINE(0, ur_1, ur_2, ur_3);
            }

            if (x >= 1 && y >= 1) {
                 PROCESS_LINE(1, bl_1, ur_1, ur_2);
            }
        }

        if (x >= 2 && y >= 2) {
            PROCESS_LINE(2, bl_2, bl_1, ur_1);
        }
    }

    if (x >= 1) {
        PROCESS_CELL(l_1, -1, 0);
        if (x >= 2) {
            PROCESS_CELL(l_2, -2, 0);
            if (x >= 3) {
                PROCESS_CELL(l_3, -3, 0);
                PROCESS_LINE(3, l_3, l_2, l_1);
            }
        }
    }

    if (x < (NUM_COLS-1)) {
        PROCESS_CELL(r_1, 1, 0);
        if (x < (NUM_COLS-2)) {
            PROCESS_CELL(r_2, 2, 0);
            if (x < (NUM_COLS-3)) {
                PROCESS_CELL(r_3, 3, 0);
                PROCESS_LINE(0, r_1, r_2, r_3);
            }

            if (x >= 1) {
                PROCESS_LINE(1, l_1, r_1, r_2);
            }
        }

        if (x >= 2) {
            PROCESS_LINE(2, l_2, l_1, r_1);
        }
    }

    if (y >= 1) {
        PROCESS_CELL(b_1, 0, -1);
        if (y >= 2) {
            PROCESS_CELL(b_2, 0, -2);
            if (y >= 3) {
                PROCESS_CELL(b_3, 0, -3);
                PROCESS_LINE(0, b_1, b_2, b_3); // in this case the offset doesn't matter, it's never used because there can't be an open cell below where we just played            }
            }
        }
    }
}

Outcome countThreatsAtCellJustTaken(
    i64 player_owned_bitfield,
    i64 opponent_owned_bitfield,
    i32 playable_ys,
    i8 x,
    i8 y,
    i8* player_threats,
    i8* opponent_threats,
    i8* forced_moves_next_turn_bitfield,
    bool opponent_has_win
) {
    i8 player_connect3s = 0;
    i8 opponent_connect3s = 0;
    i8 player_2_none_1s = 0;

    countThreats(
        player_owned_bitfield,
        opponent_owned_bitfield,
        playable_ys,
        x,
        y,
        &player_connect3s,
        &player_2_none_1s,
        &opponent_connect3s,
        forced_moves_next_turn_bitfield
    );

    if (player_connect3s > 0)
        return OUTCOME_WIN; // We just got connect-4

    if (opponent_has_win && opponent_connect3s == 0)
        return OUTCOME_LOSS; // We failed to block their connect-4 next turn

    (*opponent_threats) -= opponent_connect3s;
    (*player_threats) += player_2_none_1s;

    return OUTCOME_UNKNOWN;
}

Outcome countThreatsAtCellJustMadePlayable(
    i64 player_owned_bitfield,
    i64 opponent_owned_bitfield,
    i32 playable_ys,
    i8 x,
    i8 y,
    i8* player_threats,
    i8* opponent_threats,
    i8* forced_moves_next_turn_bitfield
) {
    i8 player_connect3s = 0;
    i8 opponent_connect3s = 0;
    i8 dont_care = 0;

    countThreats(
        player_owned_bitfield,
        opponent_owned_bitfield,
        playable_ys,
        x,
        y,
        &player_connect3s,
        &dont_care,
        &opponent_connect3s,
        &dont_care
    );

    if (opponent_connect3s > 0)
        return OUTCOME_LOSS; // We just gave them connect-4 next turn

    if (player_connect3s > 0)
        (*forced_moves_next_turn_bitfield) |= (1 << x);

    (*player_threats) += player_connect3s;

    return OUTCOME_UNKNOWN;
}

Outcome outcomeAbsentConnect4ThisTurnOrNext(i8 force_cols_bitfield) {
    switch (force_cols_bitfield) {
        case 0: return OUTCOME_UNKNOWN;
        case 1: return OUTCOME_FORCE_0;
        case 2: return OUTCOME_FORCE_1;
        case 4: return OUTCOME_FORCE_2;
        case 8: return OUTCOME_FORCE_3;
        case 16: return OUTCOME_FORCE_4;
        case 32: return OUTCOME_FORCE_5;
        case 64: return OUTCOME_FORCE_6;
        default: return OUTCOME_WIN; // There must be at least two forced moves, which our opponent can't satisfy
    }
}

Outcome countConnect3sOr4sAfterMove(State* state, Player player, i8 x, i8 y, Outcome parent_outcome) {
    // Since the game isn't over, if they have a win, that must mean they previously trapped us
    if (parent_outcome == OUTCOME_WIN)
        return OUTCOME_LOSS;

    i64 player_owned_bitfield;
    i64 opponent_owned_bitfield;
    i8* player_threats;
    i8* opponent_threats;
    i8 forced_moves_next_turn_bitfield = 0;

    if (player == P1) {
        player_owned_bitfield = state->p1_owned_bitfield;
        opponent_owned_bitfield = state->p2_owned_bitfield;
        player_threats = &(state->p1_threats);
        opponent_threats = &(state->p2_threats);
    }
    else {
        player_owned_bitfield = state->p2_owned_bitfield;
        opponent_owned_bitfield = state->p1_owned_bitfield;
        player_threats = &(state->p2_threats);
        opponent_threats = &(state->p1_threats);
    }

    Outcome outcome = countThreatsAtCellJustTaken(
        player_owned_bitfield,
        opponent_owned_bitfield,
        state->playable_ys,
        x,
        y,
        player_threats,
        opponent_threats,
        &forced_moves_next_turn_bitfield,
        isForce(parent_outcome)
    );

    if (outcome == OUTCOME_WIN || outcome == OUTCOME_LOSS)
        return outcome;

    if (y < NUM_ROWS-1) {
        outcome = countThreatsAtCellJustMadePlayable(
            player_owned_bitfield,
            opponent_owned_bitfield,
            state->playable_ys,
            x,
            y+1,
            player_threats,
            opponent_threats,
            &forced_moves_next_turn_bitfield
        );

        if (outcome == OUTCOME_LOSS)
            return outcome;
    }

    return outcomeAbsentConnect4ThisTurnOrNext(forced_moves_next_turn_bitfield);
}

Outcome setToStateAfterMove(State* parent, State* child, Player player, i8 x, i8 y) {
    if (player == P1) {
        child->p1_owned_bitfield = setOwned(parent->p1_owned_bitfield, x, y);
        child->p2_owned_bitfield = parent->p2_owned_bitfield;
    }
    else {
        child->p1_owned_bitfield = parent->p1_owned_bitfield;
        child->p2_owned_bitfield = setOwned(parent->p2_owned_bitfield, x, y);
    }

    assert(abs(bitsSet(child->p1_owned_bitfield) - bitsSet(child->p2_owned_bitfield)) < 2);

    child->p1_threats = parent->p1_threats;
    child->p2_threats = parent->p2_threats;

    child->playable_ys = incrementPlayableY(parent->playable_ys, x);

    Outcome outcome = countConnect3sOr4sAfterMove(child, player, x, y, parent->outcome);
    child->outcome = outcome;
    return outcome;
}

Score heuristicScore(State* node, Player player) {
    i8 p_threats, o_threats;
    if (player == P1) {
        p_threats = node->p1_threats;
        o_threats = node->p2_threats;
    }
    else {
        p_threats = node->p2_threats;
        o_threats = node->p1_threats;
    }

    if (p_threats == o_threats)
        return 0;

    Score score = ((Score) (p_threats - o_threats)) / (p_threats + o_threats + 1); // add one to ensure |score| < 1
    assert(score > -SCORE_MAX && score < SCORE_MAX);
    return score;
}

State*** children;

State* newGame() {
    State* state = malloc(sizeof(State));
    state->p1_owned_bitfield = 0;
    state->p2_owned_bitfield = 0;
    state->p1_threats = 0;
    state->p2_threats = 0;
    state->playable_ys = 0;
    state->outcome = OUTCOME_UNKNOWN;
    return state;
}

// We don't bother to cache at leafs, it's only worth it to avoid searching whole branches
bool cacheAtDepth(Depth depth) {
    return depth >= MIN_DEPTH_TO_CACHE || startDepth < MIN_DEPTH_TO_CACHE;
}

i8 getInitialBestMove(i8 valid_non_loss_cols, i32 playable_ys, State* state, i8 depth, i8* searchOrder) {
    // If we cached at this depth on the last, less deep search
    if (cacheAtDepth(depth-1)) {
        CacheVal* val = getCacheValue(state->p1_owned_bitfield, state->p2_owned_bitfield);
        if (isCacheValSet(val)) {
            i8 cached = val->best_move;

            // Insert at beginning of search order
            if (cached != searchOrder[0]) {
                i8 i=0;
                while (searchOrder[i] != cached) {
                    i++;
                    assert(i < NUM_COLS);
                }

                while (i>0) {
                    searchOrder[i] = searchOrder[i-1];
                    i--;
                }

                searchOrder[0] = cached;
            }

            return cached;
        }
    }

    if (valid_non_loss_cols > 0) {
        for (i8 i=0; i<NUM_COLS; i++) {
            i8 x = searchOrder[x];
            if (valid_non_loss_cols & (1 << x))
                return x;
        }
    }

    for (i8 i=0; i<NUM_COLS; i++) {
        i8 x = searchOrder[i];
        if (NUM_ROWS > getPlayableY(playable_ys, x))
            return x;
    }

    return -1;
}

#define CACHE_AND_RETURN(s, f, m) if (cacheAtDepth(depth)) setCacheValue(p1_bits, p2_bits, s, f, depth, m); return s;

Score minimax(State* node, Depth depth, Score alpha, Score beta, Player player) {
    if (depth <= 0) {
        return heuristicScore(node, player);
    }

    i64 p1_bits = node->p1_owned_bitfield;
    i64 p2_bits = node->p2_owned_bitfield;

    // Check cache first
    if (cacheAtDepth(depth)) {
        CacheVal* val = checkCache(p1_bits, p2_bits);

        if (val->start_depth == startDepth) {
            if (val->flags & EXACT) {
                return val->score;
            }
            else if ((val->flags & AT_MOST) && (val->score <= alpha)) {
                return alpha;
            }
            else if ((val->flags & AT_LEAST) && (val->score >= beta)) {
                return beta;
            }
        }
    }

    i32 playable_ys = node->playable_ys;

    // If we know our next move is forced, just search that
    if (isForce(node->outcome)) {
        i8 x = node->outcome;
        i8 y = getPlayableY(playable_ys, x);

        State* child = children[depth-1][x];
        Outcome outcome = setToStateAfterMove(node, child, player, x, y);

        if (outcome == OUTCOME_WIN) {
            CACHE_AND_RETURN(SCORE_MAX, EXACT, x);
        }
        else if (outcome == OUTCOME_LOSS) {
            CACHE_AND_RETURN(-SCORE_MAX, EXACT, x);
        }
        else {
            // Don't cache because we don't know whether the value is exact or an upper or lower bound, and anyway we wouldn't be saving much because the tree doesn't branch here
            // We can do this because we only rely on the cache to save the best move for output when there is no force move
            return -minimax(child, depth-1, -beta, -alpha, -player);
        }
    }

    i8 valid_cols = 0;
    i8 searchOrder[] = { 3, 2, 4, 1, 5, 0, 6 };

    // Absent a force move, search for instant wins
    for (i8 i=0; i<NUM_COLS; i++) {
        i8 x = searchOrder[i];
        i8 y = getPlayableY(playable_ys, x);

        if (y < NUM_ROWS) {
            State* child = children[depth-1][x];

            Outcome outcome = setToStateAfterMove(node, child, player, x, y);

            if (outcome == OUTCOME_WIN) {
                CACHE_AND_RETURN(SCORE_MAX, EXACT, x);
            }
            else if (outcome != OUTCOME_LOSS) {
                valid_cols |= (1 << x);
            }
        }
    }

    CacheFlags flag = AT_MOST;
    i8 best = getInitialBestMove(valid_cols, playable_ys, node, depth, searchOrder);

    // Absent forced move or instant win, search recursively
    for (i8 i=0; i<NUM_COLS; i++) {
        i8 x = searchOrder[i];

        if (valid_cols & (1 << x)) {
            State* child = children[depth-1][x];

            Score res = -minimax(child, depth-1, -beta, -alpha, -player);
            if (res > alpha) {
                flag = EXACT;
                alpha = res;
                best = x;
            }

            if (alpha >= beta) {
                flag = AT_LEAST;
                break;
            }
        }
    }

    CACHE_AND_RETURN(alpha, flag, best);
}

void initStorage(Depth depth) {
    children = malloc(depth*sizeof(State**));
    for (Depth i=0; i<depth; i++) {
        children[i] = malloc(NUM_COLS*sizeof(State*));
        for (i8 j=0; j<NUM_COLS; j++) {
            children[i][j] = malloc(sizeof(State));
        }
    }
}

void freeStorage(Depth depth) {
    for (Depth i=0; i<depth; i++) {
        for (i8 j=0; j<NUM_COLS; j++) {
            free(children[i][j]);
        }
        free(children[i]);
    }
    free(children);
}

void initializeCache() {
    cacheSize = 0;
    mostRecentlyUsed = NULL;

    cache = malloc(CACHE_CAPACITY*sizeof(CacheVal*));
    for (CacheIdx i=0; i<CACHE_CAPACITY; i++) {
        cache[i] = malloc(sizeof(CacheVal));
        setCacheValBlank(cache[i]);
    }
}

Score searchToDepth(State* state, Player player, Depth depth) {
    initStorage(depth);
    startDepth = depth;

    Score score = minimax(state, depth, -1, 1, player);

    freeStorage(depth);

    return score;
}

i8 findNextMove(int numMoves, i8* moves, long maxMillis) {
    long cutoffMillis = currentTimeMillis()+(maxMillis/2);

    State* state = newGame();
    Player player = P1;
    Outcome outcome = OUTCOME_UNKNOWN;

    for (int i=0; i<numMoves; i++) {
        i8 x = moves[i];
        i8 y = getPlayableY(state->playable_ys, x);

        assert(y < NUM_ROWS);

        outcome = setToStateAfterMove(state, state, player, x, y);
        player = -player;
    }

    if (isForce(outcome))
        return outcome;

    initializeCache();

    Score score;
    i8 best;
    Depth depth = 1;

    do {
        score = searchToDepth(state, player, depth);
        CacheVal* val = getCacheValue(state->p1_owned_bitfield, state->p2_owned_bitfield);
        assert(isCacheValSet(val));
        best = val->best_move;
        depth++;
    } while (currentTimeMillis() < cutoffMillis && score < SCORE_MAX && score > -SCORE_MAX);

    fprintf(stderr, "At depth %d scored %d:%f for move %d\n", depth, player, score, best);

    return best;
}

int main(int argc, char* argv[]) {
    assert(argc >= 2);

    int maxMillis = atoi(argv[1]);
    assert(maxMillis > 0);

    int numMoves = argc-2;
    i8* moves = malloc(sizeof(i8) * numMoves);
    for (int i=0; i<numMoves; i++) {
        moves[i] = atoi(argv[2+i]);
        assert(moves[i] >= 0 && moves[i] < NUM_COLS);
    }

    printf("%d\n", findNextMove(numMoves, moves, maxMillis));
}
