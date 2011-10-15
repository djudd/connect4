#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#define i64 unsigned long long
#define i32 unsigned long
#define i8 signed char

#define Player signed char
#define P1 1
#define P2 (-1)

#define NUM_ROWS 6
#define NUM_COLS 7

#define Score float
#define INF 1

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

typedef struct state {
    i64 p1_owned_bitfield;
    i64 p2_owned_bitfield;
    i8 p1_threats;
    i8 p2_threats;
    i8 p1_forces_on_opponent;
    i8 p2_forces_on_opponent;
    i32 playable_ys;
    i8 forced_next_move;
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

    printBoard(parent);
    printBoard(child);

    fprintf(stderr,"playable Ys: %d %d %d %d %d %d %d\t", tops[0], tops[1], tops[2], tops[3], tops[4], tops[5], tops[6]);
    fprintf(stderr,"player: %d\tx: %d\ty: %d\tp1 moves: %d\tp2 moves: %d\n", player, x, y, bitsSet(child->p1_owned_bitfield), p2_moves);
    fprintf(stderr,"p1 forces=%d, threats=%d; p2 forces=%d, threats=%d; score=%f\n", child->p1_forces_on_opponent, child->p1_threats, child->p2_forces_on_opponent, child->p2_threats, res);
    fprintf(stderr,"\n\n");
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

// Returns bitfield containing columns with forced moves by opponent next turn, or -1 for a win
i8 countThreatsAtCellJustTaken(
    i64 player_owned_bitfield,
    i64 opponent_owned_bitfield,
    i32 playable_ys,
    i8 x,
    i8 y,
    i8* player_threats,
    i8* opponent_threats,
    i8* player_forces_on_opponent,
    i8* opponent_forces_on_player
) {
    i8 player_connect3s = 0;
    i8 opponent_connect3s = 0;
    i8 player_2_none_1s = 0;
    i8 force_cols_bitfield = 0;

    countThreats(
        player_owned_bitfield,
        opponent_owned_bitfield,
        playable_ys,
        x,
        y,
        &player_connect3s,
        &player_2_none_1s,
        &opponent_connect3s,
        &force_cols_bitfield
    );

    // close threats & forces corresponding to connect-3s which terminate at cell just taken
    if (player_connect3s > 0) {
        (*player_forces_on_opponent)--;
        (*player_threats) -= player_connect3s;
    }
    if (opponent_connect3s > 0) {
        (*opponent_forces_on_player)--;
        (*opponent_threats) -= opponent_connect3s;
    }

    // open threats & forces corresponding to new 3-out-of-4s now owned by player which include cell just taken
    (*player_threats) += player_2_none_1s;
    if (force_cols_bitfield)
        (*player_forces_on_opponent)++;
    if (force_cols_bitfield & (force_cols_bitfield-1)) // at least two bits set - we only care about two, because two forces is enough for a win
        (*player_forces_on_opponent)++;

    if (player_connect3s > 0)
        return -1; // we just got connect-4
    else
        return force_cols_bitfield;
}

void countThreatsAtCellJustMadePlayable(
    i64 player_owned_bitfield,
    i64 opponent_owned_bitfield,
    i32 playable_ys,
    i8 x,
    i8 y,
    i8* player_threats,
    i8* opponent_threats,
    i8* player_forces_on_opponent,
    i8* opponent_forces_on_player
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

    // open threats & forces corresponding to existing connect-3s which now have their open cell playable
    if (player_connect3s > 0) {
        (*player_forces_on_opponent)++;
        (*player_threats) += player_connect3s;
    }
    if (opponent_connect3s > 0) {
        (*opponent_forces_on_player)++;
        (*opponent_threats) += opponent_connect3s;
    }
}

Outcome outcomeGivenForce(i8 force_cols_bitfield, i8 cur_col) {
    switch (force_cols_bitfield) {
        case 1: return OUTCOME_FORCE_0;
        case 2: return OUTCOME_FORCE_1;
        case 4: return OUTCOME_FORCE_2;
        case 8: return OUTCOME_FORCE_3;
        case 16: return OUTCOME_FORCE_4;
        case 32: return OUTCOME_FORCE_5;
        case 64: return OUTCOME_FORCE_6;
        default: return (Outcome) cur_col; // we didn't find a force including the just-played cell, so it must be open at the cell above
    }
}

Outcome countConnect3sOr4sAfterMove(State* state, Player player, i8 x, i8 y) {
    i64 player_owned_bitfield, opponent_owned_bitfield;
    i8 player_threats, player_forces_on_opponent, opponent_threats, opponent_forces_on_player;

    if (player == P1) {
        player_owned_bitfield = state->p1_owned_bitfield;
        opponent_owned_bitfield = state->p2_owned_bitfield;
        player_threats = state->p1_threats;
        opponent_threats = state->p2_threats;
        player_forces_on_opponent = state->p1_forces_on_opponent;
        opponent_forces_on_player = state->p2_forces_on_opponent;
    }
    else {
        player_owned_bitfield = state->p2_owned_bitfield;
        opponent_owned_bitfield = state->p1_owned_bitfield;
        player_threats = state->p2_threats;
        opponent_threats = state->p1_threats;
        player_forces_on_opponent = state->p2_forces_on_opponent;
        opponent_forces_on_player = state->p1_forces_on_opponent;
    }

    i8 force_cols_bitfield = countThreatsAtCellJustTaken(
        player_owned_bitfield,
        opponent_owned_bitfield,
        state->playable_ys,
        x,
        y,
        &player_threats,
        &opponent_threats,
        &player_forces_on_opponent,
        &opponent_forces_on_player
    );

    if (y < NUM_ROWS-1) {
        countThreatsAtCellJustMadePlayable(
            player_owned_bitfield,
            opponent_owned_bitfield,
            state->playable_ys,
            x,
            y+1,
            &player_threats,
            &opponent_threats,
            &player_forces_on_opponent,
            &opponent_forces_on_player
        );
    }

    if (player == P1) {
        state->p1_threats = player_threats;
        state->p2_threats = opponent_threats;
        state->p1_forces_on_opponent = player_forces_on_opponent;
        state->p2_forces_on_opponent = opponent_forces_on_player;
    }
    else {
        state->p2_threats = player_threats;
        state->p1_threats = opponent_threats;
        state->p2_forces_on_opponent = player_forces_on_opponent;
        state->p1_forces_on_opponent = opponent_forces_on_player;
    }

    if (force_cols_bitfield == -1)
        return OUTCOME_WIN; // we just got connect-4

    if (opponent_forces_on_player > 0)
        return OUTCOME_LOSS; // they have connect-4 next turn

    if (player_forces_on_opponent > 1)
        return OUTCOME_WIN; // we are guaranteed connect-4 turn after next

    if (player_forces_on_opponent > 0)
        return outcomeGivenForce(force_cols_bitfield, x);

    return OUTCOME_UNKNOWN;
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
    child->p1_forces_on_opponent = parent->p1_forces_on_opponent;
    child->p2_forces_on_opponent = parent->p2_forces_on_opponent;

    child->playable_ys = incrementPlayableY(parent->playable_ys, x);

    Outcome outcome = countConnect3sOr4sAfterMove(child, player, x, y);

    if (outcome < OUTCOME_WIN) // force move
        child->forced_next_move = outcome;
    else
        child->forced_next_move = -1;

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
    assert(score > -INF && score < INF);
    return score;
}

State*** children;

State* newGame() {
    State* state = malloc(sizeof(State));
    state->p1_owned_bitfield = 0;
    state->p2_owned_bitfield = 0;
    state->p1_threats = 0;
    state->p2_threats = 0;
    state->p1_forces_on_opponent = 0;
    state->p2_forces_on_opponent = 0;
    state->playable_ys = 0;
    state->forced_next_move = -1;
    return state;
}

Score minimax(State* node, int depth, Score alpha, Score beta, Player player) {
    if (depth <= 0) {
        return heuristicScore(node, player);
    }

    // TODO Check transposition table cache here?

    i32 playable_ys = node->playable_ys;

    // If we know our next move is forced, just search that
    if (node->forced_next_move >= 0) {
        i8 x = node->forced_next_move;
        i8 y = getPlayableY(playable_ys, x);

        Outcome outcome = setToStateAfterMove(node, node, player, x, y);

        if (outcome == OUTCOME_WIN)
            return INF;
        else if (outcome == OUTCOME_LOSS)
            return -INF;
        else
             // TODO experiment empirically with failing to decrement depth; should we search past our 'fixed' depth to follow a chain of forces?
            return -minimax(node, depth-1, -beta, -alpha, -player);
    }

    i8 valid_cols = 0;

    // Absent a force move, search for instant wins
    for (i8 x=0; x<NUM_COLS; x++) {
        i8 y = getPlayableY(playable_ys, x);
        if (y < NUM_ROWS) {
            State* child = children[depth-1][x];

            Outcome outcome = setToStateAfterMove(node, child, player, x, y);

            if (outcome == OUTCOME_WIN)
                return INF;
            else if (outcome != OUTCOME_LOSS)
                valid_cols |= (1 << x);
        }
    }

    // Absent forced move or instant win, search recursively
    for (i8 x=0; x<NUM_COLS; x++) {
        if (valid_cols & (1 << x)) {
            State* child = children[depth-1][x];

            Score res = -minimax(child, depth-1, -beta, -alpha, -player);
            if (res >= alpha)
                alpha = res;

            if (alpha >= beta)
                break;
        }
    }

    return alpha;
}

void initStorage(int depth) {
    children = malloc(depth*sizeof(State**));
    for (int i=0; i<depth; i++) {
        children[i] = malloc(NUM_COLS*sizeof(State*));
        for (int j=0; j<NUM_COLS; j++) {
            children[i][j] = malloc(sizeof(State));
        }
    }
}

void freeStorage(int depth) {
    for (int i=0; i<depth; i++) {
        for (int j=0; j<NUM_COLS; j++) {
            free(children[i][j]);
        }
        free(children[i]);
    }
    free(children);
}

Score score(int numMoves, i8* moves, int depth) {
    State* state = newGame();
    Player player = P1;
    Outcome outcome = OUTCOME_UNKNOWN;

    for (int i=0; i<numMoves; i++) {
        i8 x = moves[i];
        i8 y = getPlayableY(state->playable_ys, x);

        if (y >= NUM_ROWS) // invalid move
            return -2*INF; // ensure lower score than a loss

        outcome = setToStateAfterMove(state, state, player, x, y);

        player = -player;
    }

    // Player is now the opponent of the player who just moved
    if (outcome == OUTCOME_WIN)
        return -(player*INF);
    if (outcome == OUTCOME_LOSS)
        return player*INF;

    initStorage(depth);

    Score score = minimax(state, depth, -INF, INF, player);

    freeStorage(depth);
    free(state);

    return -score;
}

int main(int argc, char* argv[]) {
    assert(argc >= 2);

    int depth = atoi(argv[1]);
    assert(depth >= 0);

    int numMoves = argc-2;
    i8* moves = malloc(sizeof(i8) * numMoves);
    for (int i=0; i<numMoves; i++) {
        moves[i] = atoi(argv[2+i]);
        assert(moves[i] >= 0 && moves[i] < NUM_COLS);
    }

    printf("%f\n", score(numMoves, moves, depth));
}
