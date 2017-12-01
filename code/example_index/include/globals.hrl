-define(NUM_TOKENS, 100).
-define(TOKENS, lists:seq(0, ?NUM_TOKENS)).

-record(intent, {address, makerToken, takerToken}).