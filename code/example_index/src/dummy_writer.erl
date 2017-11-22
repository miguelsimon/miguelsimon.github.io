-module(dummy_writer).

%% API
-export([start_writer/1, start_writers/1]).

-include("globals.hrl").
-record(state, {address, intents = []}).

-define(MEAN_SECONDS_BETWEEN, 100).

-define(TOKENS, lists:seq(0, ?NUM_TOKENS)).

get_random_token() ->
    N = random:uniform(?NUM_TOKENS),
    lists:nth(N, ?TOKENS).

writer_init(Address) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    State = #state{address = Address},
    writer_loop(State).

writer_loop(State = #state{address = Address, intents = Intents}) ->
    timer:sleep(round(timer:seconds(random:uniform() * ?MEAN_SECONDS_BETWEEN))),
    
    MakerToken = get_random_token(),
    TakerToken = get_random_token(),
    
    example_index:add_intent(Address, MakerToken, TakerToken),
    Intent = #intent{address = Address, makerToken = MakerToken, takerToken = TakerToken},

    if 
        length(Intents) > 4 ->
            [OldIntent | ReversedRest] = lists:reverse(Intents),
            Rest = lists:reverse(ReversedRest),
            example_index:remove_intent(Address, OldIntent#intent.makerToken, OldIntent#intent.takerToken),
            writer_loop(State#state{intents = [Intent | Rest]});
        true ->
            writer_loop(State#state{intents = [Intent | Intents]})
    end,
    
    writer_loop(State).
    
start_writer(Address) ->
    spawn_link(fun () -> writer_init(Address) end).
    
start_writers(Num) ->
    lists:map(fun (Id) -> start_writer(Id) end, lists:seq(0, Num)).