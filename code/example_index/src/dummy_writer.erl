-module(dummy_writer).

%% API
-export([start_writer/2, start_writers/2, get_random_token/0]).

-include("globals.hrl").
-record(state, {address, mean_seconds_between, intents = []}).

get_random_token() ->
    N = random:uniform(?NUM_TOKENS),
    lists:nth(N, ?TOKENS).

writer_init(Address, MeanSecondsBetween) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    State = #state{address = Address, mean_seconds_between = MeanSecondsBetween},
    writer_loop(State).

writer_loop(State = #state{address = Address, mean_seconds_between = MeanSecondsBetween, intents = Intents}) ->
    timer:sleep(round(timer:seconds(random:uniform() * MeanSecondsBetween))),
    
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
    
start_writer(Address, MeanSecondsBetween) ->
    spawn_link(fun () -> writer_init(Address, MeanSecondsBetween) end).
    
start_writers(Num, MeanSecondsBetween) ->
    lists:map(fun (Id) -> start_writer(Id, MeanSecondsBetween) end, lists:seq(0, Num)).