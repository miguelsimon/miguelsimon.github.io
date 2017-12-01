-module(dummy_reader).

%% API
-export([start_reader/1, start_readers/2]).

-include("globals.hrl").
-record(state, {mean_seconds_between}).

normal_request() ->
	MakerToken = dummy_writer:get_random_token(),
	TakerToken = dummy_writer:get_random_token(),
	example_index:find_intent(MakerToken, TakerToken).

maker_wildcard_request() ->
	MakerToken = '_',
	TakerToken = dummy_writer:get_random_token(),
	example_index:find_intent(MakerToken, TakerToken).

taker_wildcard_request() ->
	MakerToken = dummy_writer:get_random_token(),
	TakerToken = '_',
	example_index:find_intent(MakerToken, TakerToken).

reader_init(MeanSecondsBetween) ->
	reader_loop(#state{mean_seconds_between = MeanSecondsBetween}).

reader_loop(State = #state{mean_seconds_between = MeanSecondsBetween}) ->
    timer:sleep(round(timer:seconds(random:uniform() * MeanSecondsBetween))),
	N = random:uniform(3),
	Fun = lists:nth(N, [
		fun () -> normal_request() end,
		fun () -> maker_wildcard_request() end,
		fun () -> taker_wildcard_request() end]),
	Fun(),
	reader_loop(State).

start_reader(MeanSecondsBetween) ->
    spawn_link(fun () -> reader_init(MeanSecondsBetween) end).
    
start_readers(Num, MeanSecondsBetween) ->
    lists:map(fun (_) -> start_reader(MeanSecondsBetween) end, lists:seq(0, Num)).