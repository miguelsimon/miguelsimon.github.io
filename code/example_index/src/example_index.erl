-module(example_index).

-export([add_intent/3, remove_intent/3, get_intent/1, find_intent/2, reset_intent_table/0]).

-include("globals.hrl").


reset_intent_table() ->
	mnesia:delete_table(intent),
	{atomic, ok} = mnesia:create_table(intent, [
		{attributes, record_info(fields, intent)},
		{index, [makerToken, takerToken]},
		{type, bag}]).
		
add_intent(Address, MakerToken, TakerToken) ->
	Write = fun() -> 
		mnesia:write(#intent{address = Address, makerToken = MakerToken, takerToken = TakerToken})
	end,
	
	{atomic, _} = mnesia:transaction(Write).

remove_intent(Address, MakerToken, TakerToken) ->
    Remove = fun() -> 
        mnesia:dirty_delete_object(#intent{address = Address, makerToken = MakerToken, takerToken = TakerToken})
    end,
    
    {atomic, Res} = mnesia:transaction(Remove),
    Res.
    
get_intent(Address) ->
    mnesia:dirty_read(intent, Address).
    
find_intent(MakerToken, TakerToken) ->
	mnesia:dirty_match_object(#intent{address = '_', makerToken = MakerToken, takerToken = TakerToken}).