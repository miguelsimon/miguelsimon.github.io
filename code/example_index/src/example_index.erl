-module(example_index).
-behaviour(gen_server).
%% API
-export([start_link/0, add_intent/3, remove_intent/3, get_intent/1, find_intent/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-include("globals.hrl").

-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_intent(Adress, MakerToken, TakerToken) ->
    gen_server:call(?MODULE, {add_intent, Adress, MakerToken, TakerToken}).

remove_intent(Adress, MakerToken, TakerToken) ->
    gen_server:call(?MODULE, {remove_intent, Adress, MakerToken, TakerToken}).

get_intent(Adress) ->
    gen_server:call(?MODULE, {get_intent, Adress}).

find_intent(MakerToken, TakerToken) ->
    gen_server:call(?MODULE, {find_intent, MakerToken, TakerToken}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
%     {atomic, ok} = mnesia:create_table(intent, [
%         {attributes, record_info(fields, intent)},
%         {index, [makerToken, takerToken]},
%         {type, bag}]),
    {ok, #state{}}.
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_intent, Address, MakerToken, TakerToken}, _From, State) ->
    Write = fun() -> 
        mnesia:write(#intent{address = Address, makerToken = MakerToken, takerToken = TakerToken})
    end,
    
    {atomic, _} = mnesia:transaction(Write),
    {reply, ok, State};
    
handle_call({remove_intent, Address, MakerToken, TakerToken}, _From, State) ->
    Remove = fun() -> 
        mnesia:dirty_delete_object(#intent{address = Address, makerToken = MakerToken, takerToken = TakerToken})
    end,
    
    {atomic, Res} = mnesia:transaction(Remove),
    {reply, Res, State};
    
handle_call({get_intent, Address}, _From, State) ->
    Intents = mnesia:dirty_read(intent, Address),
    {reply, Intents, State};
    
handle_call({find_intent, MakerToken, TakerToken}, _From, State) ->
    Intents = mnesia:dirty_match_object(#intent{address = '_', makerToken = MakerToken, takerToken = TakerToken}),

    {reply, Intents, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

