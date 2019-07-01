-module(erlangdemo_genserv).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, set_list/2, append_list/2, get_next_word/1, get_list/1]).

-record(state, {offset,list}).

start_link() ->
    gen_server:start_link( {local, erlangdemo_genserv}
                         , erlangdemo_genserv
                         , ["hello","world","foo","bar"]
                         , []
                         ).
init([]) ->
    {ok, #state{offset=0, list=[]}}.
handle_call({update, List}, _, State) ->
    {reply, List, State#state{list=List}};
handle_call(get_word, _, #state{offset=Offset, list=List}) ->
    NewOffset = (Offset + 1) rem length(List),
    Elem = lists:nth(Offset + 1, List),
    {reply, Elem, #state{offset=NewOffset, list=List}};
handle_call(get_list, _, State) ->
    {reply, State#state.list, State}.

handle_cast(_, _) ->
    {noreply, foo}.

%% get the next list element
get_next_word(Pid) ->
    gen_server:call(Pid, get_word).

%% get the entire list
get_list(Pid) ->
    gen_server:call(Pid, get_list).

%% set the list
set_list(Pid, List) ->
    gen_server:call(Pid, {update, List}).

append_list(Pid, Item) ->
    List = get_list(Pid),
    set_list(Pid, [Item|List]).
