-module(nitrogen_sup).
-export([start_ext_link/0, start_link/0, start_link/1]).
-export([init/1]).
-behaviour(supervisor).


% Can be used to place nitrogen into an existing supervison tree.
% 
start_ext_link() ->
    wf:init(),
    supervisor:start_link(?MODULE, []).


start_link() ->
    supervisor:start_link(?MODULE, []).
start_link(Extra) ->
    supervisor:start_link(?MODULE, Extra).


init(Extra) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [
		{wf_session_server, 
		    {wf_session_server, start_link, []},
		    permanent, 
		    2000,
		    worker,
		    [wf_session_server] },
		{wf_session_sup,
		    {wf_session_sup, start_link, []},
		    permanent,
		    2000,
		    supervisor,
		    [wf_session_sup] }
	    ] ++ Extra 
	}
    }.
