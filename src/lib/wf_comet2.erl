% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(wf_comet2).
-include("wf.inc").
-export([comet/1, get_content/1]).


% wf_content2 is an alternate to the comet solution provided by
% nitrogen. More documentation to follow.


% Time client is prepared to wait for ANY response
-define(CLIENT_REQ_TIMEOUT, 5000). % 5 seconds

% Time spent in acknowledged wait
-define(CLIENT_ACKED_WAIT, 30000). % 30 seconds

% No one lives forever, although planning to let agents survive a
% page refresh.
-define(AGENT_LIFESPAN, 180000). % 3 minutes



% Make client attaches a handler to a newly spawned middle
% man. A javascript call is made, so that on the opposite side, a
% an ajax long poll is made to this middle man process. 

comet(Handler) ->
    CurrentState = get(),
    F = fun() ->
	    % Copy state to the new process...
	    [put(X, Y) || {X, Y} <- CurrentState],
	    reset_nitrogen_state(),		

	    % HACK - We don't know if the page has flash or not,
	    % but assume it does, because if a comet function is
	    % present, then 99% of the time it will use flash.
	    wf:state(has_flash, true),

	    try handler_loop(Handler, [], none)
		catch Type : Error ->
		    io:format("CAUGHT ERROR: ~p-~p~n~p~n", [Type, Error, erlang:get_stacktrace()])
	    end,

	    % Flush out anything remaining
	    flush()
    end,
    AgentPid = spawn(F),
    wf:wire(#comet2_start{agent=AgentPid}),
    AgentPid.


get_content(AgentPid) ->
    AgentPid ! {self(), nitrogen_comet_req},
    receive
	{AgentPid, data, Data} -> 
	    Data;

	{AgentPid, ack} ->
	    get_content_wait(AgentPid)

    after ?CLIENT_REQ_TIMEOUT ->
	    % TODO: Client should probably refresh the page, and or
	    % get a new agent.
	    ?PRINT("Client request timed out"),
	    []
    end.


% The agent is alive, but no content available yet
get_content_wait(AgentPid) ->
    ?PRINT("Got Ack. Waiting."),
    receive
	{AgentPid, data, Data} ->
	    Data
    after ?CLIENT_ACKED_WAIT ->
	    ?PRINT("Leaving long wait"),
	    get_content(AgentPid)
    end.



% ====================================================================== 
% Internal
% ====================================================================== 

handler_loop(Handler, Acc, Waiting) ->
    receive
	{From, nitrogen_comet_req} when Acc =/= [] ->
	    From ! {self(), data, lists:reverse(Acc)},
	    handler_loop(Handler, [], none);

	{From, nitrogen_comet_req} ->
	    From ! {self(), ack},
	    handler_loop(Handler, Acc, From);

	Message ->
	    Handler(Message),
	    case flush() of 
		[] ->
		    handler_loop(Handler, Acc, Waiting);
		Data when is_pid(Waiting) ->
		    Waiting ! {self(), data, Data},
		    handler_loop(Handler, [], none);

		Data ->
		    handler_loop(Handler, [Data|Acc], Waiting)
	    end
    after ?AGENT_LIFESPAN ->
	    ?PRINT("Agent down."),
	    ok
    end.


flush() ->
    % Process any flash events...
    element_flash:update(),

    % Get the script...
    Content = wf_script:get_script(false),

    % See if there are redirects...
    RedirectUrl = get(wf_redirect),
    IsRedirect = RedirectUrl /= [] andalso RedirectUrl /= undefined,

    % Get the content to send back...
    Content1 = case IsRedirect of 
	true -> wf_handle:build_post_redirect(RedirectUrl);
	false -> Content
    end,
    reset_nitrogen_state(),
    Content1.


reset_nitrogen_state() ->
    % Clear some state variables...
    L = [wf_action_queue, wf_update_queue, wf_content_script, wf_script, wf_paths, wf_headers],
    [put(X, []) || X <- L].


