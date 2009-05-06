% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_template).
-include ("wf.inc").
-export([render/2, reflect/0]).

% TODO - Revisit parsing in the to_module_callback. This
% will currently fail if we encounter a string like:
% "String with ) will fail" 
% or 
% "String with ]]] will fail"


reflect() -> record_info(fields, template).

render(_ControlID, Record) ->
    % Prevent loops.
    case wf:state(template_was_called) of
	true -> throw("Calling a template from a template.");
	_ -> ignore
    end,
    wf:state(template_was_called, true),

    % Parse the template file, or read it from cache.
    File = wf:to_list(Record#template.file),
    Template = parse_template(File),

    % IN PROGRESS - Caching
    % Key = {template, File},
    % Template = wf_cache:cache(Key, fun() -> parse_template(File) end, [{ttl, 5}]),

    % Evaluate the template.
    Body = eval(Template, Record),

    IsWindexMode = wf:q(windex) == ["true"],
    case IsWindexMode of
	true ->	[
		wf:f("Nitrogen.$lookup('~s').$update(\"~s\");", [get(current_id), wf_utils:js_escape(Body)])
	    ];
	false -> Body
    end.


parse_template(File) ->
    File1 = filename:join(nitrogen:get_templateroot(), File),
    case file:read_file(File1) of
	{ok, B} -> parse(B, <<>>, []);
	_ -> 
	    ?LOG("Error reading file: ~s~n", [File1]),
	    wf:f("File not found: ~s.", [File1])
    end.


%%% PARSE %%%

%% parse/2 - Given a binary and a callback, look through the binary
%% for strings of the form [[[module:function(args)]]] | [[[script]]]

parse(<<>>, BinAcc, Acc) -> lists:reverse([BinAcc | Acc]);
parse(<<"[[[", Rest/binary>>, BinAcc, Acc) ->
    parse_token(Rest, [], [BinAcc|Acc]);
parse(<<C, Rest/binary>>, BinAcc, Acc) ->
    parse(Rest, <<BinAcc/binary, C>>, Acc).


parse_token(<<"]]]", Rest/binary>>, Token, Acc) ->
    Callback = to_callback(lists:reverse(Token)),
    parse(Rest, <<>>, [Callback | Acc]);
parse_token(<<H, Rest/binary>>, Token, Acc) -> parse_token(Rest, [H|Token], Acc).



to_callback("script") -> script;
to_callback(Tag) ->
    % Get the module...
    {ModuleString, Rest1} = peel(Tag, $:),
    Module = wf:to_atom(ModuleString),

    % Get the function...
    {FunctionString, Rest2} = peel(Rest1, $(),
    Function = wf:to_atom(FunctionString),

    {ArgString, Rest3} = peel(Rest2, $)),

    case Rest3 of
	[] -> [{Module, Function, ArgString}];
	[$,|Rest4] ->  [{Module, Function, ArgString} | to_callback(Rest4)]
    end.

peel(S, Delim) -> peel(S, Delim, []).
peel([], _Delim, Acc) -> {lists:reverse(Acc), []};
peel([Delim|T], Delim, Acc) -> {lists:reverse(Acc), T};
peel([H|T], Delim, Acc) -> peel(T, Delim, [H|Acc]).


%%% EVALUATE %%%

eval(Blocks, Record) ->
    [do_eval(B, Record) || B <- Blocks].


do_eval(script, _Record) -> wf_script:get_script();
do_eval(Bin, _Record) when is_binary(Bin) -> Bin;
do_eval(Callbacks, Record) -> 
    [eval_callback(C, Record) || C <- Callbacks].


eval_callback({M, Function, ArgString}, Record) ->
    % De-reference to page module...
    Module = case M of 
	page -> wf_platform:get_page_module();
	_ -> M
    end,

    % Convert args to term...
    Args = string:tokens(ArgString, ", "),
    {Len, ExArgs} = expand_args(Args, Record#template.bindings),

    code:ensure_loaded(Module),
    case erlang:function_exported(Module, Function, Len) of 
	false -> 
	    % Function is not defined, so ignore.
	    "";
	true ->
	    wf:render(erlang:apply(Module, Function, ExArgs))
    end.


expand_args(Args, Bindings) -> expand_args(Args, Bindings, 0, []).

expand_args([], _Bindings, Len, Acc) -> {Len, lists:reverse(Acc)};
expand_args([A|Tail], Bindings, Len, Acc) ->
    case lists:keyfind(list_to_atom(A), 1, Bindings) of
	{_, Expansion} -> Arg = Expansion;
	false -> Arg = A
    end,
    expand_args(Tail, Bindings, Len+1, [Arg|Acc]).

