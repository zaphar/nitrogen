% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_event).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) -> 
	EventType = Record#event.type, 
	Override = case Record#event.override of
        false ->
            "return true;";
        true ->
            "return false;"
    end,
    Postback = make_postback(Record#event.postback, EventType, TriggerPath, TargetPath, Record#event.delegate),
	Actions = [wf_render:render_actions(TriggerPath, TargetPath, Record#event.actions)],

	case EventType of
		enterkey ->
			[
				wf:f("Nitrogen.$observe_event(obj('~s'), 'keypress', function anonymous(event) {", [wf:to_js_id(TriggerPath)]),
				wf:f("if (Nitrogen.$is_enter_key(event)) { ~s ~s; return false; }", [Postback, Actions]),
				wf:f("});\r\n")
			];
		
		_ when EventType == timer orelse EventType == continuation ->
			wf:f("setTimeout(\"~s ~s\", ~p);", [wf_utils:js_escape(Postback), wf_utils:js_escape(Actions), Record#event.delay]);
			
		_ ->
			[
				wf:f("Nitrogen.$observe_event(obj('~s'), '~s', function anonymous(event) { ~s ~s ~s });\r\n", [wf:to_js_id(TriggerPath), EventType, Postback, Actions, Override])
			]
	end.
	
make_postback_info(Tag, EventType, TriggerPath, TargetPath, Delegate) ->
	ObjectID = get(current_id),
	Delegate1 = case Delegate of
		undefined -> wf_platform:get_page_module();
		_ -> Delegate
	end,
	PostbackInfo = {ObjectID, Tag, EventType, TriggerPath, TargetPath, Delegate1},
	wf_utils:pickle(PostbackInfo).
	
make_postback(Postback, EventType, TriggerPath, TargetPath, Delegate) ->
	case Postback of
		undefined -> [];
		Tag ->
			PickledPostbackInfo = make_postback_info(Tag, EventType, TriggerPath, TargetPath, Delegate),
			wf:f("Nitrogen.$queue_event('~s', '~s');", [wf:to_js_id(TriggerPath), PickledPostbackInfo])
	end.
