% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(action_comet2_start).
-include("wf.inc").
-compile(export_all).

render_action(_TriggerPath, _Target, Record) -> 
    Tag = {comet, Record#comet2_start.agent},
    PostbackInfo = action_event:make_postback_info(Tag, comet, undefined, undefined, undefined),
    wf:f("Nitrogen.$comet_start('~s');", [PostbackInfo]).

