-module (web_samples_comet1).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.
	
title() -> "Simple Comet Example".
headline() -> "Simple Comet Example".
right() -> linecount:render().

body() -> 
	Body = [
		#span { text="Counter updated via Comet: " },
		#span { id=myCounter, text="-" }
  ],

	% Start the counter as a background process.
	Agent = wf:comet2(fun background_update/1),
	spawn(fun() -> link(Agent), timer(Agent, myCounter, 1) end),
	wf:render(Body).

event(_) -> ok.


timer(Agent, ControlID, Count) ->
	% Sleep for a second, then update
	timer:sleep(1000),

	Agent ! {ControlID, Count},
	
	% Loop. This process will automatically be killed
	% once the page stops requesting the output that
	% it generates.
	timer(Agent, ControlID, Count + 1).
	


background_update({ControlID, Count}) ->
	% Update the control.
	wf:update(ControlID, wf:to_list(Count)).
	
