-module(rabbit_mgmt_throttle_h).
-behavior(cowboy_stream).

-export([init/3]). % With parsed headers.
-export([data/4]). % With parsed (chunked) body.
-export([info/3]). % With system updates.
-export([terminate/3]). % When the request is over.
-export([early_error/5]). % If failed before headers.

-record(state, {
        next :: any(),
        client_key :: binary(),
        resource_key :: binary(),
        throttled :: boolean(),
        reset :: erlang:timestamp()
}).

%% 
%% Callbacks
%%

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
    -> {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
    State0 = get_state(Req),
    {Commands0, Next} = cowboy_stream:init(StreamID, Req, Opts),
    Commands = case State0#state.client_key of
        {} -> [too_many_requests()|Commands0];
        _ -> Commands0
    end,
    {Commands, State0#state{next=Next}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
    -> {cowboy_stream:commands(), State} when State::#state{}.
data(StreamID, IsFin, Data, State0=#state{next=Next0}) ->
    {Commands0, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands0, State0#state{next=Next}}.

-spec info(cowboy_stream:streamid(), any(), State)
    -> {cowboy_stream:commands(), State} when State::#state{}.
info(StreamID, Info, State0=#state{next=Next0}) ->
    {Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
    {Commands0, State0#state{next=Next}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, #state{next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
    cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
    when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%%
%% Internal
%%

-spec get_state(cowboy_req:req()) -> #state{}.
get_state(#{path := Path, headers := Headers, peer := {IP, _}}) ->
    Key = case maps:get(<<"authorization">>, Headers, undefined) of
              undefined -> case inet:ntoa(IP) of
                               {error, einval} -> <<"unknown_peer">>;
                               String -> String
                           end;
              IOData -> IOData
          end,
    #state{client_key = iolist_to_binary(Key), resource_key = Path}.

-spec too_many_requests() -> {headers, cowboy:http_status(), cowboy:http_headers()}.
too_many_requests() ->
    {error_response, 429, #{<<"retry-after">> => 10}, <<>>}.

