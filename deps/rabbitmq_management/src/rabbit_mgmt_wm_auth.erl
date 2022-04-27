%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_mgmt_wm_auth).

-export([init/2, to_json/2, content_types_provided/2, is_authorized/2]).
-export([variances/2]).

-include_lib("rabbitmq_management_agent/include/rabbit_mgmt_records.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

%%--------------------------------------------------------------------

init(Req, _State) ->
    {cowboy_rest, rabbit_mgmt_headers:set_common_permission_headers(Req, ?MODULE), #context{}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

content_types_provided(ReqData, Context) ->
   {rabbit_mgmt_util:responder_map(to_json), ReqData, Context}.

to_json(ReqData, Context) ->
    %% EnableUAA = application:get_env(rabbitmq_management, enable_uaa, false),
    EnableOAUTH = application:get_env(rabbitmq_management, oauth_enable, false),
    Data = case EnableOAUTH of
               true ->
                   OAuthClientId = application:get_env(rabbitmq_management, oauth_client_id, ""),
                   OAuthURL = application:get_env(rabbitmq_management, oauth_url, ""),
                   case is_invalid([OAuthClientId, OAuthURL]) of
                       true ->
                           rabbit_log:warning("Disabling OAuth 2 authorization, relevant configuration settings are missing", []),
                           [{oauth_enable, false}, {oauth_client_id, <<>>}, {oauth_url, <<>>}];
                       false ->
                           [{oauth_enable, true},
                            {oauth_client_id, rabbit_data_coercion:to_binary(OAuthClientId)},
                            {oauth_url, rabbit_data_coercion:to_binary(OAuthURL)}]
                   end;
               false ->
                   [{enable_oauth, false}, {oauth_client_id, <<>>}, {oauth_url, <<>>}]
           end,
    rabbit_mgmt_util:reply(Data, ReqData, Context).

is_authorized(ReqData, Context) ->
    {true, ReqData, Context}.

is_invalid(List) ->
    lists:any(fun(V) -> V == "" end, List).
