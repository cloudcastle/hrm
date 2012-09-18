-module(hrm_utils).

-export([thing_to_list/1, append_query_params/2, current_time/0]).

%% General "to_string" implementation
thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% Appends proplist to url as query string 
append_query_params(Url, []) ->
  Url;
append_query_params(Url, Params) ->
  {ok, Uri} = uri:parse(Url),
  QueryString = element(6, Uri),
  Separator = case QueryString of [] -> "?"; _ -> "&" end,
  QueryString2 = QueryString ++ Separator ++ format_query(Params),
  Uri2 = setelement(6, Uri, QueryString2),
  uri:to_string(Uri2).

format_query(Params) ->
  Stringer = fun({K, V}) -> ["&", http_uri:encode(thing_to_list(K)), "=", http_uri:encode(thing_to_list(V))] end,
  lists:concat(tl(lists:flatmap(Stringer, Params))).

%% Current GMT time in RFC1123 format
current_time() ->
  httpd_util:rfc1123_date(erlang:universaltime()).
