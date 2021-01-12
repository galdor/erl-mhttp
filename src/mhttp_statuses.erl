%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_statuses).

-export([reason/1,
	 status/1]).

-export_type([status_name/0]).

-type status_name() :: informational()
		     | successful()
		     | redirection()
		     | client_error()
		     | server_error().

-type informational() :: continue | switching_protocols.

-type successful() :: ok
		    | created
		    | accepted
		    | non_authoritative_information
		    | no_content
		    | reset_content.

-type redirection() :: multiple_choices
		     | moved_permanently
		     | found
		     | see_other
		     | use_proxy
		     | temporary_redirect.

-type client_error() :: bad_request
		      | payment_required
		      | forbidden
		      | not_found
		      | method_not_allowed
		      | not_acceptable
		      | request_timeout
		      | conflict
		      | gone
		      | length_required
		      | payload_too_large
		      | uri_too_long
		      | unsupported_media_type
		      | expectation_failed
		      | upgrade_required.

-type server_error() :: internal_server_error
		      | not_implemented
		      | bad_gateway
		      | service_unavailable
		      | gateway_timeout
		      | http_version_not_supported.

-spec reason(mhttp:status()) -> binary().

%% RFC 7231 (HTTP/1.1 Semantics and Content)
reason(100) -> <<"Continue">>;
reason(101) -> <<"Switching Protocols">>;
reason(200) -> <<"OK">>;
reason(201) -> <<"Created">>;
reason(202) -> <<"Accepted">>;
reason(203) -> <<"Non-Authoritative Information">>;
reason(204) -> <<"No Content">>;
reason(205) -> <<"Reset Content">>;
reason(300) -> <<"Multiple Choices">>;
reason(301) -> <<"Moved Permanently">>;
reason(302) -> <<"Found">>;
reason(303) -> <<"See Other">>;
reason(305) -> <<"Use Proxy">>;
reason(307) -> <<"Temporary Redirect">>;
reason(400) -> <<"Bad Request">>;
reason(402) -> <<"Payment Required">>;
reason(403) -> <<"Forbidden">>;
reason(404) -> <<"Not Found">>;
reason(405) -> <<"Method Not Allowed">>;
reason(406) -> <<"Not Acceptable">>;
reason(408) -> <<"Request Timeout">>;
reason(409) -> <<"Conflict">>;
reason(410) -> <<"Gone">>;
reason(411) -> <<"Length Required">>;
reason(413) -> <<"Payload Too Large">>;
reason(414) -> <<"URI Too Long">>;
reason(415) -> <<"Unsupported Media Type">>;
reason(417) -> <<"Expectation Failed">>;
reason(426) -> <<"Upgrade Required">>;
reason(500) -> <<"Internal Server Error">>;
reason(501) -> <<"Not Implemented">>;
reason(502) -> <<"Bad Gateway">>;
reason(503) -> <<"Service Unavailable">>;
reason(504) -> <<"Gateway Timeout">>;
reason(505) -> <<"HTTP Version Not Supported">>;

%% RFC 2295 (Transparent Content Negotiation in HTTP)
reason(506) -> <<"Variant Also Negotiates">>;

%% RFC 2324 (Hyper Text Coffee Pot Control Protocol (HTCPCP/1.0))
reason(418) -> <<"I'm a teapot">>;

%% RFC 2518 (HTTP Extensions for Distributed Authoring -- WEBDAV)
reason(102) -> <<"Processing">>;

%% RFC 2774 (An HTTP Extension Framework)
reason(510) -> <<"Not Extended">>;

%% RFC 3229
reason(226) -> <<"IM Used">>;

%% RFC 4918 (WebDAV)
reason(207) -> <<"Multi-Status">>;
reason(422) -> <<"Unprocessable Entity">>;
reason(423) -> <<"Locked">>;
reason(424) -> <<"Failed Dependency">>;
reason(507) -> <<"Insufficient Storage">>;

%% RFC 5842 (Binding Extensions to WebDAV)
reason(208) -> <<"Already Reported">>;
reason(508) -> <<"Loop Detected">>;

%% RFC 6585 (Additional HTTP Status Codes)
reason(428) -> <<"Precondition Required">>;
reason(429) -> <<"Too Many Requests">>;
reason(431) -> <<"Request Header Fields Too Large">>;
reason(511) -> <<"Network Authentication Required">>;

%% RFC 7232 (HTTP/1.1 Conditional Requests)
reason(304) -> <<"Not Modified">>;
reason(412) -> <<"Precondition Failed">>;

%% RFC 7233 (HTTP/1.1 Range Requests)
reason(206) -> <<"Partial Content">>;
reason(416) -> <<"Range Not Satisfiable">>;

%% RFC 7235 (HTTP/1.1 Authentication)
reason(401) -> <<"Unauthorized">>;
reason(407) -> <<"Proxy Authentication Required">>;

%% RFC 7538 (HTTP Status Code 308)
reason(308) -> <<"Permanent Redirect">>;

%% RFC 7725 (An HTTP Status Code to Report Legal Obstacles)
reason(451) -> <<"Unavailable For Legal Reasons">>;

%% RFC 8297 (An HTTP Status Code for Indicating Hints)
reason(103) -> <<"Early Hints">>;

%% RFC 8470 (HTTP Early Data)
reason(425) -> <<"Too Early">>;

reason(_Status) ->
  <<"Unknown">>.

-spec status(status_name()) ->
	mhttp:status().
status(continue) ->
  100;
status(switching_protocols) ->
  101;
status(ok) ->
  200;
status(created) ->
  201;
status(accepted) ->
  202;
status(non_authoritative_information) ->
  203;
status(no_content) ->
  204;
status(reset_content) ->
  205;
status(multiple_choices) ->
  300;
status(moved_permanently) ->
  301;
status(found) ->
  302;
status(see_other) ->
  303;
status(use_proxy) ->
  305;
status(temporary_redirect) ->
  307;
status(bad_request) ->
  400;
status(payment_required) ->
  402;
status(forbidden) ->
  403;
status(not_found) ->
  404;
status(method_not_allowed) ->
  405;
status(not_acceptable) ->
  406;
status(request_timeout) ->
  408;
status(conflict) ->
  409;
status(gone) ->
  410;
status(length_required) ->
  411;
status(payload_too_large) ->
  413;
status(uri_too_long) ->
  414;
status(unsupported_media_type) ->
  415;
status(expectation_failed) ->
  417;
status(upgrade_required) ->
  426;
status(internal_server_error) ->
  500;
status(not_implemented) ->
  501;
status(bad_gateway) ->
  502;
status(service_unavailable) ->
  503;
status(gateway_timeout) ->
  504;
status(http_version_not_supported) ->
  505.
