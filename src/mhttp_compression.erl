%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_compression).

-export([compress/2, decompress/2]).

-type scheme() :: gzip.

-spec compress(scheme(), iodata()) -> binary().
compress(gzip, Data) ->
  try
    zlib:gzip(Data)
  catch
    error:Reason ->
      error({zlib_error, Reason})
  end.

-spec decompress(scheme(), iodata()) -> binary().
decompress(gzip, Data) ->
  try
    zlib:gunzip(Data)
  catch
    error:data_error ->
      error({invalid_gzip_data, Data});
    error:Reason ->
      error({zlib_error, Reason})
  end.
