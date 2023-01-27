%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Keep change map
%%% @end
%%% Created : 16 Jan 2023 by Tony Rogvall <tony@rogvall.se>

-module(fuserl_image_bmap).

-export([create/3]).
-export([open/1]).
-export([reopen/2]).
-export([close/1]).
-export([mark/2, mark/3, clear/2, clear/3]).
-export([scan/1]).

%% test
-ifdef(TEST).
-export([create/0, open/0]).
-endif.

-define(KB(X), (1024*(X))).
-define(MB(X), (1024*?KB(X))).
-define(GB(X), (1024*?MB(X))).

-define(FILEVSN, "BITMAP10").  %% 8 character magic+vsn
-define(HEADER_SIZE, 1024).  %% max header size (zero padded)
-define(HEADER(BS,MaxSize), ?FILEVSN,(BS):64,(MaxSize):64).

%%-define(verbose(F, A), io:format((F),(A))).
-define(verbose(F, A), ok).

-type unsigned() :: non_neg_integer().
-type bmap() :: {file:io_device(),unsigned(),epx:bitmap()}.

-spec create(Name::file:name_all(), BS::unsigned(), MaxSize::unsigned()) ->
	  bmap().

create(Name, BS, MaxSize) ->
    case filelib:is_file(Name) of
	true -> error(file_exist);
	false ->
	    {ok, Fd} = file:open(Name, [read,write,raw,binary]),
	    BM = create_bm(BS,MaxSize),
	    Header = <<?HEADER(BS,MaxSize)>>,
	    file:write(Fd, Header),
	    Pad = 1024 - byte_size(Header),
	    file:write(Fd, <<0:Pad/unit:8>>),
	    {Fd,BS,BM}
    end.

-spec open(Name::file:name_all()) ->
	  bmap().

open(Name) ->
    case filelib:is_file(Name) of
	false -> error(enoent);
	true ->
	    case file:open(Name, [read,write,raw,binary]) of
		{ok,Fd} ->
		    case file:read(Fd,?HEADER_SIZE) of
			{ok,<<?HEADER(BS,MaxSize),_/binary>>} ->
			    BM = create_bm(BS,MaxSize),
			    Width = epx:bitmap_info(BM, width),
			    Bytes = (Width+7) div 8,
			    case file:read(Fd, Bytes) of
				{ok,Buf} ->
				    ?verbose("loaded: ~w bytes, ~w bits\n",
					     [byte_size(Buf), bit_size(Buf)]),
				    ?verbose("buf: ~p\n", [Buf]),
				    epx:bitmap_put_bits(BM,0,0,bit_size(Buf),1,Buf);
				eof -> %% no blocks yet
				    ok
			    end,
			    {Fd,BS,BM};
			Error ->
			    Error
		    end;
		Error -> Error
	    end
    end.


-spec reopen(Name::file:name_all(), BMap::bmap()) -> BMap1::bmap().
%% re-open a close file reusing the bitmap, assumes to be the same
%% as the state on disk

reopen(Name, {_Fd,BS,Bitmap}) ->
    case filelib:is_file(Name) of
	false -> error(enoent);
	true ->
	    case file:open(Name, [read,write,raw,binary]) of
		{ok,Fd} ->
		    case file:read(Fd,?HEADER_SIZE) of
			{ok,<<?HEADER(BS,_MaxSize),_/binary>>} ->
			    {Fd,BS,Bitmap}
		    end;
		Error -> Error
	    end
    end.



create_bm(BS,MaxSize) ->
    Width = (MaxSize+BS-1) div BS,
    epx:bitmap_create(Width, 1, 2#00000000).

-spec close(BMap::bmap()) -> ok.
close({Fd,_,_}) ->
    file:close(Fd).

-spec mark(BMap::bmap(), Offs::unsigned()) -> ok.
mark(BM, Offs) ->
    set_block(BM, Offs, 1).
-spec mark(BMap::bmap(), Offs::unsigned(), Len::unsigned()) -> ok.
mark(BM, Offs, Len) ->
    set_block_range(BM, Offs, Len, 2#11111111).

-spec clear(BMap::bmap(), Offs::unsigned()) -> ok.
clear(BM, Offs) ->
    set_block(BM, Offs, 0).
-spec clear(BMap::bmap(), Offs::unsigned(), Len::unsigned()) -> ok.
clear(BM, Offs, Len) ->
    set_block_range(BM, Offs, Len, 2#00000000).

set_block({Fd,BS,Bitmap}, Offs, OnOff) ->
    A = Offs div BS,
    epx:bitmap_put_bit(Bitmap, A, 0, OnOff),
    A1 = A band (bnot 7),  %% bit starting the byte A
    Buf = epx:bitmap_get_bits(Bitmap,A1,0,8,1),
    ?verbose("set_block:~w, pwrite: at ~w buf: ~p\n", [OnOff, A1 div 8, Buf]),
    file:pwrite(Fd,?HEADER_SIZE+(A1 div 8),Buf),
    file:datasync(Fd),     %% we ignore update of access time...
    ok.

set_block_range({Fd,BS,Bitmap}, Offs, Len, OnOff) ->
    A = Offs div BS,
    B = (Offs+Len-1) div BS,
    ABLen = B-A+1,
    %% mark all blocks
    epx:bitmap_fill_rectangle(Bitmap,A,0,ABLen,1,OnOff),
    %% byte align 
    A1 = A band (bnot 7),  %% bit starting the byte A
    B1 = B band (bnot 7),  %% bit starting the byte B
    AB1Len = (((B1 div 8)-(A1 div 8))+1)*8,
    Buf = epx:bitmap_get_bits(Bitmap,A1,0,AB1Len,1),
    ?verbose("set_block_range;~w, pwrite: at ~w buf: ~p\n",
	     [OnOff, A1 div 8, Buf]),
    file:pwrite(Fd,?HEADER_SIZE+(A1 div 8),Buf),
    file:datasync(Fd),     %% we ignore update of access time...
    ok.
    
%% print block numbers updated, fixme: scan faster
-spec scan(BMap::bmap()) -> unsigned().
scan({_Fd,_BS,Bitmap}) ->
    N = epx:bitmap_info(Bitmap, width),
    scan_(0, 0, N, Bitmap).

scan_(I, M, N, Bitmap) when I < N ->
    case epx:bitmap_get_bit(Bitmap,I,0) of
	0 ->
	    scan_(I+1,M,N,Bitmap);
	1 ->
	    io:format("block ~w\n", [I]),
	    scan_(I+1,M+1,N,Bitmap)
    end;
scan_(_I, M, _N, _Bitmap) ->
    M.

-ifdef(TEST).
-define(TEST_FILE, "image.bmap").

create() ->
    create(?TEST_FILE,4096, ?GB(2)).

open() ->
    open(?TEST_FILE).

-endif.
