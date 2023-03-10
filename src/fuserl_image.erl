-module (fuserl_image).
-behaviour (application).
-export ([ start/0,
           start/2,
           stop/0,
           stop/1 ]).

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start () ->
    application:start (fuserl),
    application:start (fuserl_image).

%% @hidden

start (_Type, _Args) ->
    io:format("fuserlimage: start called\n", []),
    Args = application:get_all_env(fuserl_image),
    case proplists:get_value(make_mount_point,Args) of
	undefined -> ok;
	false -> ok;
	true ->
	    MountPoint = proplists:get_value(mount_point,Args),
	    io:format("make_dir: ~p\n", [MountPoint]),
	    case file:make_dir(MountPoint) of
		ok -> ok;
		{ error, eexist } -> ok
	    end
    end,
    fuserl_image_sup:start_link(Args).

%% @hidden

stop () ->
  application:stop (fuserl_image).

%% @hidden

stop (_State) ->
  ok.
