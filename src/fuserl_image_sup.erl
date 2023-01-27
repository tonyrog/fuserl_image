-module (fuserl_image_sup).
-behaviour (supervisor).

-export ([ start_link/1, init/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link(Args) ->
    supervisor:start_link (?MODULE, [Args]).

%-=====================================================================-
%-                         supervisor callbacks                        -
%-=====================================================================-

%% @hidden

init([Args]) ->
  { ok,
    { { one_for_one, 3, 10 },
      [
        { fuserl_image_srv,
          { fuserl_image_srv, start_link, [ Args ] },
          permanent,
          10000,
          worker,
          [ fuserl_image_srv ]
        }
      ]
    }
  }.
