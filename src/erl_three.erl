-module(erl_three).
-export([start/0, save/1]).

% This code inspired by Alexander Samoylovich and fprog.ru team.
% http://fprog.ru/2009/issue2/alexander-samoylovich-erlang-crawler/

get_start_base() -> "http://airwar.ru/".
get_start_url() -> get_start_base() ++ "image/".
out_file_name() -> "pictures.txt".

start() ->
  inets:start(),
  Writer = start_write(),
  process_page(Writer, get_start_url()),
  stop_write(Writer).

start_write() ->
  spawn(fun write_proc/0).

stop_write(W) ->
  W ! stop.

write(W, String) ->
  W ! {write, String}.

write_proc() -> write_loop([], 0).

write_loop(Data, DataLen) ->
  receive
    stop ->
      io:format("Saving ~b entries~n", [DataLen]),
      {ok, F} = file:open(out_file_name(), write),
      [io:format(F, "~s~n", [S]) || S <- Data],
      file:close(F),
      io:format("Done~n");
    {write, String} ->
      case DataLen rem 1000 of
        0 -> io:format("Downloaded: ~p~n", [DataLen]);
        _ -> ok
      end,
      write_loop([String|Data], 1 + DataLen)
  after 10000 ->
    io:format("Stop on timeout~n"),
    stop_write(self()),
    write_loop(Data, DataLen) 
  end.                

process_page(W, Url) ->
  MyPid = self(),
  case get_url_contents(Url) of
    {ok, Data} ->
      Strings = string:tokens(Data, "\n"),
      Pids = [spawn(fun() ->
                process_string(W, MyPid, Url, Str)
              end) || Str <- Strings],
      collect(length(Pids));
    _ -> ok
  end.

collect(0) -> ok;
collect(N) ->
  receive
    done -> collect(N - 1)
  end.

get_url_contents(Url) -> get_url_contents(Url, 5).
get_url_contents(_, 0) -> failed;
get_url_contents(Url, MaxFailures) ->
  case httpc:request(Url) of
    {ok, {{_, RetCode, _}, _, Result}} -> 
      if 
        RetCode == 200; RetCode == 201 -> {ok, Result};
        RetCode >= 500 ->
          timer:sleep(1000),
          get_url_contents(Url, MaxFailures - 1);
        true -> failed
      end;
    {error, _Why} ->
      timer:sleep(1000),
      get_url_contents(Url, MaxFailures - 1)
  end.

process_string(W, Parent, Dir, Str) ->
  case extract_link(Str) of
    {ok, Url} -> process_link(W, Dir, Url);
    failed    -> ok
  end,
  done(Parent).

done(Parent) ->
  Parent ! done.

extract_link(S) ->
  case re:run(S, "href *= *([^>]*)>", [{capture, all_but_first, list}]) of
    {match, [Link]} -> {ok, string:strip(Link, both, $")};
    _               -> failed
  end.

process_link(W, Dir, Url) ->
  case get_link_type(Url) of
    image -> process_image(W, Dir ++ Url);
    page  -> process_page(W, Dir ++ Url);
    _     -> process_other(W, Dir ++ Url)
  end.

get_link_type(Url) ->
  {ok, ReImg}  = re:compile("\\.(gif|jpg|jpeg|png)",
                            [extended, caseless]),
  {ok, RePage} = re:compile("^[^/]+/$"),
  case re:run(Url, ReImg) of
    {match, _} -> image;
    _          -> case re:run(Url, RePage) of
                    {match, _} -> page;
                    _          -> strange
                  end
  end.

process_image(W, Url) ->
  write(W, Url).

process_other(_, _) ->
  ok.

save(Path) ->
  inets:start(),
  {ok, Data} = file:read_file(Path),
  L = string:tokens(binary_to_list(Data), "\n"),
  save_loop(0, L).

save_loop(0, []) ->
  io:format("saving done~n", []);
save_loop(Running, []) ->
  receive
    done ->
      io:format("to save: ~p~n", [Running]),
      save_loop(Running - 1, [])
    end;
save_loop(Running, [U|Us]) when Running < 200 ->
  S = self(),
  spawn(fun () -> save_url(S, U) end),
  save_loop(Running + 1, Us);
save_loop(Running, Us) ->
  receive
    done ->
      io:format("to save: ~p~n", [Running + length(Us)]),
      save_loop(Running - 1 , Us)
  end.

save_url(Parent, Url) ->
  Path = url_to_path(Url),
  ensure_dir(".", filename:split(Path)),
  case get_url_contents(Url) of
    {ok, Data} ->
      file:write_file(Path, Data);
    _ -> ok
  end,
  done(Parent).

url_to_path(Url) ->
  string:substr(Url, length(get_start_base()) + 1).

ensure_dir(_, [_FN]) -> ok;
ensure_dir(Dir, [NextDir|RestDirs]) ->
  DirName = filename:join(Dir, NextDir),
  file:make_dir(DirName),
  ensure_dir(DirName, RestDirs).