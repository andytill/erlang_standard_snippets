
%%% esnipper takes a list of module atoms `esnipper:main([lists,dict])` and
%%% generates a snippet file for each exported function in those modules.

-module(esnipper).
-export([main/1]).

%%
main(Module_list) ->
    Module_data = [module_data(M) || M <- Module_list, is_atom(M)],
    [write_module_data(Data) || Data <- Module_data],
    io:format("all done~n").

%%
module_data(Module) ->
    {Module, Module:module_info(exports)}.

%%
write_module_data({Module, Exports}) ->
    ok = filelib:ensure_dir([Module, "/"]),
    [write_function_snippet(Module, F, A) || {F, A} <- Exports],
    ok.

write_function_snippet(M, F, A) ->
    Snippet = snippet_text(M, F, A),
    Filename = ["./", M, "/", M, "-", F, "-", integer_to_list(A), ".sublime-snippet"],
    ok = file:write_file(Filename, Snippet).

%%
snippet_text(Module, Function, Arity) ->
    M = atom_to_list(Module),
    F = atom_to_list(Function),
    A = integer_to_list(Arity),
    [
    "<snippet>\n",
    "    <content><![CDATA[\n",
    M, ":", F, "(", args_text(Arity), ")\n",
    "]]></content>\n",
    "    <tabTrigger>", M, ":", F, "</tabTrigger>\n",
    "    <scope>source.erlang</scope>\n",
    "    <description>", M, ":", F, "/", A, "</description>\n"
    "</snippet>"
    ].

%%
args_text(Arity) ->
    args_text(1, Arity, "").

%%
args_text(_, 0, Acc) ->
    Acc;
args_text(Arg_num, Arity, Acc) when Arg_num > Arity ->
    Acc;
args_text(Arg_num, Arity, "") ->
    Acc = "${1:Arg_1}",
    args_text(Arg_num + 1, Arity, Acc);
args_text(Arg_num, Arity, Acc_1) ->
    AN = integer_to_list(Arg_num),
    Acc_2 = Acc_1 ++ [", ${", AN, ":Arg_", AN, "}"],
    args_text(Arg_num + 1, Arity, Acc_2).

