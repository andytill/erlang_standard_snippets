
%%% esnipper takes a list of module atoms `esnipper:main([lists,dict])` and
%%% generates a snippet file for each exported function in those modules.

-module(esnipper).
-export([main/1]).

%%
main(Module_list) ->
    Module_data = [module_data(M) || M <- Module_list, is_atom(M)],
    [write_module_data(Data) || Data <- Module_data],
    io:format("Snippets for ~p have been generated~n", [Module_list]).

%%
module_data(Module) ->
    Exports = Module:module_info(exports),
    case abstract_code(Module) of
        {error, Reason} ->
            io:format("Could not get abstract code for module ~p becuz ~p",
                [Module, Reason]),
            {Module, Exports};
        {ok, AST} ->
            {Module, [{F, function_args_from_ast(F,A,AST)} || {F, A} <- Exports, F=/=module_info]}
    end.

%%
function_args_from_ast(F,A,AST) when is_atom(F), is_integer(A)->
    Filter_fn = 
        fun({attribute,_,spec,{{X_F, X_A}, _}}) -> 
                (X_F == F andalso X_A == A);
           (_) ->
                false
        end,
    Spec = lists:filter(Filter_fn, AST),
    % io:format("Filtered spec for ~p, ~p~n", [F, Spec]),
    case Spec of
        [] ->
            default_arg_names(A);
        [{attribute,_,spec,{{F, A}, Args_spec}} | _] ->
            arg_names_from_args_spec(A,Args_spec)
    end.

%%
arg_names_from_args_spec(0, _) ->
    [];
arg_names_from_args_spec(Arity, [{type,_, bounded_fun, Spec} | _]) ->
    arg_names_from_args_spec(Arity, Spec);
arg_names_from_args_spec(_Arity, [{type, _, 'fun', [{type,_,product,Args} | _]} | _]) ->
    [arg_name(A) || A <- Args].

%%
arg_name({var,_,Var_name}) ->
    atom_to_list(Var_name);
arg_name({ann_type,_,[Annotated_arg | _]}) -> 
    % ann_type happens for specs like...
    % -spec update_element(Tab, Key, ElementSpec :: {Pos, Value}) -> boolean().
    % ...the last arg gets turned into the following AST...
    % {ann_type,430,
    %   [{var,430,'ElementSpec'},
    %    {type,430,tuple,[{var,430,'Pos'},{var,430,'Value'}]}]}
    % TODO it would be nice to offer the literal {Pos, Value} in the snippet instead of ElementSpec.
    arg_name(Annotated_arg); 
arg_name({type,_,Type_name,_}) ->
    % the type tuple happens when a type is given without a var name i.e.
    % -spec my_func(string()) -> ok.
    [First_char | Tail] = atom_to_list(Type_name),
    [Upper_first_char] = string:to_upper([First_char]),
    [Upper_first_char | Tail].

%%
default_arg_names(Arity) ->
    ["Arg_" ++ integer_to_list(N) || N <- lists:seq(1, Arity)].

%%
write_module_data({Module, Exports}) ->
    ok = filelib:ensure_dir([Module, "/"]),
    % io:format("exports ~p~n", [Exports]),
    [write_function_snippet(Module, F, A) || {F, A} <- Exports],
    ok.

%%
write_function_snippet(M, F, A) ->
    Snippet = snippet_text(M, F, A),
    Filename = ["./", M, "/", M, "-", F, "-", integer_to_list(length(A)), ".sublime-snippet"],
    ok = file:write_file(Filename, Snippet).

%%
snippet_text(Module, Function, Args) ->
    M = atom_to_list(Module),
    F = atom_to_list(Function),
    A = integer_to_list(length(Args)),
    [
    "<snippet>\n",
    "    <content><![CDATA[\n",
    M, ":", F, "(", args_text(Args), ")\n",
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
args_text(_, [], Acc) ->
    Acc;
args_text(Arg_num, [Arg | Tail], "") ->
    Acc = "${1:" ++ Arg ++ "}",
    args_text(Arg_num + 1, Tail, Acc);
args_text(Arg_num, [Arg | Tail], Acc_1) ->
    AN = integer_to_list(Arg_num),
    Acc_2 = Acc_1 ++ [", ${", AN, ":", Arg, "}"],
    args_text(Arg_num + 1, Tail, Acc_2).

%%
abstract_code(Module) ->
    File = code:which(Module),
    case beam_lib:chunks(File, [abstract_code]) of 
        {ok,{_Mod,[{abstract_code,no_abstract_code}]}} ->
            {error, no_abstract_code};
        {ok,{_Mod,[{abstract_code,{_Version,Forms}}]}} ->
            {ok, Forms}
    end.