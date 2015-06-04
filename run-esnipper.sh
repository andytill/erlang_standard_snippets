
erlc +debug_info esnipper.erl

erl -noinput -s esnipper main $* -eval "halt()."