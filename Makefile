
get_deps:
	@./build_deps.sh

compile_nif: get_deps
	@make V=0 -C c_src -j 8

clean_nif:
	@make -C c_src clean

ct:
	mkdir -p log
	rebar3 ct --logdir log


