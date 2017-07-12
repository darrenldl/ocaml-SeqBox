default:
	jbuilder build @install

install:
	cp _build/default/src/osbx.exe $(opam config var bin)/osbx
