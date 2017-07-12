default:
	jbuilder make @install

install:
	cp _build/default/src/osbx.exe $(opam config var bin)/osbx
