all:
	./jabbachipbuild main.native

test:
	python tests/test.py

clean:
	rm -rf _build
	rm *.native
