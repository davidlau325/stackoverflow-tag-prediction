native:
	corebuild -pkg cohttp.async,re2 crawler.native

.PHONY:clean
clean:
	@rm -r _build || (echo "The build is already clean"; exit 1;)
	@rm crawler.native
	@echo "Clean!"
