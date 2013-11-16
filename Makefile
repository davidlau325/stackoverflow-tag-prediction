crawler:
	corebuild -pkg cohttp.async,re2 crawler.native

extract:
	corebuild extract.native

.PHONY:clean_build
clean_build:
	@rm -r _build || (echo "The build is already clean")
	@rm crawler.native || (echo "No crawler.native found")
	@rm extract.native || (echo "No extract.native found")
	@echo "Clean build file done!"

.PHONY:clean_extract
clean_extract:
	@rm -r title || (echo "No title directory found")
	@rm -r content || (echo "No content directory found")
	@rm -r tags || (echo "No tags directory found")
	@rm *.txt || (echo "No crawled txt file found")
	@echo "Clean extract file done!"


