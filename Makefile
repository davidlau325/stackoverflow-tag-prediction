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

.PHONT:clean_crawl
clean_crawl:
	@rm -r crawl || (echo "No crawled file found")
	@rm -r test || (echo "No test file found")
	@echo "Clean crawl file done!"

.PHONY:clean_extract
clean_extract:
	@rm -r words || (echo "No words directory found")
	@rm -r tags || (echo "No tags directory found")
	@echo "Clean extract file done!"


