all: cvpartner/README.md

cvpartner/README.md: cvpartner-api.yml
	rm -rf cvpartner
	bin/openapi-generator-cli generate -g haskell-http-client -i cvpartner-api.yml -o cvpartner
