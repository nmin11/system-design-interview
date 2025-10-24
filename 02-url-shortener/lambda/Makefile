.PHONY: build clean

build:
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -tags lambda.norpc -o bootstrap main.go base62.go mongodb.go
	zip bootstrap.zip bootstrap

clean:
	rm -f bootstrap bootstrap.zip
