.PHONY: build clean test

build:
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -tags lambda.norpc -o bootstrap main.go base62.go mongodb.go repository.go
	zip bootstrap.zip bootstrap

test:
	MONGODB_URI=mongodb://localhost:27017/test go test -v

test-coverage:
	MONGODB_URI=mongodb://localhost:27017/test go test -cover

clean:
	rm -f bootstrap bootstrap.zip
