package main

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"

	"github.com/aws/aws-lambda-go/events"
	"github.com/aws/aws-lambda-go/lambda"
	"github.com/bwmarrin/snowflake"
)

var (
	repo     URLRepository
	snowNode *snowflake.Node
)

type ShortenRequest struct {
	URL string `json:"url"`
}

type ShortenResponse struct {
	ShortURL string `json:"shortUrl"`
}

func init() {
	var err error

	repo, err = NewMongoClient()
	if err != nil {
		panic(fmt.Sprintf("Failed to initialize MongoDB client: %v", err))
	}

	snowNode, err = snowflake.NewNode(1)
	if err != nil {
		panic(fmt.Sprintf("Failed to create snowflake node: %v", err))
	}
}

func handler(ctx context.Context, request events.LambdaFunctionURLRequest) (events.LambdaFunctionURLResponse, error) {
	method := request.RequestContext.HTTP.Method
	path := request.RequestContext.HTTP.Path

	if method == "POST" && path == "/shorten" {
		return handleShorten(ctx, request)
	}

	if method == "GET" && path != "/" && path != "/shorten" {
		shortURL := strings.TrimPrefix(path, "/")
		return handleRedirect(ctx, shortURL)
	}

	return events.LambdaFunctionURLResponse{
		StatusCode: http.StatusNotFound,
		Body:       "Not Found",
	}, nil
}

func handleShorten(ctx context.Context, request events.LambdaFunctionURLRequest) (events.LambdaFunctionURLResponse, error) {
	var req ShortenRequest
	if err := json.Unmarshal([]byte(request.Body), &req); err != nil {
		return events.LambdaFunctionURLResponse{
			StatusCode: http.StatusBadRequest,
			Body:       fmt.Sprintf(`{"error": "Invalid request body: %v"}`, err),
			Headers: map[string]string{
				"Content-Type": "application/json",
			},
		}, nil
	}

	if req.URL == "" {
		return events.LambdaFunctionURLResponse{
			StatusCode: http.StatusBadRequest,
			Body:       `{"error": "URL is required"}`,
			Headers: map[string]string{
				"Content-Type": "application/json",
			},
		}, nil
	}

	id := snowNode.Generate()

	shortURL := base62Encode(id.Int64())

	mapping := URLMapping{
		ID:       id.Int64(),
		ShortURL: shortURL,
		LongURL:  req.URL,
	}

	err := repo.SaveURL(ctx, mapping)
	if err != nil {
		return events.LambdaFunctionURLResponse{
			StatusCode: http.StatusInternalServerError,
			Body:       fmt.Sprintf(`{"error": "Failed to save URL: %v"}`, err),
			Headers: map[string]string{
				"Content-Type": "application/json",
			},
		}, nil
	}

	response := ShortenResponse{
		ShortURL: shortURL,
	}
	responseBody, _ := json.Marshal(response)

	return events.LambdaFunctionURLResponse{
		StatusCode: http.StatusOK,
		Body:       string(responseBody),
		Headers: map[string]string{
			"Content-Type": "application/json",
		},
	}, nil
}

func handleRedirect(ctx context.Context, shortURL string) (events.LambdaFunctionURLResponse, error) {
	mapping, err := repo.GetURLByShortURL(ctx, shortURL)
	if err != nil {
		return events.LambdaFunctionURLResponse{
			StatusCode: http.StatusInternalServerError,
			Body:       fmt.Sprintf("Database error: %v", err),
		}, nil
	}
	if mapping == nil {
		return events.LambdaFunctionURLResponse{
			StatusCode: http.StatusNotFound,
			Body:       "Short URL not found",
		}, nil
	}

	return events.LambdaFunctionURLResponse{
		StatusCode: http.StatusFound,
		Headers: map[string]string{
			"Location": mapping.LongURL,
		},
	}, nil
}

func main() {
	lambda.Start(handler)
}
