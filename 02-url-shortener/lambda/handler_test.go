package main

import (
	"context"
	"encoding/json"
	"testing"

	"github.com/aws/aws-lambda-go/events"
	"github.com/bwmarrin/snowflake"
)

func setupTest(t *testing.T) *MockRepository {
	t.Helper()

	var err error
	snowNode, err = snowflake.NewNode(1)
	if err != nil {
		t.Fatalf("Failed to create snowflake node: %v", err)
	}

	mockRepo := NewMockRepository()
	repo = mockRepo

	return mockRepo
}

func TestHandleShorten(t *testing.T) {
	tests := []struct {
		name           string
		requestBody    string
		expectedStatus int
		expectedError  bool
		mockError      error
	}{
		{
			name:           "valid request",
			requestBody:    `{"url": "https://example.com/very/long/url"}`,
			expectedStatus: 200,
			expectedError:  false,
		},
		{
			name:           "empty url",
			requestBody:    `{"url": ""}`,
			expectedStatus: 400,
			expectedError:  true,
		},
		{
			name:           "invalid json",
			requestBody:    `{invalid json}`,
			expectedStatus: 400,
			expectedError:  true,
		},
		{
			name:           "database error",
			requestBody:    `{"url": "https://example.com"}`,
			expectedStatus: 500,
			expectedError:  true,
			mockError:      ErrMockDatabase,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockRepo := setupTest(t)
			defer mockRepo.Clear()

			if tt.mockError != nil {
				mockRepo.SetSaveError(tt.mockError)
			}

			request := events.LambdaFunctionURLRequest{
				Body: tt.requestBody,
				RequestContext: events.LambdaFunctionURLRequestContext{
					HTTP: events.LambdaFunctionURLRequestContextHTTPDescription{
						Method: "POST",
						Path:   "/shorten",
					},
				},
			}

			response, err := handleShorten(context.Background(), request)

			if err != nil {
				t.Fatalf("handleShorten returned error: %v", err)
			}

			if response.StatusCode != tt.expectedStatus {
				t.Errorf("expected status %d, got %d", tt.expectedStatus, response.StatusCode)
			}

			if !tt.expectedError && response.StatusCode == 200 {
				var shortenResp ShortenResponse
				err := json.Unmarshal([]byte(response.Body), &shortenResp)
				if err != nil {
					t.Fatalf("failed to unmarshal response: %v", err)
				}

				if shortenResp.ShortURL == "" {
					t.Error("expected non-empty shortUrl in response")
				}

				// Verify it was saved to repository
				mapping, err := mockRepo.GetURLByShortURL(context.Background(), shortenResp.ShortURL)
				if err != nil {
					t.Fatalf("failed to get URL from repository: %v", err)
				}
				if mapping == nil {
					t.Fatal("expected mapping to be saved in repository")
				}
				if mapping.LongURL != "https://example.com/very/long/url" {
					t.Errorf("expected long URL to be saved, got %s", mapping.LongURL)
				}
			}
		})
	}
}

func TestHandleRedirect(t *testing.T) {
	tests := []struct {
		name           string
		shortURL       string
		setupMapping   *URLMapping
		expectedStatus int
		expectedURL    string
		mockError      error
	}{
		{
			name:     "valid short url",
			shortURL: "abc123",
			setupMapping: &URLMapping{
				ID:       123456,
				ShortURL: "abc123",
				LongURL:  "https://example.com/original",
			},
			expectedStatus: 302,
			expectedURL:    "https://example.com/original",
		},
		{
			name:           "non-existent short url",
			shortURL:       "notfound",
			setupMapping:   nil,
			expectedStatus: 404,
		},
		{
			name:           "database error",
			shortURL:       "error",
			setupMapping:   nil,
			expectedStatus: 500,
			mockError:      ErrMockDatabase,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockRepo := setupTest(t)
			defer mockRepo.Clear()

			if tt.setupMapping != nil {
				mockRepo.AddMapping(*tt.setupMapping)
			}

			if tt.mockError != nil {
				mockRepo.SetGetError(tt.mockError)
			}

			response, err := handleRedirect(context.Background(), tt.shortURL)

			if err != nil {
				t.Fatalf("handleRedirect returned error: %v", err)
			}

			if response.StatusCode != tt.expectedStatus {
				t.Errorf("expected status %d, got %d", tt.expectedStatus, response.StatusCode)
			}

			if tt.expectedStatus == 302 {
				location, ok := response.Headers["Location"]
				if !ok {
					t.Error("expected Location header in redirect response")
				}
				if location != tt.expectedURL {
					t.Errorf("expected Location header %s, got %s", tt.expectedURL, location)
				}
			}
		})
	}
}

func TestHandler(t *testing.T) {
	tests := []struct {
		name           string
		method         string
		path           string
		body           string
		setupMapping   *URLMapping
		expectedStatus int
	}{
		{
			name:           "POST /shorten",
			method:         "POST",
			path:           "/shorten",
			body:           `{"url": "https://example.com"}`,
			expectedStatus: 200,
		},
		{
			name:   "GET /{shortUrl}",
			method: "GET",
			path:   "/abc123",
			setupMapping: &URLMapping{
				ID:       123456,
				ShortURL: "abc123",
				LongURL:  "https://example.com",
			},
			expectedStatus: 302,
		},
		{
			name:           "GET / (root)",
			method:         "GET",
			path:           "/",
			expectedStatus: 404,
		},
		{
			name:           "GET /shorten",
			method:         "GET",
			path:           "/shorten",
			expectedStatus: 404,
		},
		{
			name:           "PUT /shorten",
			method:         "PUT",
			path:           "/shorten",
			expectedStatus: 404,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockRepo := setupTest(t)
			defer mockRepo.Clear()

			if tt.setupMapping != nil {
				mockRepo.AddMapping(*tt.setupMapping)
			}

			request := events.LambdaFunctionURLRequest{
				Body: tt.body,
				RequestContext: events.LambdaFunctionURLRequestContext{
					HTTP: events.LambdaFunctionURLRequestContextHTTPDescription{
						Method: tt.method,
						Path:   tt.path,
					},
				},
			}

			response, err := handler(context.Background(), request)

			if err != nil {
				t.Fatalf("handler returned error: %v", err)
			}

			if response.StatusCode != tt.expectedStatus {
				t.Errorf("expected status %d, got %d", tt.expectedStatus, response.StatusCode)
			}
		})
	}
}
