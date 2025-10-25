package main

import "context"

type URLRepository interface {
	SaveURL(ctx context.Context, mapping URLMapping) error
	GetURLByShortURL(ctx context.Context, shortURL string) (*URLMapping, error)
	Close(ctx context.Context) error
}
