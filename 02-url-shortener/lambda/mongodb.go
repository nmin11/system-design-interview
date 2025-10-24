package main

import (
	"context"
	"fmt"
	"os"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

type MongoClient struct {
	client     *mongo.Client
	collection *mongo.Collection
}

type URLMapping struct {
	ID       int64  `bson:"_id"`
	ShortURL string `bson:"short_url"`
	LongURL  string `bson:"long_url"`
}

func NewMongoClient() (*MongoClient, error) {
	mongoURI := os.Getenv("MONGODB_URI")
	if mongoURI == "" {
		return nil, fmt.Errorf("MONGODB_URI environment variable is not set")
	}

	clientOptions := options.Client().ApplyURI(mongoURI)
	client, err := mongo.Connect(context.Background(), clientOptions)
	if err != nil {
		return nil, fmt.Errorf("failed to connect to MongoDB: %w", err)
	}

	collection := client.Database("urlshortener").Collection("urls")

	return &MongoClient{
		client:     client,
		collection: collection,
	}, nil
}

func (mc *MongoClient) SaveURL(ctx context.Context, mapping URLMapping) error {
	_, err := mc.collection.InsertOne(ctx, mapping)
	if err != nil {
		return fmt.Errorf("failed to insert URL mapping: %w", err)
	}
	return nil
}

func (mc *MongoClient) GetURLByShortURL(ctx context.Context, shortURL string) (*URLMapping, error) {
	var mapping URLMapping
	err := mc.collection.FindOne(ctx, bson.M{"short_url": shortURL}).Decode(&mapping)
	if err == mongo.ErrNoDocuments {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to find URL mapping: %w", err)
	}
	return &mapping, nil
}

func (mc *MongoClient) Close(ctx context.Context) error {
	return mc.client.Disconnect(ctx)
}
