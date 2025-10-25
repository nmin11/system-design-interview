package main

import (
	"context"
	"fmt"
)

type MockRepository struct {
	storage map[string]*URLMapping
	saveErr error
	getErr  error
}

// NewMockRepository creates a new mock repository
func NewMockRepository() *MockRepository {
	return &MockRepository{
		storage: make(map[string]*URLMapping),
	}
}

// SaveURL stores a URL mapping in memory
func (m *MockRepository) SaveURL(ctx context.Context, mapping URLMapping) error {
	if m.saveErr != nil {
		return m.saveErr
	}
	m.storage[mapping.ShortURL] = &mapping
	return nil
}

// GetURLByShortURL retrieves a URL mapping by short URL
func (m *MockRepository) GetURLByShortURL(ctx context.Context, shortURL string) (*URLMapping, error) {
	if m.getErr != nil {
		return nil, m.getErr
	}
	mapping, exists := m.storage[shortURL]
	if !exists {
		return nil, nil
	}
	return mapping, nil
}

// Close does nothing for mock
func (m *MockRepository) Close(ctx context.Context) error {
	return nil
}

// SetSaveError configures the mock to return an error on SaveURL
func (m *MockRepository) SetSaveError(err error) {
	m.saveErr = err
}

// SetGetError configures the mock to return an error on GetURLByShortURL
func (m *MockRepository) SetGetError(err error) {
	m.getErr = err
}

// AddMapping adds a mapping directly to the storage (for test setup)
func (m *MockRepository) AddMapping(mapping URLMapping) {
	m.storage[mapping.ShortURL] = &mapping
}

// Clear removes all mappings
func (m *MockRepository) Clear() {
	m.storage = make(map[string]*URLMapping)
	m.saveErr = nil
	m.getErr = nil
}

// ErrMockDatabase is a sample error for testing
var ErrMockDatabase = fmt.Errorf("mock database error")
