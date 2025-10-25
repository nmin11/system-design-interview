package main

import "testing"

func TestBase62Encode(t *testing.T) {
	tests := []struct {
		name     string
		input    int64
		expected string
	}{
		{
			name:     "zero",
			input:    0,
			expected: "0",
		},
		{
			name:     "single digit",
			input:    5,
			expected: "5",
		},
		{
			name:     "small number",
			input:    61,
			expected: "z",
		},
		{
			name:     "base boundary",
			input:    62,
			expected: "10",
		},
		{
			name:     "medium number",
			input:    12345,
			expected: "3D7",
		},
		{
			name:     "large number",
			input:    123456789,
			expected: "8M0kX",
		},
		{
			name:     "snowflake ID example",
			input:    1234567890123456,
			expected: "5eZG7YB1s",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := base62Encode(tt.input)
			if result != tt.expected {
				t.Errorf("base62Encode(%d) = %s, want %s", tt.input, result, tt.expected)
			}
		})
	}
}

func TestBase62Decode(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected int64
	}{
		{
			name:     "zero",
			input:    "0",
			expected: 0,
		},
		{
			name:     "single digit",
			input:    "5",
			expected: 5,
		},
		{
			name:     "small number",
			input:    "z",
			expected: 61,
		},
		{
			name:     "base boundary",
			input:    "10",
			expected: 62,
		},
		{
			name:     "medium number",
			input:    "3D7",
			expected: 12345,
		},
		{
			name:     "large number",
			input:    "8M0kX",
			expected: 123456789,
		},
		{
			name:     "snowflake ID example",
			input:    "5eZG7YB1s",
			expected: 1234567890123456,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := base62Decode(tt.input)
			if result != tt.expected {
				t.Errorf("base62Decode(%s) = %d, want %d", tt.input, result, tt.expected)
			}
		})
	}
}

func TestBase62RoundTrip(t *testing.T) {
	tests := []int64{
		0,
		1,
		61,
		62,
		63,
		1000,
		123456789,
		1234567890123456,
		9223372036854775807, // max int64
	}

	for _, num := range tests {
		t.Run("roundtrip", func(t *testing.T) {
			encoded := base62Encode(num)
			decoded := base62Decode(encoded)
			if decoded != num {
				t.Errorf("round trip failed: %d -> %s -> %d", num, encoded, decoded)
			}
		})
	}
}

func BenchmarkBase62Encode(b *testing.B) {
	for i := 0; i < b.N; i++ {
		base62Encode(1234567890123456)
	}
}

func BenchmarkBase62Decode(b *testing.B) {
	for i := 0; i < b.N; i++ {
		base62Decode("5eZG7YB1s")
	}
}
