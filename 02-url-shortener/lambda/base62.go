package main

const base62Chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

func base62Encode(num int64) string {
	if num == 0 {
		return string(base62Chars[0])
	}

	encoded := ""
	base := int64(len(base62Chars))

	for num > 0 {
		remainder := num % base
		encoded = string(base62Chars[remainder]) + encoded
		num = num / base
	}

	return encoded
}

func base62Decode(encoded string) int64 {
	decoded := int64(0)
	base := int64(len(base62Chars))

	for _, char := range encoded {
		decoded = decoded * base
		for i, c := range base62Chars {
			if c == char {
				decoded += int64(i)
				break
			}
		}
	}

	return decoded
}
