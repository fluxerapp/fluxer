/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package utils

import (
	"bufio"
	"crypto/rand"
	"encoding/base32"
	"fmt"
	"net/url"
	"os"
	"strings"
	"time"
)

// FileExists checks if a file exists at the given path
func FileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// BoolString converts a boolean to a string ("true" or "false")
func BoolString(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

// FirstNonZeroInt returns the first non-zero integer from the provided values,
// or the default value if all are zero
func FirstNonZeroInt(values ...int) int {
	for _, v := range values {
		if v != 0 {
			return v
		}
	}
	return 0
}

// DefaultString returns the value if non-empty, otherwise returns the default
func DefaultString(value, defaultValue string) string {
	if strings.TrimSpace(value) == "" {
		return defaultValue
	}
	return value
}

// RandomString generates a random alphanumeric string of the given length
func RandomString(length int) string {
	const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	b := make([]byte, length)
	if _, err := rand.Read(b); err != nil {
		panic(err)
	}
	for i := range b {
		b[i] = charset[int(b[i])%len(charset)]
	}
	return string(b)
}

// RandomBase32 generates a random base32-encoded string (without padding)
func RandomBase32(byteLength int) string {
	b := make([]byte, byteLength)
	if _, err := rand.Read(b); err != nil {
		panic(err)
	}
	return strings.TrimRight(base32.StdEncoding.EncodeToString(b), "=")
}

// GenerateSnowflake generates a snowflake ID
// Format: timestamp (42 bits) + worker ID (10 bits) + sequence (12 bits)
func GenerateSnowflake() string {
	const fluxerEpoch = 1420070400000
	timestamp := time.Now().UnixMilli() - fluxerEpoch
	workerID := int64(0)
	sequence := int64(0)
	snowflake := (timestamp << 22) | (workerID << 12) | sequence
	return fmt.Sprintf("%d", snowflake)
}

// ValidateURL validates that a string is a valid URL
func ValidateURL(urlStr string) error {
	if urlStr == "" {
		return fmt.Errorf("URL cannot be empty")
	}
	parsedURL, err := url.Parse(urlStr)
	if err != nil {
		return fmt.Errorf("invalid URL: %w", err)
	}
	if parsedURL.Scheme == "" {
		return fmt.Errorf("URL must have a scheme (http:// or https://)")
	}
	if parsedURL.Host == "" {
		return fmt.Errorf("URL must have a host")
	}
	return nil
}

// ParseEnvFile parses a .env file and returns a map of key-value pairs
func ParseEnvFile(path string) (map[string]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	env := make(map[string]string)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		parts := strings.SplitN(line, "=", 2)
		if len(parts) != 2 {
			continue
		}

		key := strings.TrimSpace(parts[0])
		value := strings.TrimSpace(parts[1])

		if len(value) >= 2 {
			if (value[0] == '"' && value[len(value)-1] == '"') ||
				(value[0] == '\'' && value[len(value)-1] == '\'') {
				value = value[1 : len(value)-1]
			}
		}

		env[key] = value
	}

	return env, scanner.Err()
}
