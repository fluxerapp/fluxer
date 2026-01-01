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

package integration

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/base32"
	"encoding/binary"
	"fmt"
	"strings"
	"testing"
	"time"
)

func totpCodeAt(t testing.TB, secret string, at time.Time) string {
	t.Helper()
	decoder := base32.StdEncoding.WithPadding(base32.NoPadding)
	normalized := strings.ToUpper(strings.ReplaceAll(secret, " ", ""))
	key, err := decoder.DecodeString(normalized)
	if err != nil {
		t.Fatalf("failed to decode totp secret: %v", err)
	}

	counter := uint64(at.Unix() / 30)
	var counterBytes [8]byte
	binary.BigEndian.PutUint64(counterBytes[:], counter)

	mac := hmac.New(sha1.New, key)
	if _, err := mac.Write(counterBytes[:]); err != nil {
		t.Fatalf("failed to compute totp hmac: %v", err)
	}
	sum := mac.Sum(nil)
	offset := sum[len(sum)-1] & 0x0f
	code := ((int(sum[offset]) & 0x7f) << 24) |
		((int(sum[offset+1]) & 0xff) << 16) |
		((int(sum[offset+2]) & 0xff) << 8) |
		(int(sum[offset+3]) & 0xff)
	code %= 1000000
	return fmt.Sprintf("%06d", code)
}
