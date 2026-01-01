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
	"crypto/rand"
	"encoding/base32"
	"testing"
)

func newTotpSecret(t testing.TB) string {
	t.Helper()
	buf := make([]byte, 20)
	if _, err := rand.Read(buf); err != nil {
		t.Fatalf("failed to generate totp secret: %v", err)
	}
	return base32.StdEncoding.WithPadding(base32.NoPadding).EncodeToString(buf)
}
