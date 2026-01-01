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
	"encoding/base64"
	"testing"
)

func decodeBase64URL(t testing.TB, value string) []byte {
	t.Helper()
	decoders := []*base64.Encoding{base64.RawURLEncoding, base64.URLEncoding, base64.RawStdEncoding, base64.StdEncoding}
	for _, enc := range decoders {
		if b, err := enc.DecodeString(value); err == nil {
			return b
		}
	}
	t.Fatalf("failed to decode base64 value: %s", value)
	return nil
}
