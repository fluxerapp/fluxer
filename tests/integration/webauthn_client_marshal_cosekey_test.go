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
	"crypto/ecdsa"
	"github.com/go-webauthn/webauthn/protocol/webauthncbor"
	"testing"
)

func marshalCOSEKey(t testing.TB, pub *ecdsa.PublicKey) []byte {
	t.Helper()
	xBytes := padCoordinate(pub.X.Bytes())
	yBytes := padCoordinate(pub.Y.Bytes())
	key := map[int]any{
		1:  2,
		3:  -7,
		-1: 1,
		-2: xBytes,
		-3: yBytes,
	}
	data, err := webauthncbor.Marshal(key)
	if err != nil {
		t.Fatalf("failed to marshal cose key: %v", err)
	}
	return data
}
