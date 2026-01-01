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
	"bytes"
	"crypto/ecdsa"
	"crypto/sha256"
	"encoding/binary"
	"testing"
)

func (d *webAuthnDevice) buildRegistrationAuthData(t testing.TB) []byte {
	t.Helper()
	rpHash := sha256.Sum256([]byte(d.rpID))

	flags := byte(0x01 | 0x04 | 0x40)
	buf := &bytes.Buffer{}
	buf.Write(rpHash[:])
	buf.WriteByte(flags)

	if err := binary.Write(buf, binary.BigEndian, d.signCount); err != nil {
		t.Fatalf("failed to write sign count: %v", err)
	}

	aaguid := make([]byte, 16)
	buf.Write(aaguid)

	credID := d.credentialID
	if err := binary.Write(buf, binary.BigEndian, uint16(len(credID))); err != nil {
		t.Fatalf("failed to write credential id length: %v", err)
	}
	buf.Write(credID)

	pubKeyCBOR := marshalCOSEKey(t, d.privateKey.Public().(*ecdsa.PublicKey))
	buf.Write(pubKeyCBOR)

	return buf.Bytes()
}
