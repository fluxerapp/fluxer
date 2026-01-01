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
	"crypto/sha256"
	"encoding/binary"
	"fmt"
)

func (d *webAuthnDevice) buildAssertionAuthData() []byte {
	rpHash := sha256.Sum256([]byte(d.rpID))
	flags := byte(0x01 | 0x04)
	buf := &bytes.Buffer{}
	buf.Write(rpHash[:])
	buf.WriteByte(flags)

	d.signCount++
	if err := binary.Write(buf, binary.BigEndian, d.signCount); err != nil {
		panic(fmt.Sprintf("failed to write sign count: %v", err))
	}

	return buf.Bytes()
}
