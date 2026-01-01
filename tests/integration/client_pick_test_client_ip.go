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
	"fmt"
	"os"
	"time"
)

func pickTestClientIP() string {
	if ip := os.Getenv("FLUXER_TEST_IP"); ip != "" {
		return ip
	}

	buf := make([]byte, 1)
	if _, err := rand.Read(buf); err == nil {
		return fmt.Sprintf("198.51.100.%d", 10+int(buf[0])%200)
	}

	return fmt.Sprintf("198.51.100.%d", time.Now().UnixNano()%200+10)
}
