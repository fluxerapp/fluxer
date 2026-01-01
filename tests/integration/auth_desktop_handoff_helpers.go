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
	"regexp"
)

type handoffInitiateResponse struct {
	Code      string `json:"code"`
	ExpiresAt string `json:"expires_at"`
}

type handoffStatusResponse struct {
	Status string `json:"status"`
	Token  string `json:"token,omitempty"`
	UserID string `json:"user_id,omitempty"`
}

// validateHandoffCodeFormat checks that the code matches XXXX-XXXX format
// with uppercase letters and digits (excluding ambiguous characters 0/O, 1/I/L)
func validateHandoffCodeFormat(code string) bool {
	pattern := regexp.MustCompile(`^[ABCDEFGHJKMNPQRSTUVWXYZ23456789]{4}-[ABCDEFGHJKMNPQRSTUVWXYZ23456789]{4}$`)
	return pattern.MatchString(code)
}
