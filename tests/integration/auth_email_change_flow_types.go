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

type emailChangeStartResponse struct {
	Ticket             string  `json:"ticket"`
	RequireOriginal    bool    `json:"require_original"`
	OriginalProof      *string `json:"original_proof,omitempty"`
	OriginalCodeExpiry *string `json:"original_code_expires_at,omitempty"`
	ResendAvailableAt  *string `json:"resend_available_at,omitempty"`
}

type emailChangeVerifyOriginalResponse struct {
	OriginalProof string `json:"original_proof"`
}

type emailChangeRequestNewResponse struct {
	Ticket            string  `json:"ticket"`
	NewEmail          string  `json:"new_email"`
	NewCodeExpiresAt  string  `json:"new_code_expires_at"`
	ResendAvailableAt *string `json:"resend_available_at,omitempty"`
}

type emailChangeVerifyNewResponse struct {
	EmailToken string `json:"email_token"`
}
