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

// userDataExistsResponse represents the response from /test/users/:userId/data-exists
type userDataExistsResponse struct {
	UserExists         bool    `json:"user_exists"`
	EmailCleared       bool    `json:"email_cleared"`
	PhoneCleared       bool    `json:"phone_cleared"`
	PasswordCleared    bool    `json:"password_cleared"`
	Flags              string  `json:"flags"`
	HasDeletedFlag     bool    `json:"has_deleted_flag"`
	HasSelfDeletedFlag bool    `json:"has_self_deleted_flag"`
	PendingDeletionAt  *string `json:"pending_deletion_at"`
	RelationshipsCount int     `json:"relationships_count"`
	SessionsCount      int     `json:"sessions_count"`
	OAuthTokensCount   int     `json:"oauth_tokens_count"`
	PinnedDmsCount     int     `json:"pinned_dms_count"`
	SavedMessagesCount int     `json:"saved_messages_count"`
}
