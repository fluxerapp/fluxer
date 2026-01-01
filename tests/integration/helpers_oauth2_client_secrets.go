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
	"sync"
)

// Track client secrets by application ID to enforce confidential-client flows.
var oauthClientSecrets sync.Map // map[string]string

// storeClientSecret stores a client secret for an OAuth2 application.
func storeClientSecret(appID string, clientSecret string) {
	if appID != "" && clientSecret != "" {
		oauthClientSecrets.Store(appID, clientSecret)
	}
}
