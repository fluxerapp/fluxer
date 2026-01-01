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
	"encoding/json"
	"net/http"
)

func (c *testClient) requestJSON(method, path string, payload any, token string) (*http.Response, error) {
	var body *bytes.Buffer
	if payload != nil {
		body = &bytes.Buffer{}
		if err := json.NewEncoder(body).Encode(payload); err != nil {
			return nil, err
		}
	} else {
		body = &bytes.Buffer{}
	}

	req, err := http.NewRequest(method, c.baseURL+path, body)
	if err != nil {
		return nil, err
	}

	if payload != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	c.applyCommonHeaders(req)
	if token != "" {
		req.Header.Set("Authorization", token)
	}
	return c.httpClient.Do(req)
}
