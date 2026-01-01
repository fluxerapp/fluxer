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

type tenorGIF struct {
	ID       string `json:"id"`
	Title    string `json:"title"`
	URL      string `json:"url"`
	Src      string `json:"src"`
	ProxySrc string `json:"proxy_src"`
	Width    int    `json:"width"`
	Height   int    `json:"height"`
}

type registerTenorShareRequest struct {
	ID     string  `json:"id"`
	Q      *string `json:"q"`
	Locale string  `json:"locale,omitempty"`
}
