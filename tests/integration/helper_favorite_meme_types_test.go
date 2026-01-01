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

const favoriteMemeTestImageURL = "https://picsum.photos/100"

// favoriteMemeResponse represents the API response for a favorite meme.
type favoriteMemeResponse struct {
	ID           string   `json:"id"`
	UserID       string   `json:"user_id"`
	Name         string   `json:"name"`
	AltText      *string  `json:"alt_text"`
	Tags         []string `json:"tags"`
	AttachmentID string   `json:"attachment_id"`
	Filename     string   `json:"filename"`
	ContentType  string   `json:"content_type"`
	ContentHash  *string  `json:"content_hash"`
	Size         int64    `json:"size"`
	Width        *int     `json:"width"`
	Height       *int     `json:"height"`
	Duration     *float64 `json:"duration"`
	URL          string   `json:"url"`
	IsGifv       bool     `json:"is_gifv"`
	TenorID      *string  `json:"tenor_id"`
	CreatedAt    string   `json:"created_at"`
}
