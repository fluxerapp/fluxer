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

type CreateCheckoutRequest struct {
	PriceID string `json:"price_id"`
}

type CheckoutResponse struct {
	URL string `json:"url"`
}

type premiumPriceIDs struct {
	Monthly       *string `json:"monthly"`
	Yearly        *string `json:"yearly"`
	Visionary     *string `json:"visionary"`
	Gift1Month    *string `json:"gift1Month"`
	Gift1Year     *string `json:"gift1Year"`
	GiftVisionary *string `json:"giftVisionary"`
	Currency      string  `json:"currency"`
}
