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
	"testing"
	"time"
)

// WaitForGiftCode polls until a gift code matches the predicate.
func WaitForGiftCode(
	t testing.TB,
	client *testClient,
	token string,
	timeout time.Duration,
	predicate func(g GiftCodeMetadataResponse) bool,
) GiftCodeMetadataResponse {
	t.Helper()
	var matched GiftCodeMetadataResponse
	waitForCondition(t, timeout, func() (bool, error) {
		gifts := ListUserGifts(t, client, token)
		for _, gift := range gifts {
			if predicate(gift) {
				matched = gift
				return true, nil
			}
		}
		return false, nil
	})
	return matched
}
