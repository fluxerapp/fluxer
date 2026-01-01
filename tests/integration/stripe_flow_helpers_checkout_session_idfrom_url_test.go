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
	"net/url"
	"strings"
	"testing"
)

// CheckoutSessionIDFromURL extracts the session id from a Stripe checkout URL.
func CheckoutSessionIDFromURL(t testing.TB, checkoutURL string) string {
	t.Helper()

	parsed, err := url.Parse(checkoutURL)
	if err != nil {
		t.Fatalf("invalid checkout URL %q: %v", checkoutURL, err)
	}

	segments := strings.Split(parsed.Path, "/")
	for _, segment := range segments {
		if strings.HasPrefix(segment, "cs_") {
			return segment
		}
	}

	t.Fatalf("unable to extract checkout session id from %s", checkoutURL)
	return ""
}
