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
	"encoding/json"
	"fmt"
	"strings"
	"testing"
	"time"
)

func drainRelationshipEvents(t testing.TB, gw interface {
	NextDispatch(timeout time.Duration) (string, json.RawMessage)
}) {
	t.Helper()
	timeout := 100 * time.Millisecond
	defer func() {
		if r := recover(); r != nil {
			if !strings.Contains(fmt.Sprint(r), "context deadline exceeded") {
				panic(r)
			}
		}
	}()
	for {
		eventName, _ := gw.NextDispatch(timeout)
		if eventName == "" {
			return
		}
		if eventName == "READY" || eventName == "RESUMED" {
			continue
		}
		if eventName != "RELATIONSHIP_ADD" && eventName != "RELATIONSHIP_UPDATE" && eventName != "PRESENCE_UPDATE" {
			t.Fatalf("expected RELATIONSHIP or PRESENCE_UPDATE event while draining, got %s", eventName)
		}
	}
}
