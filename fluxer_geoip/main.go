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

package main

import (
	"log"
	"net/http"
	"net/netip"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	maxminddb "github.com/oschwald/maxminddb-golang/v2"
)

type cfg struct {
	Port   string
	DBPath string
	TTL    time.Duration
	Cap    int
}

type liteRecord struct {
	CountryCode string `maxminddb:"country_code"`
}

type cacheEntry struct {
	val string
	exp time.Time
}
type lru struct {
	mu    sync.Mutex
	data  map[string]cacheEntry
	order []string
	ttl   time.Duration
	cap   int
}

func newLRU(ttl time.Duration, cap int) *lru {
	return &lru{data: make(map[string]cacheEntry, cap), order: make([]string, 0, cap), ttl: ttl, cap: cap}
}
func (c *lru) get(k string) (string, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	ent, ok := c.data[k]
	if !ok || time.Now().After(ent.exp) {
		if ok {
			delete(c.data, k)
		}
		return "", false
	}
	for i, v := range c.order {
		if v == k {
			copy(c.order[i:], c.order[i+1:])
			c.order[len(c.order)-1] = k
			break
		}
	}
	return ent.val, true
}
func (c *lru) put(k, v string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if _, exists := c.data[k]; !exists && len(c.data) >= c.cap {
		if len(c.order) > 0 {
			ev := c.order[0]
			delete(c.data, ev)
			c.order = c.order[1:]
		}
	}
	c.data[k] = cacheEntry{val: v, exp: time.Now().Add(c.ttl)}
	for i, v2 := range c.order {
		if v2 == k {
			copy(c.order[i:], c.order[i+1:])
			c.order = c.order[:len(c.order)-1]
			break
		}
	}
	c.order = append(c.order, k)
}

func getEnv(key, defaultVal string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return defaultVal
}

func getEnvInt(key string, defaultVal int) int {
	if v := os.Getenv(key); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 {
			return n
		}
	}
	return defaultVal
}

func getEnvDuration(key string, defaultVal time.Duration) time.Duration {
	if v := os.Getenv(key); v != "" {
		if d, err := time.ParseDuration(v); err == nil && d > 0 {
			return d
		}
	}
	return defaultVal
}

func main() {
	c := cfg{
		Port:   getEnv("FLUXER_GEOIP_PORT", "8080"),
		DBPath: getEnv("GEOIP_DB_PATH", "/data/ipinfo_lite.mmdb"),
		TTL:    getEnvDuration("GEOIP_CACHE_TTL", 10*time.Minute),
		Cap:    getEnvInt("GEOIP_CACHE_SIZE", 20000),
	}

	db, err := maxminddb.Open(c.DBPath)
	if err != nil {
		log.Fatalf("open mmdb: %v", err)
	}
	defer db.Close()

	cache := newLRU(c.TTL, c.Cap)

	http.HandleFunc("/_health", func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})

	http.HandleFunc("/lookup", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")

		ipStr := r.URL.Query().Get("ip")
		if ipStr == "" {
			http.Error(w, "missing 'ip' query param", http.StatusBadRequest)
			return
		}
		addr, err := netip.ParseAddr(strings.TrimSpace(strings.Trim(ipStr, "[]")))
		if err != nil {
			http.Error(w, "invalid ip", http.StatusBadRequest)
			return
		}

		key := addr.String()
		if v, ok := cache.get(key); ok {
			if v == "" {
				w.WriteHeader(http.StatusNoContent)
				return
			}
			_, _ = w.Write([]byte(v))
			return
		}

		var rec liteRecord
		if err := db.Lookup(addr).Decode(&rec); err != nil {
			http.Error(w, "lookup error", http.StatusInternalServerError)
			return
		}

		cc := strings.TrimSpace(rec.CountryCode)
		cache.put(key, cc)

		if cc == "" {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		_, _ = w.Write([]byte(cc))
	})

	log.Printf("geoip-lite (text+cache) listening on :%s (ttl=%s cap=%d)", c.Port, c.TTL, c.Cap)
	log.Fatal(http.ListenAndServe(":"+c.Port, nil))
}
