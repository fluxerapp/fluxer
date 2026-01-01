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

package integrations

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"fluxer.dev/dev/pkg/utils"
)

const (
	DefaultGeoIPDir  = "dev/geoip"
	DefaultGeoIPFile = "country_asn.mmdb"
)

// DownloadGeoIP downloads the GeoIP database from IPInfo
func DownloadGeoIP(tokenFlag, envPath string) error {
	token := strings.TrimSpace(tokenFlag)
	if token == "" {
		token = strings.TrimSpace(os.Getenv("IPINFO_TOKEN"))
	}

	if token == "" && envPath != "" {
		env, err := utils.ParseEnvFile(envPath)
		if err == nil {
			token = strings.TrimSpace(env["IPINFO_TOKEN"])
		}
	}

	if token == "" {
		return errors.New("IPInfo token required; provide via --token, IPINFO_TOKEN env var, or the config/env")
	}

	if err := os.MkdirAll(DefaultGeoIPDir, 0o755); err != nil {
		return err
	}
	outPath := filepath.Join(DefaultGeoIPDir, DefaultGeoIPFile)
	u := fmt.Sprintf("https://ipinfo.io/data/free/country_asn.mmdb?token=%s", url.QueryEscape(token))

	fmt.Printf("Downloading GeoIP database to %s...\n", outPath)

	resp, err := http.Get(u)
	if err != nil {
		return fmt.Errorf("failed to download GeoIP db: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 512))
		return fmt.Errorf("unexpected response (%d): %s", resp.StatusCode, strings.TrimSpace(string(body)))
	}

	f, err := os.Create(outPath)
	if err != nil {
		return err
	}
	defer f.Close()

	n, err := io.Copy(f, resp.Body)
	if err != nil {
		return err
	}
	if n == 0 {
		return errors.New("downloaded GeoIP file is empty; check your IPInfo token")
	}

	fmt.Printf("✅ GeoIP database downloaded (%d bytes).\n", n)
	fmt.Println()
	fmt.Println("If you're running a GeoIP service container, restart it so it picks up the new database.")
	return nil
}
