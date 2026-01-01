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
	"bufio"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
)

type POFile struct {
	HeaderLines []string
	Entries     []POEntry
}

type POEntry struct {
	Comments   []string
	References []string
	MsgID      string
	MsgStr     string
}

func main() {
	localesDir := flag.String("locales-dir", "../../../../src/locales", "Path to the locales directory")
	singleLocale := flag.String("locale", "", "Reset only this locale (empty = all)")
	dryRun := flag.Bool("dry-run", false, "Show what would be reset without making changes")
	flag.Parse()

	absLocalesDir, err := absPath(*localesDir)
	if err != nil {
		fmt.Printf("Failed to resolve locales directory: %v\n", err)
		os.Exit(1)
	}

	locales, err := discoverLocales(absLocalesDir)
	if err != nil {
		fmt.Printf("Failed to discover locales: %v\n", err)
		os.Exit(1)
	}

	var targetLocales []string
	for _, locale := range locales {
		if locale == "en-US" {
			continue
		}
		if *singleLocale != "" && locale != *singleLocale {
			continue
		}
		targetLocales = append(targetLocales, locale)
	}

	if len(targetLocales) == 0 {
		fmt.Println("No target locales found")
		os.Exit(1)
	}

	fmt.Printf("Resetting translations for %d locales...\n", len(targetLocales))
	if *dryRun {
		fmt.Println("(DRY RUN - no changes will be made)")
	}
	fmt.Println()

	totalReset := 0
	for _, locale := range targetLocales {
		poPath := filepath.Join(absLocalesDir, locale, "messages.po")
		poFile, err := parsePOFile(poPath)
		if err != nil {
			fmt.Printf("  ✗ %s: failed to parse: %v\n", locale, err)
			continue
		}

		resetCount := 0
		for i := range poFile.Entries {
			if poFile.Entries[i].MsgStr != "" {
				resetCount++
				poFile.Entries[i].MsgStr = ""
			}
		}

		if resetCount == 0 {
			fmt.Printf("  - %s: already empty (0 strings)\n", locale)
			continue
		}

		if !*dryRun {
			if err := writePOFile(poPath, poFile); err != nil {
				fmt.Printf("  ✗ %s: failed to write: %v\n", locale, err)
				continue
			}
		}

		fmt.Printf("  ✓ %s: reset %d strings\n", locale, resetCount)
		totalReset += resetCount
	}

	fmt.Printf("\nTotal: reset %d translations across %d locales\n", totalReset, len(targetLocales))
	if *dryRun {
		fmt.Println("(DRY RUN - run without --dry-run to apply changes)")
	}
}

func absPath(rel string) (string, error) {
	if filepath.IsAbs(rel) {
		return rel, nil
	}
	wd, err := os.Getwd()
	if err != nil {
		return "", err
	}
	return filepath.Join(wd, rel), nil
}

func discoverLocales(localesDir string) ([]string, error) {
	entries, err := os.ReadDir(localesDir)
	if err != nil {
		return nil, err
	}
	var locales []string
	for _, entry := range entries {
		if entry.IsDir() {
			locales = append(locales, entry.Name())
		}
	}
	sort.Strings(locales)
	return locales, nil
}

func parsePOFile(path string) (POFile, error) {
	file, err := os.Open(path)
	if err != nil {
		return POFile{}, err
	}
	defer file.Close()

	var (
		current   []string
		scanner   = bufio.NewScanner(file)
		trimmed   string
		headerSet bool
		result    POFile
	)

	for scanner.Scan() {
		line := scanner.Text()
		trimmed = strings.TrimSpace(line)
		if trimmed == "" {
			if len(current) > 0 {
				entry := parseBlock(current)
				if !headerSet && entry.MsgID == "" {
					result.HeaderLines = append([]string{}, current...)
					headerSet = true
				} else {
					result.Entries = append(result.Entries, entry)
				}
				current = nil
			}
			continue
		}
		current = append(current, line)
	}
	if len(current) > 0 {
		entry := parseBlock(current)
		if !headerSet && entry.MsgID == "" {
			result.HeaderLines = append([]string{}, current...)
		} else {
			result.Entries = append(result.Entries, entry)
		}
	}
	if err := scanner.Err(); err != nil {
		return POFile{}, err
	}

	return result, nil
}

func parseBlock(lines []string) POEntry {
	entry := POEntry{}
	var (
		inMsgID  bool
		inMsgStr bool
	)

	for _, raw := range lines {
		line := strings.TrimSpace(raw)
		switch {
		case strings.HasPrefix(line, "#."):
			entry.Comments = append(entry.Comments, strings.TrimSpace(line[2:]))
		case strings.HasPrefix(line, "#:"):
			entry.References = append(entry.References, strings.TrimSpace(line[2:]))
		case strings.HasPrefix(line, "msgid"):
			entry.MsgID = parseQuoted(strings.TrimSpace(line[len("msgid"):]))
			inMsgID = true
			inMsgStr = false
		case strings.HasPrefix(line, "msgstr"):
			entry.MsgStr = parseQuoted(strings.TrimSpace(line[len("msgstr"):]))
			inMsgStr = true
			inMsgID = false
		case strings.HasPrefix(line, "\""):
			if inMsgID {
				entry.MsgID += parseQuoted(line)
			} else if inMsgStr {
				entry.MsgStr += parseQuoted(line)
			}
		}
	}
	return entry
}

func parseQuoted(value string) string {
	value = strings.TrimSpace(value)
	if value == "" {
		return ""
	}
	if unquoted, err := strconv.Unquote(value); err == nil {
		return unquoted
	}
	if strings.HasPrefix(value, "\"") && strings.HasSuffix(value, "\"") {
		return value[1 : len(value)-1]
	}
	return value
}

func writePOFile(path string, po POFile) error {
	var lines []string
	if len(po.HeaderLines) > 0 {
		lines = append(lines, po.HeaderLines...)
		lines = append(lines, "")
	}

	for idx, entry := range po.Entries {
		lines = append(lines, renderEntry(entry))
		if idx < len(po.Entries)-1 {
			lines = append(lines, "")
		}
	}
	lines = append(lines, "")
	return os.WriteFile(path, []byte(strings.Join(lines, "\n")), 0o644)
}

func renderEntry(entry POEntry) string {
	var sb strings.Builder
	for _, comment := range entry.Comments {
		sb.WriteString("#. ")
		sb.WriteString(comment)
		sb.WriteString("\n")
	}
	for _, ref := range entry.References {
		sb.WriteString("#: ")
		sb.WriteString(ref)
		sb.WriteString("\n")
	}
	sb.WriteString("msgid ")
	sb.WriteString(strconv.Quote(entry.MsgID))
	sb.WriteString("\nmsgstr ")
	sb.WriteString(strconv.Quote(entry.MsgStr))
	return sb.String()
}
