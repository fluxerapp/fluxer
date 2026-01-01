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
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/schollz/progressbar/v3"
)

type Locale struct {
	Code       string
	Name       string
	NativeName string
}

type OpenRouterRequest struct {
	Model    string    `json:"model"`
	Messages []Message `json:"messages"`
}

type Message struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

type OpenRouterResponse struct {
	Choices []Choice `json:"choices"`
}

type Choice struct {
	Message Message `json:"message"`
}

type TranslatableString struct {
	MsgID       string
	MsgIDPlural string
	Context     string
	Comments    []string
	Files       []FileLocation
	IsPlural    bool
	HasContext  bool
}

type FileLocation struct {
	FilePath string
	Line     int
}

type POEntry struct {
	Context     string
	Comments    []string
	Files       []string
	MsgID       string
	MsgIDPlural string
	MsgStr      []string
	IsPlural    bool
	HasContext  bool
}

type TranslationJob struct {
	Locale     string
	FilePath   string
	Entries    []POEntry
	Language   string
	TotalCount int
}

type TranslationResult struct {
	Locale     string
	FilePath   string
	Success    bool
	Translated int
	Error      error
}

type DocTranslationResult struct {
	Locale  string
	Folder  string
	Success bool
	Error   error
}

type FileMetadata struct {
	Hash         string    `json:"hash"`
	LastModified time.Time `json:"last_modified"`
}

type MetadataFile struct {
	Files map[string]FileMetadata `json:"files"`
}

type DocTranslationJob struct {
	Locale     string
	Folder     string
	SourcePath string
	TargetPath string
	Content    string
	Language   string
	NativeName string
	DocType    string
}

const (
	apiURL     = "https://openrouter.ai/api/v1/chat/completions"
	batchSize  = 10
	maxWorkers = 3
)

var (
	gFuncRegex  = regexp.MustCompile(`(?s)g_\s*\(\s*.*?,\s*"([^"]+)"`)
	nFuncRegex  = regexp.MustCompile(`(?s)n_\s*\(\s*"([^"]+)"\s*,\s*"([^"]+)"\s*,\s*"([^"]+)"`)
	pFuncRegex  = regexp.MustCompile(`(?s)p_\s*\(\s*"([^"]+)"\s*,\s*"([^"]+)"\s*,\s*"([^"]+)"`)
	npFuncRegex = regexp.MustCompile(`(?s)np_\s*\(\s*"([^"]+)"\s*,\s*"([^"]+)"\s*,\s*"([^"]+)"\s*,\s*"([^"]+)"`)

	msgIDRegex       = regexp.MustCompile(`^msgid\s+"(.*)"\s*$`)
	msgIDPluralRegex = regexp.MustCompile(`^msgid_plural\s+"(.*)"\s*$`)
	msgStrRegex      = regexp.MustCompile(`^msgstr(?:\[(\d+)\])?\s+"(.*)"\s*$`)
	msgCtxtRegex     = regexp.MustCompile(`^msgctxt\s+"(.*)"\s*$`)
	commentRegex     = regexp.MustCompile(`^#\.\s+(.*)$`)
	fileRefRegex     = regexp.MustCompile(`^#:\s+(.*)$`)
	continueRegex    = regexp.MustCompile(`^"(.*)"\s*$`)

	supportedLocales = []Locale{
		{Code: "ar", Name: "Arabic", NativeName: "العربية"},
		{Code: "bg", Name: "Bulgarian", NativeName: "Български"},
		{Code: "cs", Name: "Czech", NativeName: "Čeština"},
		{Code: "da", Name: "Danish", NativeName: "Dansk"},
		{Code: "de", Name: "German", NativeName: "Deutsch"},
		{Code: "el", Name: "Greek", NativeName: "Ελληνικά"},
		{Code: "en-GB", Name: "English", NativeName: "English"},
		{Code: "es-ES", Name: "Spanish (Spain)", NativeName: "Español (España)"},
		{Code: "es-419", Name: "Spanish (Latin America)", NativeName: "Español (Latinoamérica)"},
		{Code: "fi", Name: "Finnish", NativeName: "Suomi"},
		{Code: "fr", Name: "French", NativeName: "Français"},
		{Code: "he", Name: "Hebrew", NativeName: "עברית"},
		{Code: "hi", Name: "Hindi", NativeName: "हिन्दी"},
		{Code: "hr", Name: "Croatian", NativeName: "Hrvatski"},
		{Code: "hu", Name: "Hungarian", NativeName: "Magyar"},
		{Code: "id", Name: "Indonesian", NativeName: "Bahasa Indonesia"},
		{Code: "it", Name: "Italian", NativeName: "Italiano"},
		{Code: "ja", Name: "Japanese", NativeName: "日本語"},
		{Code: "ko", Name: "Korean", NativeName: "한국어"},
		{Code: "lt", Name: "Lithuanian", NativeName: "Lietuvių"},
		{Code: "nl", Name: "Dutch", NativeName: "Nederlands"},
		{Code: "no", Name: "Norwegian", NativeName: "Norsk"},
		{Code: "pl", Name: "Polish", NativeName: "Polski"},
		{Code: "pt-BR", Name: "Portuguese (Brazil)", NativeName: "Português (Brasil)"},
		{Code: "ro", Name: "Romanian", NativeName: "Română"},
		{Code: "ru", Name: "Russian", NativeName: "Русский"},
		{Code: "sv-SE", Name: "Swedish", NativeName: "Svenska"},
		{Code: "th", Name: "Thai", NativeName: "ไทย"},
		{Code: "tr", Name: "Turkish", NativeName: "Türkçe"},
		{Code: "uk", Name: "Ukrainian", NativeName: "Українська"},
		{Code: "vi", Name: "Vietnamese", NativeName: "Tiếng Việt"},
		{Code: "zh-CN", Name: "Chinese (Simplified)", NativeName: "中文 (简体)"},
		{Code: "zh-TW", Name: "Chinese (Traditional)", NativeName: "中文 (繁體)"},
	}

	languageMap = make(map[string]string)
)

func init() {
	for _, locale := range supportedLocales {
		languageMap[locale.Code] = locale.NativeName
	}
}

func main() {
	fmt.Printf("Fluxer Localization Tool\n")
	fmt.Printf("========================\n\n")

	apiKey := os.Getenv("OPENROUTER_API_KEY")
	if apiKey == "" {
		fmt.Printf("Error: OPENROUTER_API_KEY environment variable not set\n")
		os.Exit(1)
	}

	startTime := time.Now()

	processCodeLocalization(apiKey)
	fmt.Printf("%s", "\n"+strings.Repeat("=", 50)+"\n\n")

	duration := time.Since(startTime)
	fmt.Printf("\n========================\n")
	fmt.Printf("Localization complete in %v\n", duration.Round(time.Second))
}

func processCodeLocalization(apiKey string) {
	fmt.Printf("Processing code localization...\n\n")

	fmt.Printf("Step 1: Extracting translatable strings from Gleam files...\n")
	strings := extractStringsFromGleam("src")
	if len(strings) == 0 {
		fmt.Printf("No translatable strings found\n")
		return
	}
	fmt.Printf("Found %d translatable strings\n\n", len(strings))

	fmt.Printf("Step 2: Creating POT template file...\n")
	potFile := "locales/messages.pot"
	if err := createPOTFile(potFile, strings); err != nil {
		fmt.Printf("Failed to create POT file: %v\n", err)
		return
	}
	fmt.Printf("Created %s\n\n", potFile)

	fmt.Printf("Step 3: Processing translations for all locales...\n")
	processAllTranslations(strings, apiKey)
	fmt.Printf("\n")

	fmt.Printf("Step 4: Compiling PO files to MO files...\n")
	if err := compileMOFiles(); err != nil {
		fmt.Printf("Warning: Failed to compile MO files: %v\n", err)
		fmt.Printf("Make sure msgfmt is installed (part of gettext package)\n")
	} else {
		fmt.Printf("Successfully compiled all MO files\n")
	}
}

func extractStringsFromGleam(srcDir string) []TranslatableString {
	tempPOT := "temp_extracted.pot"
	defer os.Remove(tempPOT)

	var gleamFiles []string
	err := filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if strings.HasSuffix(path, ".gleam") {
			gleamFiles = append(gleamFiles, path)
		}
		return nil
	})

	if err != nil {
		fmt.Printf("Error walking directory: %v\n", err)
		return nil
	}

	if len(gleamFiles) == 0 {
		return nil
	}

	args := []string{
		"--language=C",
		"--keyword=g_:2",
		"--keyword=n_:2,3,4",
		"--keyword=p_:1c,3",
		"--keyword=np_:1c,3,4",
		"--from-code=UTF-8",
		"--output=" + tempPOT,
		"--no-wrap",
	}
	args = append(args, gleamFiles...)

	cmd := exec.Command("xgettext", args...)
	if output, err := cmd.CombinedOutput(); err != nil {
		fmt.Printf("xgettext failed: %v\nOutput: %s\n", err, output)
		return extractStringsFromGleamRegex(srcDir)
	}

	entries, err := parsePOFile(tempPOT)
	if err != nil {
		fmt.Printf("Failed to parse generated POT file: %v\n", err)
		return extractStringsFromGleamRegex(srcDir)
	}

	var result []TranslatableString
	for _, entry := range entries {
		if entry.MsgID == "" {
			continue
		}

		var files []FileLocation
		for _, file := range entry.Files {
			parts := strings.Split(file, ":")
			if len(parts) >= 2 {
				line := 0
				if _, err := fmt.Sscanf(parts[1], "%d", &line); err != nil {
					fmt.Printf("Warning: Failed to parse line number: %v\n", err)
				}
				files = append(files, FileLocation{FilePath: parts[0], Line: line})
			}
		}

		result = append(result, TranslatableString{
			MsgID:       entry.MsgID,
			MsgIDPlural: entry.MsgIDPlural,
			Context:     entry.Context,
			Comments:    entry.Comments,
			Files:       files,
			IsPlural:    entry.IsPlural,
			HasContext:  entry.HasContext,
		})
	}

	sort.Slice(result, func(i, j int) bool {
		if result[i].MsgID != result[j].MsgID {
			return result[i].MsgID < result[j].MsgID
		}
		return result[i].Context < result[j].Context
	})

	return result
}

func extractStringsFromGleamRegex(srcDir string) []TranslatableString {
	stringsMap := make(map[string]*TranslatableString)

	err := filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !strings.HasSuffix(path, ".gleam") {
			return nil
		}

		content, err := os.ReadFile(path)
		if err != nil {
			fmt.Printf("Error reading %s: %v\n", path, err)
			return nil
		}

		extractFromFile(string(content), path, stringsMap)
		return nil
	})

	if err != nil {
		fmt.Printf("Error walking directory: %v\n", err)
	}

	var result []TranslatableString
	for _, str := range stringsMap {
		result = append(result, *str)
	}

	sort.Slice(result, func(i, j int) bool {
		if result[i].MsgID != result[j].MsgID {
			return result[i].MsgID < result[j].MsgID
		}
		return result[i].Context < result[j].Context
	})

	return result
}

func extractFromFile(content, filePath string, stringsMap map[string]*TranslatableString) {
	lines := strings.Split(content, "\n")

	for lineNum, line := range lines {
		if matches := gFuncRegex.FindAllStringSubmatch(line, -1); matches != nil {
			for _, match := range matches {
				key := match[1]
				addString(stringsMap, key, "", "", false, false, filePath, lineNum+1)
			}
		}

		if matches := nFuncRegex.FindAllStringSubmatch(line, -1); matches != nil {
			for _, match := range matches {
				singular := match[2]
				plural := match[3]
				addString(stringsMap, singular, plural, "", true, false, filePath, lineNum+1)
			}
		}

		if matches := pFuncRegex.FindAllStringSubmatch(line, -1); matches != nil {
			for _, match := range matches {
				context := match[1]
				msgid := match[3]
				addString(stringsMap, msgid, "", context, false, true, filePath, lineNum+1)
			}
		}

		if matches := npFuncRegex.FindAllStringSubmatch(line, -1); matches != nil {
			for _, match := range matches {
				context := match[1]
				singular := match[3]
				plural := match[4]
				addString(stringsMap, singular, plural, context, true, true, filePath, lineNum+1)
			}
		}
	}
}

func addString(stringsMap map[string]*TranslatableString, msgID, msgIDPlural, context string, isPlural, hasContext bool, filePath string, line int) {
	key := msgID
	if hasContext {
		key = context + "|" + msgID
	}

	if existing, ok := stringsMap[key]; ok {
		existing.Files = append(existing.Files, FileLocation{FilePath: filePath, Line: line})
	} else {
		stringsMap[key] = &TranslatableString{
			MsgID:       msgID,
			MsgIDPlural: msgIDPlural,
			Context:     context,
			IsPlural:    isPlural,
			HasContext:  hasContext,
			Files:       []FileLocation{{FilePath: filePath, Line: line}},
		}
	}
}

func createPOTFile(filename string, translatableStrings []TranslatableString) error {
	if err := os.MkdirAll(filepath.Dir(filename), 0755); err != nil {
		return err
	}

	existingTimestamp := time.Now().Format("2006-01-02 15:04-0700")
	if existingContent, err := os.ReadFile(filename); err == nil {
		lines := strings.Split(string(existingContent), "\n")
		for _, line := range lines {
			if strings.Contains(line, "POT-Creation-Date:") {
				if matches := regexp.MustCompile(`POT-Creation-Date: ([^\\]+)\\n`).FindStringSubmatch(line); len(matches) > 1 {
					existingTimestamp = matches[1]
					break
				}
			}
		}
	}

	var content strings.Builder

	content.WriteString(`# SOME DESCRIPTIVE TITLE.
# Copyright (C) 2026 Fluxer Contributors
# This file is distributed under the same license as the Fluxer Marketing package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
msgid ""
msgstr ""
"Project-Id-Version: Fluxer Marketing 1.0.0\n"
"Report-Msgid-Bugs-To: support@fluxer.app\n"
"POT-Creation-Date: ` + existingTimestamp + `\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\n"

`)

	for _, str := range translatableStrings {
		for _, file := range str.Files {
			content.WriteString(fmt.Sprintf("#: %s:%d\n", file.FilePath, file.Line))
		}

		if str.HasContext {
			content.WriteString(fmt.Sprintf("msgctxt \"%s\"\n", escapeString(str.Context)))
		}

		content.WriteString(fmt.Sprintf("msgid \"%s\"\n", escapeString(str.MsgID)))

		if str.IsPlural {
			content.WriteString(fmt.Sprintf("msgid_plural \"%s\"\n", escapeString(str.MsgIDPlural)))
			content.WriteString("msgstr[0] \"\"\n")
			content.WriteString("msgstr[1] \"\"\n")
		} else {
			content.WriteString("msgstr \"\"\n")
		}

		content.WriteString("\n")
	}

	newContent := content.String()

	if existingContent, err := os.ReadFile(filename); err == nil {
		existingNormalized := normalizeContentForComparison(string(existingContent))
		newNormalized := normalizeContentForComparison(newContent)

		if existingNormalized == newNormalized {
			return nil
		}
	}

	return os.WriteFile(filename, []byte(newContent), 0644)
}

func normalizeContentForComparison(content string) string {
	lines := strings.Split(content, "\n")
	var normalized []string

	for _, line := range lines {
		if strings.Contains(line, "POT-Creation-Date:") {
			continue
		}
		normalized = append(normalized, line)
	}

	return strings.Join(normalized, "\n")
}

func processAllTranslations(referenceStrings []TranslatableString, apiKey string) {
	referenceEntries := convertToPoEntries(referenceStrings)
	jobs, totalMissing := scanForMissingTranslations("locales", referenceEntries)

	if totalMissing == 0 {
		fmt.Printf("All translations are complete! No work needed.\n")
		return
	}

	fmt.Printf("Found %d missing translations across %d locales\n", totalMissing, len(jobs))
	fmt.Printf("Processing with %d workers, batch size %d\n", maxWorkers, batchSize)
	fmt.Printf("===================================\n\n")

	results := processTranslationJobs(jobs, totalMissing, apiKey)
	printSummary(results)
}

func convertToPoEntries(strings []TranslatableString) []POEntry {
	var entries []POEntry
	for _, str := range strings {
		var files []string
		for _, file := range str.Files {
			files = append(files, fmt.Sprintf("%s:%d", file.FilePath, file.Line))
		}

		entry := POEntry{
			Context:     str.Context,
			Comments:    str.Comments,
			Files:       files,
			MsgID:       str.MsgID,
			MsgIDPlural: str.MsgIDPlural,
			IsPlural:    str.IsPlural,
			HasContext:  str.HasContext,
		}

		if str.IsPlural {
			entry.MsgStr = []string{"", ""}
		} else {
			entry.MsgStr = []string{""}
		}

		entries = append(entries, entry)
	}
	return entries
}

func scanForMissingTranslations(localesDir string, referenceEntries []POEntry) ([]TranslationJob, int) {
	var jobs []TranslationJob
	totalMissing := 0

	for locale, language := range languageMap {
		localeDir := filepath.Join(localesDir, locale)
		if err := os.MkdirAll(localeDir, 0755); err != nil {
			fmt.Printf("Failed to create directory %s: %v\n", localeDir, err)
			continue
		}

		poFile := filepath.Join(localeDir, "messages.po")
		needsUpdate := false

		if _, err := os.Stat(poFile); os.IsNotExist(err) {
			if err := createLocaleFileFromReference(poFile, referenceEntries, language, locale); err != nil {
				fmt.Printf("Failed to create %s: %v\n", poFile, err)
				continue
			}
		} else {
			needsUpdate = syncWithReference(poFile, referenceEntries)
		}

		fmt.Printf("Scanning %s (%s)...", locale, language)
		currentEntries, err := parsePOFile(poFile)
		if err != nil {
			fmt.Printf(" Error: %v\n", err)
			continue
		}

		missingEntries := findMissingTranslations(currentEntries, referenceEntries)
		if len(missingEntries) == 0 && !needsUpdate {
			fmt.Printf(" Complete!\n")
			continue
		}

		if needsUpdate {
			fmt.Printf(" %d missing (synced with source)", len(missingEntries))
		} else {
			fmt.Printf(" %d missing", len(missingEntries))
		}
		if len(missingEntries) == 1 {
			fmt.Printf(" (msgid: %q)", missingEntries[0].MsgID)
		}
		fmt.Printf("\n")
		jobs = append(jobs, TranslationJob{
			Locale:     locale,
			FilePath:   poFile,
			Entries:    missingEntries,
			Language:   language,
			TotalCount: len(missingEntries),
		})
		totalMissing += len(missingEntries)
	}

	sort.Slice(jobs, func(i, j int) bool {
		return jobs[i].TotalCount > jobs[j].TotalCount
	})

	return jobs, totalMissing
}

func syncWithReference(filename string, referenceEntries []POEntry) bool {
	currentEntries, err := parsePOFile(filename)
	if err != nil {
		return false
	}

	currentMap := make(map[string]*POEntry)
	obsoleteMap := make(map[string]*POEntry)

	for i := range currentEntries {
		key := currentEntries[i].MsgID
		if currentEntries[i].HasContext {
			key = currentEntries[i].Context + "|" + currentEntries[i].MsgID
		}
		currentMap[key] = &currentEntries[i]
	}

	refMap := make(map[string]*POEntry)
	for i := range referenceEntries {
		key := referenceEntries[i].MsgID
		if referenceEntries[i].HasContext {
			key = referenceEntries[i].Context + "|" + referenceEntries[i].MsgID
		}
		refMap[key] = &referenceEntries[i]
	}

	var updatedEntries []POEntry
	needsUpdate := false

	for _, entry := range currentEntries {
		if entry.MsgID == "" {
			updatedEntries = append(updatedEntries, entry)
			break
		}
	}

	for _, refEntry := range referenceEntries {
		if refEntry.MsgID == "" {
			continue
		}

		refKey := refEntry.MsgID
		if refEntry.HasContext {
			refKey = refEntry.Context + "|" + refEntry.MsgID
		}

		if _, exists := currentMap[refKey]; !exists {
			for currKey, currEntry := range currentMap {
				if currEntry.MsgID == "" {
					continue
				}

				if !allEmpty(currEntry.MsgStr) {
					lenDiff := len(refEntry.MsgID) - len(currEntry.MsgID)
					if lenDiff >= -20 && lenDiff <= 20 {
						obsoleteMap[currKey] = currEntry
					}
				}
			}
		}
	}

	for _, refEntry := range referenceEntries {
		if refEntry.MsgID == "" {
			continue
		}

		key := refEntry.MsgID
		if refEntry.HasContext {
			key = refEntry.Context + "|" + refEntry.MsgID
		}

		if currentEntry, exists := currentMap[key]; exists {
			newEntry := refEntry
			newEntry.MsgStr = currentEntry.MsgStr
			updatedEntries = append(updatedEntries, newEntry)
		} else {
			newEntry := refEntry
			if refEntry.IsPlural {
				newEntry.MsgStr = []string{"", ""}
			} else {
				newEntry.MsgStr = []string{""}
			}
			updatedEntries = append(updatedEntries, newEntry)
			needsUpdate = true
		}
	}

	removedCount := 0
	for key, entry := range currentMap {
		if _, exists := refMap[key]; !exists && key != "" && entry.MsgID != "" {
			removedCount++
			needsUpdate = true

			if !allEmpty(entry.MsgStr) {
				if _, isObsolete := obsoleteMap[key]; isObsolete {
					fmt.Printf(" [obsolete: %q]", entry.MsgID)
				}
			}
		}
	}

	if needsUpdate {
		if err := writePOFile(filename, updatedEntries); err != nil {
			fmt.Printf("Warning: Failed to write PO file: %v\n", err)
		}
	}

	return needsUpdate
}

func createLocaleFileFromReference(filename string, referenceEntries []POEntry, language, locale string) error {
	var content strings.Builder

	pluralForm := getPluralForm(locale)

	content.WriteString(fmt.Sprintf(`# %s translations for Fluxer Marketing
# Copyright (C) 2026 Fluxer Contributors
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"
"Language: %s\n"
"Plural-Forms: %s\n"

`, language, locale, pluralForm))

	for _, entry := range referenceEntries {
		if entry.MsgID == "" {
			continue
		}

		for _, file := range entry.Files {
			content.WriteString(fmt.Sprintf("#: %s\n", file))
		}

		for _, comment := range entry.Comments {
			content.WriteString(fmt.Sprintf("#. %s\n", comment))
		}

		if entry.HasContext {
			content.WriteString(fmt.Sprintf("msgctxt \"%s\"\n", escapeString(entry.Context)))
		}

		content.WriteString(fmt.Sprintf("msgid \"%s\"\n", escapeString(entry.MsgID)))

		if entry.IsPlural {
			content.WriteString(fmt.Sprintf("msgid_plural \"%s\"\n", escapeString(entry.MsgIDPlural)))
			content.WriteString("msgstr[0] \"\"\n")
			content.WriteString("msgstr[1] \"\"\n")
		} else {
			content.WriteString("msgstr \"\"\n")
		}

		content.WriteString("\n")
	}

	return os.WriteFile(filename, []byte(content.String()), 0644)
}

func getPluralForm(locale string) string {
	pluralForms := map[string]string{
		"ar":    "nplurals=6; plural=(n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 && n%100<=99 ? 4 : 5);",
		"cs":    "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;",
		"pl":    "nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);",
		"ru":    "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);",
		"uk":    "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);",
		"hr":    "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);",
		"lt":    "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2);",
		"ro":    "nplurals=3; plural=(n==1?0:(((n%100>19)||((n%100==0)&&(n!=0)))?2:1));",
		"ja":    "nplurals=1; plural=0;",
		"ko":    "nplurals=1; plural=0;",
		"zh-CN": "nplurals=1; plural=0;",
		"zh-TW": "nplurals=1; plural=0;",
		"vi":    "nplurals=1; plural=0;",
		"th":    "nplurals=1; plural=0;",
		"id":    "nplurals=1; plural=0;",
		"tr":    "nplurals=1; plural=0;",
		"hu":    "nplurals=2; plural=(n != 1);",
	}

	if form, ok := pluralForms[locale]; ok {
		return form
	}

	return "nplurals=2; plural=(n != 1);"
}

func parsePOFile(filename string) ([]POEntry, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var entries []POEntry
	var current POEntry
	var inMsgID, inMsgIDPlural, inMsgStr bool
	var currentMsgStrIndex int

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		trimmed := strings.TrimSpace(line)

		if trimmed == "" {
			if current.MsgID != "" {
				entries = append(entries, current)
			}
			current = POEntry{MsgStr: []string{}}
			inMsgID = false
			inMsgIDPlural = false
			inMsgStr = false
			currentMsgStrIndex = 0
			continue
		}

		if strings.HasPrefix(trimmed, "#.") {
			if match := commentRegex.FindStringSubmatch(trimmed); match != nil {
				current.Comments = append(current.Comments, match[1])
			}
		} else if strings.HasPrefix(trimmed, "#:") {
			if match := fileRefRegex.FindStringSubmatch(trimmed); match != nil {
				current.Files = append(current.Files, match[1])
			}
		} else if strings.HasPrefix(trimmed, "msgctxt") {
			if match := msgCtxtRegex.FindStringSubmatch(trimmed); match != nil {
				current.Context = unescapeString(match[1])
				current.HasContext = true
			}
		} else if strings.HasPrefix(trimmed, "msgid_plural") {
			if match := msgIDPluralRegex.FindStringSubmatch(trimmed); match != nil {
				current.MsgIDPlural = unescapeString(match[1])
				current.IsPlural = true
				inMsgIDPlural = true
				inMsgID = false
				inMsgStr = false
			}
		} else if strings.HasPrefix(trimmed, "msgid") {
			if match := msgIDRegex.FindStringSubmatch(trimmed); match != nil {
				current.MsgID = unescapeString(match[1])
				inMsgID = true
				inMsgIDPlural = false
				inMsgStr = false
			}
		} else if strings.HasPrefix(trimmed, "msgstr") {
			if match := msgStrRegex.FindStringSubmatch(trimmed); match != nil {
				if match[1] != "" {
					index := 0
					if _, err := fmt.Sscanf(match[1], "%d", &index); err != nil {
						fmt.Printf("Warning: Failed to parse msgstr index: %v\n", err)
					}
					currentMsgStrIndex = index
					for len(current.MsgStr) <= index {
						current.MsgStr = append(current.MsgStr, "")
					}
					current.MsgStr[index] = unescapeString(match[2])
				} else {
					currentMsgStrIndex = 0
					if len(current.MsgStr) == 0 {
						current.MsgStr = []string{unescapeString(match[2])}
					} else {
						current.MsgStr[0] = unescapeString(match[2])
					}
				}
				inMsgID = false
				inMsgIDPlural = false
				inMsgStr = true
			}
		} else if strings.HasPrefix(trimmed, "\"") {
			if match := continueRegex.FindStringSubmatch(trimmed); match != nil {
				if inMsgID {
					current.MsgID += unescapeString(match[1])
				} else if inMsgIDPlural {
					current.MsgIDPlural += unescapeString(match[1])
				} else if inMsgStr && currentMsgStrIndex < len(current.MsgStr) {
					current.MsgStr[currentMsgStrIndex] += unescapeString(match[1])
				}
			}
		}
	}

	if current.MsgID != "" {
		entries = append(entries, current)
	}

	return entries, scanner.Err()
}

func findMissingTranslations(current, reference []POEntry) []POEntry {
	currentMap := make(map[string]POEntry)
	for _, entry := range current {
		key := entry.MsgID
		if entry.HasContext {
			key = entry.Context + "|" + entry.MsgID
		}
		currentMap[key] = entry
	}

	var missing []POEntry
	for _, refEntry := range reference {
		if refEntry.MsgID == "" {
			continue
		}

		key := refEntry.MsgID
		if refEntry.HasContext {
			key = refEntry.Context + "|" + refEntry.MsgID
		}

		if currentEntry, exists := currentMap[key]; !exists || allEmpty(currentEntry.MsgStr) {
			missing = append(missing, refEntry)
		}
	}

	return missing
}

func allEmpty(strs []string) bool {
	for _, s := range strs {
		if s != "" {
			return false
		}
	}
	return true
}

func processTranslationJobs(jobs []TranslationJob, totalMissing int, apiKey string) []TranslationResult {
	ctx := context.Background()

	overallBar := progressbar.NewOptions(totalMissing,
		progressbar.OptionSetDescription("Overall Progress"),
		progressbar.OptionSetWidth(50),
		progressbar.OptionShowCount(),
		progressbar.OptionShowIts(),
		progressbar.OptionSetItsString("translations"),
		progressbar.OptionThrottle(100*time.Millisecond),
		progressbar.OptionShowElapsedTimeOnFinish(),
	)

	jobChan := make(chan TranslationJob, len(jobs))
	resultChan := make(chan TranslationResult, len(jobs))
	var wg sync.WaitGroup

	for i := 0; i < maxWorkers; i++ {
		wg.Add(1)
		go func(workerID int) {
			defer wg.Done()
			for job := range jobChan {
				result := processLocale(ctx, job, workerID, overallBar, apiKey)
				resultChan <- result
			}
		}(i)
	}

	for _, job := range jobs {
		jobChan <- job
	}
	close(jobChan)

	go func() {
		wg.Wait()
		close(resultChan)
	}()

	var results []TranslationResult
	for result := range resultChan {
		results = append(results, result)
	}

	_ = overallBar.Finish()
	fmt.Println()

	return results
}

func processLocale(ctx context.Context, job TranslationJob, workerID int, overallBar *progressbar.ProgressBar, apiKey string) TranslationResult {
	fmt.Printf("Worker %d: Starting %s (%s) - %d translations\n", workerID+1, job.Locale, job.Language, job.TotalCount)

	translations := make(map[string][]string)
	translatedCount := 0

	for i := 0; i < len(job.Entries); i += batchSize {
		end := min(i+batchSize, len(job.Entries))
		batch := job.Entries[i:end]

		batchTranslations, err := translateBatch(ctx, batch, job.Language, apiKey)
		if err != nil {
			fmt.Printf("Worker %d: Batch translation failed for %s: %v\n", workerID+1, job.Locale, err)
			continue
		}

		for key, translation := range batchTranslations {
			translations[key] = translation
			translatedCount++
			_ = overallBar.Add(1)
		}

		time.Sleep(500 * time.Millisecond)
	}

	if translatedCount == 0 {
		return TranslationResult{
			Locale:     job.Locale,
			FilePath:   job.FilePath,
			Success:    false,
			Translated: 0,
			Error:      fmt.Errorf("no translations completed"),
		}
	}

	err := updatePOFile(job.FilePath, translations)
	success := err == nil

	if success {
		fmt.Printf("Worker %d: Completed %s (%d/%d translations)\n", workerID+1, job.Locale, translatedCount, job.TotalCount)
	} else {
		fmt.Printf("Worker %d: Failed to update %s: %v\n", workerID+1, job.Locale, err)
	}

	return TranslationResult{
		Locale:     job.Locale,
		FilePath:   job.FilePath,
		Success:    success,
		Translated: translatedCount,
		Error:      err,
	}
}

func translateBatch(ctx context.Context, entries []POEntry, targetLanguage string, apiKey string) (map[string][]string, error) {
	if len(entries) == 0 {
		return make(map[string][]string), nil
	}

	prompt := fmt.Sprintf(`You are a professional translator. Translate the following English text snippets to %s.

CRITICAL LANGUAGE REQUIREMENTS:
- ONLY translate to %s - never use any other language
- If the target language is "English (UK)", translate from American English to British English spelling and terminology
- Double-check that your response is entirely in the target language %s
- Never mix languages in your response

IMPORTANT FORMATTING RULES:
1. Preserve ALL formatting, placeholders, and special syntax exactly as shown
2. Keep {0}, {1}, etc. placeholders unchanged
3. Preserve plural forms syntax like {0, plural, =0 {text} one {text} other {text}}
4. Maintain HTML tags and markdown formatting
5. Keep technical terms and brand names unchanged when appropriate
6. Provide natural, contextually appropriate translations
7. For plural forms, provide appropriate translations for both singular and plural
8. Return ONLY the translation for each text, no explanations
9. PRESERVE ABBREVIATIONS: If the source uses abbreviations like "Docs" instead of "Documentation", maintain the same level of abbreviation in the target language
10. NEVER TRANSLATE OR CONVERT CURRENCIES: ALWAYS preserve currency symbols and amounts exactly as they appear in the source text. For USD ($): always keep the dollar sign before the amount with no space ("$49.99/year"). For EUR (€): follow EU Interinstitutional Style Guide - place euro sign BEFORE the amount in English ("€100 EUR") but AFTER the amount with a space in all other languages ("100 € EUR"). NEVER convert between different currencies or change currency symbols under any circumstances
11. PRESERVE WHITESPACE: Maintain the same spacing, line breaks, and indentation as the source
12. KEEP SPECIAL CHARACTERS: Preserve symbols, emojis, and special punctuation marks
13. MAINTAIN CAPITALIZATION PATTERNS: Follow target language conventions while preserving intentional capitalization (like ALL CAPS for emphasis)
14. CONSISTENT TERMINOLOGY: Use the same translation for recurring terms throughout the text
15. NO MACHINE TRANSLATION ARTIFACTS: Avoid awkward phrasing that sounds like machine translation
16. VERIFY COMPLETENESS: Ensure no part of the source text is omitted in the translation

TARGET LANGUAGE: %s
SOURCE LANGUAGE: English

Translate these texts:

`, targetLanguage, targetLanguage, targetLanguage, targetLanguage)

	for i, entry := range entries {
		contextInfo := ""
		if entry.Context != "" {
			contextInfo = fmt.Sprintf(" (Context: %s)", entry.Context)
		}
		if len(entry.Comments) > 0 {
			contextInfo += fmt.Sprintf(" (Note: %s)", strings.Join(entry.Comments, "; "))
		}

		if entry.IsPlural {
			prompt += fmt.Sprintf("%d. Singular: \"%s\", Plural: \"%s\"%s\n",
				i+1, entry.MsgID, entry.MsgIDPlural, contextInfo)
		} else {
			prompt += fmt.Sprintf("%d. \"%s\"%s\n", i+1, entry.MsgID, contextInfo)
		}
	}

	if entries[0].IsPlural {
		prompt += "\nRespond with translations in the format:\n1. Singular: [translation], Plural: [translation]\n2. Singular: [translation], Plural: [translation]\n..."
	} else {
		prompt += "\nRespond with translations in the format:\n1. [translation]\n2. [translation]\n..."
	}

	reqBody := OpenRouterRequest{
		Model: "gpt-4o-mini",
		Messages: []Message{
			{Role: "user", Content: prompt},
		},
	}

	jsonData, err := json.Marshal(reqBody)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, "POST", apiURL, bytes.NewBuffer(jsonData))
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+apiKey)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("API error %d: %s", resp.StatusCode, string(body))
	}

	var apiResp OpenRouterResponse
	if err := json.NewDecoder(resp.Body).Decode(&apiResp); err != nil {
		return nil, fmt.Errorf("failed to decode response: %w", err)
	}

	if len(apiResp.Choices) == 0 {
		return nil, fmt.Errorf("no response from API")
	}

	return parseTranslationResponse(apiResp.Choices[0].Message.Content, entries)
}

func parseTranslationResponse(response string, entries []POEntry) (map[string][]string, error) {
	lines := strings.Split(strings.TrimSpace(response), "\n")
	translations := make(map[string][]string)

	numberRegex := regexp.MustCompile(`^\d+\.\s*(.*)$`)
	pluralRegex := regexp.MustCompile(`Singular:\s*"?([^",]*)"?,\s*Plural:\s*"?([^"]*)"?`)

	entryIndex := 0

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		var translation string
		if match := numberRegex.FindStringSubmatch(line); match != nil {
			translation = match[1]
		} else {
			continue
		}

		if entryIndex < len(entries) {
			entry := entries[entryIndex]
			key := entry.MsgID
			if entry.HasContext {
				key = entry.Context + "|" + entry.MsgID
			}

			if entry.IsPlural {
				if match := pluralRegex.FindStringSubmatch(translation); match != nil {
					singular := strings.Trim(match[1], `"`)
					plural := strings.Trim(match[2], `"`)
					translations[key] = []string{singular, plural}
				}
			} else {
				cleanTranslation := strings.Trim(translation, `"`)
				translations[key] = []string{cleanTranslation}
			}

			entryIndex++
		}
	}

	return translations, nil
}

func updatePOFile(filename string, translations map[string][]string) error {
	entries, err := parsePOFile(filename)
	if err != nil {
		return err
	}

	for i, entry := range entries {
		key := entry.MsgID
		if entry.HasContext {
			key = entry.Context + "|" + entry.MsgID
		}

		if translation, exists := translations[key]; exists {
			entries[i].MsgStr = translation
		}
	}

	return writePOFile(filename, entries)
}

func writePOFile(filename string, entries []POEntry) error {
	var content strings.Builder

	content.WriteString(`# SOME DESCRIPTIVE TITLE.
# Copyright (C) 2026 Fluxer Contributors
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

`)

	for _, entry := range entries {
		if entry.MsgID == "" {
			continue
		}

		for _, file := range entry.Files {
			content.WriteString(fmt.Sprintf("#: %s\n", file))
		}

		for _, comment := range entry.Comments {
			content.WriteString(fmt.Sprintf("#. %s\n", comment))
		}

		if entry.HasContext {
			content.WriteString(fmt.Sprintf("msgctxt \"%s\"\n", escapeString(entry.Context)))
		}

		content.WriteString(fmt.Sprintf("msgid \"%s\"\n", escapeString(entry.MsgID)))

		if entry.IsPlural {
			content.WriteString(fmt.Sprintf("msgid_plural \"%s\"\n", escapeString(entry.MsgIDPlural)))
			for i, str := range entry.MsgStr {
				content.WriteString(fmt.Sprintf("msgstr[%d] \"%s\"\n", i, escapeString(str)))
			}
		} else {
			if len(entry.MsgStr) > 0 {
				content.WriteString(fmt.Sprintf("msgstr \"%s\"\n", escapeString(entry.MsgStr[0])))
			} else {
				content.WriteString("msgstr \"\"\n")
			}
		}

		content.WriteString("\n")
	}

	return os.WriteFile(filename, []byte(content.String()), 0644)
}

func escapeString(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\r", "\\r")
	s = strings.ReplaceAll(s, "\t", "\\t")
	return s
}

func unescapeString(s string) string {
	s = strings.ReplaceAll(s, "\\\"", "\"")
	s = strings.ReplaceAll(s, "\\n", "\n")
	s = strings.ReplaceAll(s, "\\r", "\r")
	s = strings.ReplaceAll(s, "\\t", "\t")
	s = strings.ReplaceAll(s, "\\\\", "\\")
	return s
}

func compileMOFiles() error {
	localesDir := "locales"
	privDir := "priv/locales"

	entries, err := os.ReadDir(localesDir)
	if err != nil {
		return err
	}

	compiledCount := 0

	for _, entry := range entries {
		if !entry.IsDir() || entry.Name() == "." || entry.Name() == ".." {
			continue
		}

		locale := entry.Name()
		poFile := filepath.Join(localesDir, locale, "messages.po")

		if _, err := os.Stat(poFile); os.IsNotExist(err) {
			continue
		}

		moDir := filepath.Join(privDir, locale, "LC_MESSAGES")
		if err := os.MkdirAll(moDir, 0755); err != nil {
			fmt.Printf("Failed to create directory %s: %v\n", moDir, err)
			continue
		}

		moFile := filepath.Join(moDir, "messages.mo")

		cmd := exec.Command("msgfmt", poFile, "-o", moFile)
		if output, err := cmd.CombinedOutput(); err != nil {
			fmt.Printf("Failed to compile %s: %v\nOutput: %s\n", locale, err, output)
			continue
		}

		fmt.Printf("Compiled %s/messages.po → %s\n", locale, moFile)
		compiledCount++
	}

	if compiledCount > 0 {
		fmt.Printf("Successfully compiled %d locale files\n", compiledCount)
		return nil
	}

	return fmt.Errorf("no files compiled")
}

func printSummary(results []TranslationResult) {
	fmt.Printf("\n===================================\n")
	fmt.Printf("TRANSLATION SUMMARY\n")
	fmt.Printf("===================================\n\n")

	totalProcessed := 0
	totalSuccess := 0
	totalTranslations := 0
	var successful []TranslationResult
	var failed []TranslationResult

	for _, result := range results {
		totalProcessed++
		totalTranslations += result.Translated
		if result.Success {
			totalSuccess++
			successful = append(successful, result)
		} else {
			failed = append(failed, result)
		}
	}

	if len(successful) > 0 {
		fmt.Printf("SUCCESSFUL LOCALIZATIONS:\n")
		for _, result := range successful {
			lang := languageMap[result.Locale]
			fmt.Printf("   %s (%s): %d translations\n", result.Locale, lang, result.Translated)
		}
		fmt.Println()
	}

	if len(failed) > 0 {
		fmt.Printf("FAILED LOCALIZATIONS:\n")
		for _, result := range failed {
			lang := languageMap[result.Locale]
			fmt.Printf("   %s (%s): %v\n", result.Locale, lang, result.Error)
		}
		fmt.Println()
	}

	fmt.Printf("STATISTICS:\n")
	fmt.Printf("   Total Locales Processed: %d\n", totalProcessed)
	fmt.Printf("   Successful: %d\n", totalSuccess)
	fmt.Printf("   Failed: %d\n", len(failed))
	fmt.Printf("   Total Translations: %d\n", totalTranslations)

	if totalSuccess == totalProcessed && totalProcessed > 0 {
		fmt.Printf("\nAll localizations completed successfully!\n")
	} else if totalProcessed > 0 {
		fmt.Printf("\n%d of %d localizations completed successfully.\n", totalSuccess, totalProcessed)
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
