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
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

const (
	defaultChunkSize   = 50
	codeBlockStart     = "```json"
	codeBlockEnd       = "```"
	maxPromptLength    = 102372
	defaultAPIEndpoint = "https://openrouter.ai/api/v1/chat/completions"
	defaultAPITimeout  = 35 * time.Minute
	defaultQCPasses    = 0
	defaultQCOnlyPasses = 3
	maxRetries         = 5
	maxConcurrency     = 100
	maxErrorSnippet    = 200
	defaultModel       = "gpt-4o-mini"
	defaultMaxTokens   = 8192
	defaultTemp        = 1.3
	defaultTopP        = 0.9
)

var languageMap = map[string]string{
	"ar":     "Arabic",
	"bg":     "Bulgarian",
	"cs":     "Czech",
	"da":     "Danish",
	"de":     "German",
	"el":     "Greek",
	"en-GB":  "English (UK)",
	"es-419": "Spanish (Latin America)",
	"es-ES":  "Spanish (Spain)",
	"fi":     "Finnish",
	"fr":     "French",
	"he":     "Hebrew",
	"hi":     "Hindi",
	"hr":     "Croatian",
	"hu":     "Hungarian",
	"id":     "Indonesian",
	"it":     "Italian",
	"ja":     "Japanese",
	"ko":     "Korean",
	"lt":     "Lithuanian",
	"nl":     "Dutch",
	"no":     "Norwegian",
	"pl":     "Polish",
	"pt-BR":  "Portuguese (Brazil)",
	"ro":     "Romanian",
	"ru":     "Russian",
	"sv-SE":  "Swedish",
	"th":     "Thai",
	"tr":     "Turkish",
	"uk":     "Ukrainian",
	"vi":     "Vietnamese",
	"zh-CN":  "Chinese (Simplified)",
	"zh-TW":  "Chinese (Traditional)",
}

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

type chunk struct {
	Entries      []POEntry
	StartIndex   int
	TotalEntries int
}

type translationPayload struct {
	Translations []struct {
		MsgID  string `json:"msgid"`
		MsgStr string `json:"msgstr"`
	} `json:"translations"`
}

type apiRequest struct {
	Model          string            `json:"model"`
	Messages       []chatMessage     `json:"messages"`
	Temperature    float64           `json:"temperature"`
	TopP           float64           `json:"top_p"`
	MaxTokens      int               `json:"max_tokens"`
	ResponseFormat *responseFormat   `json:"response_format,omitempty"`
	Stream         bool              `json:"stream"`
}

type responseFormat struct {
	Type string `json:"type"`
}

type chatMessage struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

type apiResponse struct {
	Choices []struct {
		Message struct {
			Content string `json:"content"`
		} `json:"message"`
	} `json:"choices"`
	Error *struct {
		Message string `json:"message"`
		Code    string `json:"code"`
	} `json:"error,omitempty"`
}

type retryableError struct {
	wait time.Duration
	err  error
}

func (e retryableError) Error() string {
	return e.err.Error()
}

type requestMeta struct {
	locale      string
	phase       string
	chunkNumber int
	totalChunks int
	pass        int
	promptChars int
}

type requestState struct {
	meta  requestMeta
	start time.Time
}

type requestTracker struct {
	mu       sync.Mutex
	inFlight map[string]requestState
}

type localeProgress struct {
	locale            string
	localeName        string
	totalChunks       int
	completed         int32
	failed            int32
	inProgress        int32
	qcCompleted       int32
	qcFailed          int32
	qcInProgress      int32
	currentPhase      string
	translations      map[string]string
	chunkTranslations []map[string]string
	errors            []string
	mu                sync.Mutex
}

type progressTracker struct {
	locales   map[string]*localeProgress
	startTime time.Time
	requests  *requestTracker
	slowAfter time.Duration
	lastSlow  time.Time
	qcPasses  int
	mu        sync.RWMutex
}

func main() {
	chunkSize := flag.Int("chunk-size", defaultChunkSize, "Number of msgids per prompt chunk")
	localesDir := flag.String("locales-dir", "../src/locales", "Path to the locales directory")
	apiEndpoint := flag.String("api", defaultAPIEndpoint, "API endpoint for translation")
	apiTimeout := flag.Duration("timeout", defaultAPITimeout, "Timeout for each API request")
	concurrency := flag.Int("concurrency", maxConcurrency, "Simultaneous API calls")
	dryRun := flag.Bool("dry-run", false, "Generate prompts without calling API")
	singleLocale := flag.String("locale", "", "Process only this locale (empty = all)")
	logRequests := flag.Bool("log-requests", false, "Log each API request start/end")
	slowAfter := flag.Duration("slow-request", 2*time.Minute, "Highlight slow API requests (0 to disable)")
	qcOnly := flag.Bool("qc-only", false, "Run QC on all existing translations (no new translations)")
	qcPassesFlag := flag.Int("qc-passes", -1, "Number of QC passes (-1 = auto: 0 for translate mode, 3 for qc-only mode)")
	if err := flag.CommandLine.Parse(stripArgSeparators(os.Args[1:])); err != nil {
		fmt.Printf("Failed to parse flags: %v\n", err)
		os.Exit(1)
	}

	log.SetFlags(0)

	if err := validateAPIConfig(*apiEndpoint, *dryRun); err != nil {
		fmt.Printf("API configuration error: %v\n", err)
		os.Exit(1)
	}

	absLocalesDir, err := absPath(*localesDir)
	if err != nil {
		fmt.Printf("Failed to resolve locales directory: %v\n", err)
		os.Exit(1)
	}

	if *chunkSize <= 0 {
		fmt.Printf("chunk-size must be > 0\n")
		os.Exit(1)
	}

	if *concurrency <= 0 {
		fmt.Printf("concurrency must be > 0\n")
		os.Exit(1)
	}

	qcPasses := *qcPassesFlag
	if qcPasses < 0 {
		if *qcOnly {
			qcPasses = defaultQCOnlyPasses
		} else {
			qcPasses = defaultQCPasses
		}
	}

	if err := runTranslation(absLocalesDir, *chunkSize, *apiEndpoint, *apiTimeout, *concurrency, *dryRun, *singleLocale, *logRequests, *slowAfter, *qcOnly, qcPasses); err != nil {
		fmt.Printf("\nTranslation failed: %v\n", err)
		os.Exit(1)
	}
}

func runTranslation(localesDir string, chunkSize int, apiEndpoint string, apiTimeout time.Duration, concurrency int, dryRun bool, singleLocale string, logRequests bool, slowAfter time.Duration, qcOnly bool, qcPasses int) error {
	referenceFile := filepath.Join(localesDir, "en-US", "messages.po")
	reference, err := parsePOFile(referenceFile)
	if err != nil {
		return fmt.Errorf("parsing reference PO: %w", err)
	}
	if len(reference.Entries) == 0 {
		return fmt.Errorf("reference file %s does not contain any entries", referenceFile)
	}

	locales, err := discoverLocales(localesDir)
	if err != nil {
		return fmt.Errorf("discovering locales: %w", err)
	}

	var targetLocales []string
	for _, locale := range locales {
		if locale == "en-US" {
			continue
		}
		if singleLocale != "" && locale != singleLocale {
			continue
		}
		targetLocales = append(targetLocales, locale)
	}

	if len(targetLocales) == 0 {
		return fmt.Errorf("no target locales found")
	}

	tracker := &progressTracker{
		locales:   make(map[string]*localeProgress),
		startTime: time.Now(),
		requests:  newRequestTracker(),
		slowAfter: slowAfter,
		qcPasses:  qcPasses,
	}

	for _, locale := range targetLocales {
		localeName := languageName(locale)
		chunks, err := buildChunks(reference.Entries, chunkSize, localeName, locale)
		if err != nil {
			return fmt.Errorf("building chunks for %s: %w", locale, err)
		}
		chunkTranslations := make([]map[string]string, len(chunks))
		for i := range chunkTranslations {
			chunkTranslations[i] = make(map[string]string)
		}
		tracker.locales[locale] = &localeProgress{
			locale:            locale,
			localeName:        localeName,
			totalChunks:       len(chunks),
			currentPhase:      "translate",
			translations:      make(map[string]string),
			chunkTranslations: chunkTranslations,
		}
	}

	totalChunks := 0
	for _, lp := range tracker.locales {
		totalChunks += lp.totalChunks
	}

	fmt.Printf("╔════════════════════════════════════════════════════════════════════╗\n")
	if qcOnly {
		fmt.Printf("║  Fluxer Locale QC (Quality Control)                                ║\n")
	} else {
		fmt.Printf("║  Fluxer Locale Translation                                         ║\n")
	}
	fmt.Printf("╠════════════════════════════════════════════════════════════════════╣\n")
	fmt.Printf("║  Source strings: %-5d                                             ║\n", len(reference.Entries))
	fmt.Printf("║  Target locales: %-5d                                             ║\n", len(targetLocales))
	if qcOnly {
		fmt.Printf("║  Max chunks:     %-5d (QC on all existing translations)           ║\n", totalChunks)
	} else {
		fmt.Printf("║  Max chunks:     %-5d (will skip already-translated)              ║\n", totalChunks)
	}
	fmt.Printf("║  Chunk size:     %-5d strings                                     ║\n", chunkSize)
	fmt.Printf("║  QC passes:      %-5d (per chunk)                                 ║\n", qcPasses)
	fmt.Printf("║  Concurrency:    %-5d simultaneous API calls                      ║\n", concurrency)
	fmt.Printf("║  Model:          %-51s ║\n", defaultModel)
	if dryRun {
		fmt.Printf("║  Mode:           DRY RUN (no API calls)                           ║\n")
	} else if qcOnly {
		fmt.Printf("║  Mode:           QC ONLY (no new translations)                    ║\n")
		fmt.Printf("║  API endpoint:   %-51s ║\n", truncateString(apiEndpoint, 51))
	} else {
		fmt.Printf("║  API endpoint:   %-51s ║\n", truncateString(apiEndpoint, 51))
	}
	if logRequests {
		fmt.Printf("║  Request logging: enabled                                         ║\n")
	}
	proxyLabel, proxyErr := proxyLabelForEndpoint(apiEndpoint)
	if proxyErr == nil {
		fmt.Printf("║  Proxy:          %-52s║\n", truncateString(proxyLabel, 52))
	}
	if slowAfter > 0 {
		fmt.Printf("║  Slow request:   %-5s                                          ║\n", slowAfter)
	}
	fmt.Printf("╚════════════════════════════════════════════════════════════════════╝\n\n")

	if dryRun {
		return runDryRun(reference.Entries, chunkSize, tracker)
	}

	stopProgress := make(chan struct{})
	var progressWg sync.WaitGroup
	progressWg.Add(1)
	go func() {
		defer progressWg.Done()
		displayProgress(tracker, stopProgress)
	}()

	semaphore := make(chan struct{}, concurrency)

	var wg sync.WaitGroup
	transport := &http.Transport{
		Proxy:                 proxyForRequest,
		DialContext:           (&net.Dialer{Timeout: 30 * time.Second, KeepAlive: 30 * time.Second}).DialContext,
		MaxIdleConns:          concurrency * 2,
		MaxIdleConnsPerHost:   concurrency,
		MaxConnsPerHost:       concurrency,
		IdleConnTimeout:       90 * time.Second,
		ResponseHeaderTimeout: 2 * time.Minute,
		ExpectContinueTimeout: 1 * time.Second,
		TLSHandshakeTimeout:   15 * time.Second,
	}
	httpClient := &http.Client{Timeout: apiTimeout, Transport: transport}

	for _, locale := range targetLocales {
		wg.Add(1)
		go func(locale string) {
			defer wg.Done()
			processLocale(locale, reference.Entries, chunkSize, apiEndpoint, httpClient, tracker, semaphore, localesDir, logRequests, apiTimeout, qcOnly, qcPasses)
		}(locale)
	}

	wg.Wait()
	close(stopProgress)
	progressWg.Wait()

	printFinalReport(tracker)

	referenceIDs := make(map[string]struct{}, len(reference.Entries))
	for _, entry := range reference.Entries {
		referenceIDs[entry.MsgID] = struct{}{}
	}

	successCount := 0
	for _, locale := range targetLocales {
		lp := tracker.locales[locale]
		if len(lp.errors) > 0 {
			continue
		}

		if err := ensureCompleteTranslations(locale, referenceIDs, lp.translations); err != nil {
			lp.mu.Lock()
			lp.errors = append(lp.errors, err.Error())
			lp.mu.Unlock()
			continue
		}

		targetPO := filepath.Join(localesDir, locale, "messages.po")
		poFile, err := parsePOFile(targetPO)
		if err != nil {
			lp.mu.Lock()
			lp.errors = append(lp.errors, fmt.Sprintf("parsing PO file: %v", err))
			lp.mu.Unlock()
			continue
		}

		updated := applyTranslations(poFile, lp.translations)
		if err := writePOFile(targetPO, updated); err != nil {
			lp.mu.Lock()
			lp.errors = append(lp.errors, fmt.Sprintf("writing PO file: %v", err))
			lp.mu.Unlock()
			continue
		}
		log.Printf("[%s] %s (%s): wrote %s", time.Now().Format(time.RFC3339), lp.localeName, locale, targetPO)
		successCount++
	}

	fmt.Printf("\n✓ Successfully updated %d/%d locale files\n", successCount, len(targetLocales))

	failedLocales := []string{}
	for _, locale := range targetLocales {
		if len(tracker.locales[locale].errors) > 0 {
			failedLocales = append(failedLocales, locale)
		}
	}

	if len(failedLocales) > 0 {
		fmt.Printf("\n⚠ Failed locales (%d):\n", len(failedLocales))
		for _, locale := range failedLocales {
			lp := tracker.locales[locale]
			fmt.Printf("  • %s (%s):\n", lp.localeName, locale)
			for _, errMsg := range lp.errors {
				fmt.Printf("    - %s\n", errMsg)
			}
		}
		return fmt.Errorf("%d locales failed", len(failedLocales))
	}

	return nil
}

func runDryRun(entries []POEntry, chunkSize int, tracker *progressTracker) error {
	fmt.Println("Generating prompts (dry run)...")
	for locale, lp := range tracker.locales {
		chunks, _ := buildChunks(entries, chunkSize, lp.localeName, locale)
		fmt.Printf("\n═══ %s (%s) - %d chunks ═══\n", lp.localeName, locale, len(chunks))
		for i, c := range chunks {
			prompt := buildChunkPrompt(lp.localeName, locale, c, i+1, len(chunks))
			fmt.Printf("\n--- Chunk %d/%d (%d strings, %d chars) ---\n", i+1, len(chunks), len(c.Entries), len(prompt))
			fmt.Println(prompt[:min(2000, len(prompt))])
			if len(prompt) > 2000 {
				fmt.Println("... [truncated for display]")
			}
		}
	}
	return nil
}

func processLocale(locale string, entries []POEntry, chunkSize int, apiEndpoint string, client *http.Client, tracker *progressTracker, sem chan struct{}, localesDir string, logRequests bool, apiTimeout time.Duration, qcOnly bool, qcPasses int) {
	lp := tracker.locales[locale]

	targetPOPath := filepath.Join(localesDir, locale, "messages.po")
	existingPO, err := parsePOFile(targetPOPath)
	if err != nil {
		lp.mu.Lock()
		lp.errors = append(lp.errors, fmt.Sprintf("reading existing PO: %v", err))
		lp.mu.Unlock()
		return
	}

	existingTranslations := make(map[string]string)
	for _, entry := range existingPO.Entries {
		if entry.MsgStr != "" {
			existingTranslations[entry.MsgID] = entry.MsgStr
		}
	}

	lp.mu.Lock()
	for msgID, msgStr := range existingTranslations {
		lp.translations[msgID] = msgStr
	}
	lp.mu.Unlock()

	if qcOnly {
		var translatedEntries []POEntry
		for _, entry := range entries {
			if _, exists := existingTranslations[entry.MsgID]; exists {
				translatedEntries = append(translatedEntries, entry)
			}
		}

		if len(translatedEntries) == 0 {
			log.Printf("[%s] %s (%s): no existing translations to QC, skipping", time.Now().Format(time.RFC3339), lp.localeName, locale)
			lp.mu.Lock()
			lp.currentPhase = "done"
			lp.mu.Unlock()
			return
		}

		chunks, err := buildChunks(translatedEntries, chunkSize, lp.localeName, locale)
		if err != nil {
			lp.mu.Lock()
			lp.errors = append(lp.errors, fmt.Sprintf("building chunks: %v", err))
			lp.mu.Unlock()
			return
		}

		log.Printf("[%s] %s (%s): starting QC of %d existing translations in %d chunks (%d passes)", time.Now().Format(time.RFC3339), lp.localeName, locale, len(translatedEntries), len(chunks), qcPasses)

		lp.mu.Lock()
		lp.totalChunks = len(chunks)
		lp.chunkTranslations = make([]map[string]string, len(chunks))
		for i, c := range chunks {
			lp.chunkTranslations[i] = make(map[string]string)
			for _, entry := range c.Entries {
				if trans, ok := existingTranslations[entry.MsgID]; ok {
					lp.chunkTranslations[i][entry.MsgID] = trans
				}
			}
		}
		lp.currentPhase = "qc"
		lp.mu.Unlock()

		for qcPass := 1; qcPass <= qcPasses; qcPass++ {
			var qcWg sync.WaitGroup
			for i, c := range chunks {
				qcWg.Add(1)
				go func(chunkIndex int, chunkData chunk, passNum int) {
					defer qcWg.Done()

					sem <- struct{}{}
					defer func() { <-sem }()

					atomic.AddInt32(&lp.qcInProgress, 1)
					defer atomic.AddInt32(&lp.qcInProgress, -1)

					lp.mu.Lock()
					currentTranslations := make(map[string]string)
					for k, v := range lp.chunkTranslations[chunkIndex] {
						currentTranslations[k] = v
					}
					lp.mu.Unlock()

					prompt := buildQCPrompt(lp.localeName, locale, chunkData, currentTranslations, chunkIndex+1, len(chunks), passNum, qcPasses)
					meta := requestMeta{
						locale:      locale,
						phase:       "qc",
						chunkNumber: chunkIndex + 1,
						totalChunks: len(chunks),
						pass:        passNum,
						promptChars: len(prompt),
					}

					log.Printf("[%s] %s: QC chunk %d/%d pass %d/%d starting (%d strings, %d chars)", time.Now().Format(time.RFC3339), locale, chunkIndex+1, len(chunks), passNum, qcPasses, len(chunkData.Entries), len(prompt))
					startTime := time.Now()

					improvedTranslations, err := callTranslationAPI(client, apiEndpoint, prompt, meta, tracker, logRequests, apiTimeout)
					if err != nil {
						log.Printf("[%s] %s: QC chunk %d/%d pass %d failed after %s: %v", time.Now().Format(time.RFC3339), locale, chunkIndex+1, len(chunks), passNum, time.Since(startTime).Round(time.Second), err)
						atomic.AddInt32(&lp.qcFailed, 1)
						lp.mu.Lock()
						lp.errors = append(lp.errors, fmt.Sprintf("chunk %d QC pass %d: %v", chunkIndex+1, passNum, err))
						lp.mu.Unlock()
						return
					}

					log.Printf("[%s] %s: QC chunk %d/%d pass %d completed in %s (%d translations)", time.Now().Format(time.RFC3339), locale, chunkIndex+1, len(chunks), passNum, time.Since(startTime).Round(time.Second), len(improvedTranslations))

					lp.mu.Lock()
					for msgID, msgStr := range improvedTranslations {
						lp.chunkTranslations[chunkIndex][msgID] = msgStr
					}
					lp.mu.Unlock()
					atomic.AddInt32(&lp.qcCompleted, 1)
				}(i, c, qcPass)
			}
			qcWg.Wait()

			if atomic.LoadInt32(&lp.qcFailed) > 0 {
				return
			}
		}

		lp.mu.Lock()
		for _, chunkTrans := range lp.chunkTranslations {
			for msgID, msgStr := range chunkTrans {
				lp.translations[msgID] = msgStr
			}
		}
		lp.mu.Unlock()

		log.Printf("[%s] %s (%s): QC completed, %d total translations", time.Now().Format(time.RFC3339), lp.localeName, locale, len(lp.translations))
		return
	}

	var untranslatedEntries []POEntry
	for _, entry := range entries {
		if _, exists := existingTranslations[entry.MsgID]; !exists {
			untranslatedEntries = append(untranslatedEntries, entry)
		}
	}

	if len(untranslatedEntries) == 0 {
		log.Printf("[%s] %s (%s): all strings already translated, skipping", time.Now().Format(time.RFC3339), lp.localeName, locale)
		lp.mu.Lock()
		lp.currentPhase = "done"
		lp.mu.Unlock()
		return
	}

	chunks, err := buildChunks(untranslatedEntries, chunkSize, lp.localeName, locale)
	if err != nil {
		lp.mu.Lock()
		lp.errors = append(lp.errors, fmt.Sprintf("building chunks: %v", err))
		lp.mu.Unlock()
		return
	}
	log.Printf("[%s] %s (%s): starting translation of %d strings in %d chunks", time.Now().Format(time.RFC3339), lp.localeName, locale, len(untranslatedEntries), len(chunks))

	lp.mu.Lock()
	lp.totalChunks = len(chunks)
	lp.chunkTranslations = make([]map[string]string, len(chunks))
	for i := range lp.chunkTranslations {
		lp.chunkTranslations[i] = make(map[string]string)
	}
	lp.mu.Unlock()

	var chunkWg sync.WaitGroup
	for i, c := range chunks {
		chunkWg.Add(1)
		go func(chunkIndex int, chunkData chunk) {
			defer chunkWg.Done()

			sem <- struct{}{}
			defer func() { <-sem }()

			atomic.AddInt32(&lp.inProgress, 1)
			defer atomic.AddInt32(&lp.inProgress, -1)

			prompt := buildChunkPrompt(lp.localeName, locale, chunkData, chunkIndex+1, len(chunks))
			meta := requestMeta{
				locale:      locale,
				phase:       "translate",
				chunkNumber: chunkIndex + 1,
				totalChunks: len(chunks),
				pass:        0,
				promptChars: len(prompt),
			}
			log.Printf("[%s] %s: chunk %d/%d starting (%d strings, %d chars)", time.Now().Format(time.RFC3339), locale, chunkIndex+1, len(chunks), len(chunkData.Entries), len(prompt))
			startTime := time.Now()
			translations, err := callTranslationAPI(client, apiEndpoint, prompt, meta, tracker, logRequests, apiTimeout)
			if err != nil {
				log.Printf("[%s] %s: chunk %d/%d failed after %s: %v", time.Now().Format(time.RFC3339), locale, chunkIndex+1, len(chunks), time.Since(startTime).Round(time.Second), err)
				atomic.AddInt32(&lp.failed, 1)
				lp.mu.Lock()
				lp.errors = append(lp.errors, fmt.Sprintf("chunk %d translate: %v", chunkIndex+1, err))
				lp.mu.Unlock()
				return
			}

			log.Printf("[%s] %s: chunk %d/%d completed in %s (%d translations)", time.Now().Format(time.RFC3339), locale, chunkIndex+1, len(chunks), time.Since(startTime).Round(time.Second), len(translations))
			lp.mu.Lock()
			lp.chunkTranslations[chunkIndex] = translations
			lp.mu.Unlock()
			atomic.AddInt32(&lp.completed, 1)
		}(i, c)
	}
	chunkWg.Wait()

	if atomic.LoadInt32(&lp.failed) > 0 {
		log.Printf("[%s] %s (%s): translation phase failed", time.Now().Format(time.RFC3339), lp.localeName, locale)
		return
	}

	log.Printf("[%s] %s (%s): translation phase completed", time.Now().Format(time.RFC3339), lp.localeName, locale)

	lp.mu.Lock()
	lp.currentPhase = "qc"
	lp.mu.Unlock()
	if qcPasses > 0 {
		log.Printf("[%s] %s (%s): starting QC phase (%d passes)", time.Now().Format(time.RFC3339), lp.localeName, locale, qcPasses)
	}

	for qcPass := 1; qcPass <= qcPasses; qcPass++ {
		var qcWg sync.WaitGroup
		for i, c := range chunks {
			qcWg.Add(1)
			go func(chunkIndex int, chunkData chunk, passNum int) {
				defer qcWg.Done()

				sem <- struct{}{}
				defer func() { <-sem }()

				atomic.AddInt32(&lp.qcInProgress, 1)
				defer atomic.AddInt32(&lp.qcInProgress, -1)

				lp.mu.Lock()
				currentTranslations := make(map[string]string)
				for k, v := range lp.chunkTranslations[chunkIndex] {
					currentTranslations[k] = v
				}
				lp.mu.Unlock()

				prompt := buildQCPrompt(lp.localeName, locale, chunkData, currentTranslations, chunkIndex+1, len(chunks), passNum, qcPasses)
				meta := requestMeta{
					locale:      locale,
					phase:       "qc",
					chunkNumber: chunkIndex + 1,
					totalChunks: len(chunks),
					pass:        passNum,
					promptChars: len(prompt),
				}
				improvedTranslations, err := callTranslationAPI(client, apiEndpoint, prompt, meta, tracker, logRequests, apiTimeout)
				if err != nil {
					atomic.AddInt32(&lp.qcFailed, 1)
					lp.mu.Lock()
					lp.errors = append(lp.errors, fmt.Sprintf("chunk %d QC pass %d: %v", chunkIndex+1, passNum, err))
					lp.mu.Unlock()
					return
				}

				lp.mu.Lock()
				for msgID, msgStr := range improvedTranslations {
					lp.chunkTranslations[chunkIndex][msgID] = msgStr
				}
				lp.mu.Unlock()
				atomic.AddInt32(&lp.qcCompleted, 1)
			}(i, c, qcPass)
		}
		qcWg.Wait()

		if atomic.LoadInt32(&lp.qcFailed) > 0 {
			return
		}
	}

	lp.mu.Lock()
	for _, chunkTrans := range lp.chunkTranslations {
		for msgID, msgStr := range chunkTrans {
			lp.translations[msgID] = msgStr
		}
	}
	lp.mu.Unlock()

	log.Printf("[%s] %s (%s): all phases completed, %d total translations", time.Now().Format(time.RFC3339), lp.localeName, locale, len(lp.translations))
}

func callTranslationAPI(client *http.Client, endpoint, prompt string, meta requestMeta, tracker *progressTracker, logRequests bool, apiTimeout time.Duration) (map[string]string, error) {
	var lastErr error

	for attempt := 1; attempt <= maxRetries; attempt++ {
		result, err := doTranslationRequest(client, endpoint, prompt, meta, tracker, logRequests, apiTimeout, attempt)
		if err == nil {
			return result, nil
		}
		lastErr = err
		if attempt < maxRetries {
			var waitDuration time.Duration
			if rerr, ok := err.(retryableError); ok {
				waitDuration = rerr.wait
				if waitDuration == 0 {
					waitDuration = time.Duration(attempt) * 2 * time.Second
				}
			} else {
				waitDuration = time.Duration(attempt) * 2 * time.Second
			}
			log.Printf("[%s] %s: chunk %d/%d attempt %d failed, retrying in %s: %v", time.Now().Format(time.RFC3339), meta.locale, meta.chunkNumber, meta.totalChunks, attempt, waitDuration.Round(time.Second), err)
			time.Sleep(waitDuration)
			continue
		}
	}

	return nil, fmt.Errorf("after %d attempts: %w", maxRetries, lastErr)
}

func doTranslationRequest(client *http.Client, endpoint, prompt string, meta requestMeta, tracker *progressTracker, logRequests bool, apiTimeout time.Duration, attempt int) (map[string]string, error) {
	reqBody, err := json.Marshal(apiRequest{
		Model: defaultModel,
		Messages: []chatMessage{
			{Role: "user", Content: prompt},
		},
		Temperature:    defaultTemp,
		TopP:           defaultTopP,
		MaxTokens:      defaultMaxTokens,
		ResponseFormat: &responseFormat{Type: "json_object"},
	})
	if err != nil {
		return nil, fmt.Errorf("marshaling request: %w", err)
	}

	ctx := context.Background()
	if apiTimeout > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, apiTimeout)
		defer cancel()
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, endpoint, bytes.NewReader(reqBody))
	if err != nil {
		return nil, fmt.Errorf("creating request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")
	if apiKey := os.Getenv("OPENROUTER_API_KEY"); apiKey != "" {
		req.Header.Set("Authorization", "Bearer "+apiKey)
	}

	reqID := tracker.requests.start(meta)
	if logRequests {
		logRequestStart(meta, attempt)
	}
	start := time.Now()

	resp, err := client.Do(req)
	if err != nil {
		tracker.requests.finish(reqID)
		log.Printf("[%s] %s: chunk %d/%d HTTP error after %s: %v", time.Now().Format(time.RFC3339), meta.locale, meta.chunkNumber, meta.totalChunks, time.Since(start).Round(time.Second), err)
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), 0, 0, err)
		}
		return nil, fmt.Errorf("API request failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		tracker.requests.finish(reqID)
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), fmt.Errorf("status %d", resp.StatusCode))
		}
		wait := retryAfterDuration(resp)
		if resp.StatusCode == http.StatusTooManyRequests || resp.StatusCode == http.StatusServiceUnavailable || resp.StatusCode == http.StatusInternalServerError {
			minWait := time.Second * time.Duration(attempt) * 2
			if wait < minWait {
				wait = minWait
			}
			return nil, retryableError{
				wait: wait,
				err:  fmt.Errorf("API returned status %d: %s", resp.StatusCode, truncateForError(string(body))),
			}
		}
		return nil, fmt.Errorf("API returned status %d: %s", resp.StatusCode, truncateForError(string(body)))
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		tracker.requests.finish(reqID)
		log.Printf("[%s] %s: chunk %d/%d body read error after %s: %v", time.Now().Format(time.RFC3339), meta.locale, meta.chunkNumber, meta.totalChunks, time.Since(start).Round(time.Second), err)
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, 0, err)
		}
		return nil, fmt.Errorf("reading response: %w", err)
	}
	log.Printf("[%s] %s: chunk %d/%d response complete (%d bytes) in %s", time.Now().Format(time.RFC3339), meta.locale, meta.chunkNumber, meta.totalChunks, len(body), time.Since(start).Round(time.Second))

	var apiResp apiResponse
	if err := json.Unmarshal(body, &apiResp); err != nil {
		tracker.requests.finish(reqID)
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), err)
		}
		return nil, fmt.Errorf("decoding response: %w", err)
	}

	if apiResp.Error != nil {
		tracker.requests.finish(reqID)
		err := fmt.Errorf("API error: %s (%s)", apiResp.Error.Message, apiResp.Error.Code)
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), err)
		}
		return nil, err
	}

	if len(apiResp.Choices) == 0 || strings.TrimSpace(apiResp.Choices[0].Message.Content) == "" {
		tracker.requests.finish(reqID)
		err := fmt.Errorf("API returned empty response field (body=%q)", truncateForError(string(body)))
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), err)
		}
		return nil, err
	}

	content := strings.TrimSpace(apiResp.Choices[0].Message.Content)
	payload, err := extractPayload(content)
	if err != nil {
		tracker.requests.finish(reqID)
		suffix := content
		if len(suffix) > 200 {
			suffix = suffix[len(suffix)-200:]
		}
		err = fmt.Errorf("%w (response_len=%d response_prefix=%q response_suffix=%q)", err, len(content), truncateForError(content), suffix)
		if logRequests {
			logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), err)
		}
		return nil, fmt.Errorf("extracting JSON: %w", err)
	}

	var parsed translationPayload
	if err := json.Unmarshal(payload, &parsed); err != nil {
		parsed = extractPartialTranslations(string(payload))
		if len(parsed.Translations) == 0 {
			tracker.requests.finish(reqID)
			if logRequests {
				logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), err)
			}
			return nil, fmt.Errorf("parsing JSON: %w", err)
		}
		log.Printf("[%s] %s: chunk %d/%d recovered %d partial translations from malformed JSON", time.Now().Format(time.RFC3339), meta.locale, meta.chunkNumber, meta.totalChunks, len(parsed.Translations))
	}

	result := make(map[string]string)
	for _, t := range parsed.Translations {
		result[t.MsgID] = t.MsgStr
	}
	tracker.requests.finish(reqID)
	if logRequests {
		logRequestEnd(meta, attempt, time.Since(start), resp.StatusCode, len(body), nil)
	}
	return result, nil
}

func displayProgress(tracker *progressTracker, stop chan struct{}) {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case <-stop:
			printProgressUpdate(tracker, true)
			return
		case <-ticker.C:
			printProgressUpdate(tracker, false)
		}
	}
}

func printProgressUpdate(tracker *progressTracker, final bool) {
	tracker.mu.RLock()

	var totalCompleted, totalFailed, totalInProgress, totalChunks int32
	var totalQCCompleted, totalQCFailed, totalQCInProgress, totalQCExpected int32
	inQCPhase := false

	for _, lp := range tracker.locales {
		totalCompleted += atomic.LoadInt32(&lp.completed)
		totalFailed += atomic.LoadInt32(&lp.failed)
		totalInProgress += atomic.LoadInt32(&lp.inProgress)
		totalChunks += int32(lp.totalChunks)

		totalQCCompleted += atomic.LoadInt32(&lp.qcCompleted)
		totalQCFailed += atomic.LoadInt32(&lp.qcFailed)
		totalQCInProgress += atomic.LoadInt32(&lp.qcInProgress)
		totalQCExpected += int32(lp.totalChunks * tracker.qcPasses)

		lp.mu.Lock()
		if lp.currentPhase == "qc" {
			inQCPhase = true
		}
		lp.mu.Unlock()
	}

	elapsed := time.Since(tracker.startTime).Round(time.Second)

	fmt.Printf("\r\033[K")

	completedLocales := 0
	failedLocales := 0
	for _, lp := range tracker.locales {
		completed := atomic.LoadInt32(&lp.completed)
		failed := atomic.LoadInt32(&lp.failed)
		qcCompleted := atomic.LoadInt32(&lp.qcCompleted)
		qcFailed := atomic.LoadInt32(&lp.qcFailed)
		expectedQC := int32(lp.totalChunks * tracker.qcPasses)

		if failed > 0 || qcFailed > 0 {
			failedLocales++
		} else if int(completed) >= lp.totalChunks && qcCompleted >= expectedQC {
			completedLocales++
		}
	}

	totalLocales := len(tracker.locales)

	statusParts := []string{}

	if !inQCPhase || totalInProgress > 0 {
		translatePct := float64(totalCompleted+totalFailed) / float64(totalChunks) * 100
		if totalInProgress > 0 {
			statusParts = append(statusParts, fmt.Sprintf("Translate: %.0f%% (%d in flight)", translatePct, totalInProgress))
		} else if totalCompleted+totalFailed < totalChunks {
			statusParts = append(statusParts, fmt.Sprintf("Translate: %.0f%%", translatePct))
		} else {
			statusParts = append(statusParts, "Translate: done")
		}
	} else {
		statusParts = append(statusParts, "Translate: done")
	}

	if inQCPhase || totalQCCompleted > 0 || totalQCInProgress > 0 {
		qcPct := float64(totalQCCompleted+totalQCFailed) / float64(totalQCExpected) * 100
		if totalQCInProgress > 0 {
			statusParts = append(statusParts, fmt.Sprintf("QC: %.0f%% (%d in flight)", qcPct, totalQCInProgress))
		} else {
			statusParts = append(statusParts, fmt.Sprintf("QC: %.0f%%", qcPct))
		}
	}

	if completedLocales > 0 {
		statusParts = append(statusParts, fmt.Sprintf("✓ %d/%d locales", completedLocales, totalLocales))
	}
	if failedLocales > 0 {
		statusParts = append(statusParts, fmt.Sprintf("✗ %d failed", failedLocales))
	}

	status := strings.Join(statusParts, " | ")
	if status == "" {
		status = "Starting..."
	}

	tracker.mu.RUnlock()

	oldestState, oldestAge, inFlightCount, ok := tracker.requests.oldest()
	if ok && tracker.slowAfter > 0 && oldestAge >= tracker.slowAfter {
		status = fmt.Sprintf("%s | Slow: %s (%s %d/%d p%d, %s, %d in flight)", status, oldestAge.Round(time.Second), oldestState.meta.locale, oldestState.meta.chunkNumber, oldestState.meta.totalChunks, oldestState.meta.pass, oldestState.meta.phase, inFlightCount)
		if !final {
			tracker.mu.Lock()
			if time.Since(tracker.lastSlow) > time.Minute {
				tracker.lastSlow = time.Now()
				tracker.mu.Unlock()
				logSlowRequest(oldestState, oldestAge, inFlightCount)
			} else {
				tracker.mu.Unlock()
			}
		}
	}

	fmt.Printf("[%s] %s", elapsed, status)

	if final {
		fmt.Println()
	}
}

func printFinalReport(tracker *progressTracker) {
	fmt.Println("\n\n╔════════════════════════════════════════════════════════════════════════════╗")
	fmt.Println("║  Translation Results                                                         ║")
	fmt.Println("╠════════════════════════════════════════════════════════════════════════════╣")

	locales := make([]string, 0, len(tracker.locales))
	for locale := range tracker.locales {
		locales = append(locales, locale)
	}
	sort.Strings(locales)

	for _, locale := range locales {
		lp := tracker.locales[locale]
		completed := atomic.LoadInt32(&lp.completed)
		failed := atomic.LoadInt32(&lp.failed)
		qcCompleted := atomic.LoadInt32(&lp.qcCompleted)
		qcFailed := atomic.LoadInt32(&lp.qcFailed)
		expectedQC := int32(lp.totalChunks * tracker.qcPasses)

		status := "✓"
		if failed > 0 || qcFailed > 0 {
			status = "✗"
		} else if int(completed) < lp.totalChunks || qcCompleted < expectedQC {
			status = "⚠"
		}

		name := fmt.Sprintf("%s (%s)", lp.localeName, locale)
		translateProgress := fmt.Sprintf("%d/%d", completed, lp.totalChunks)
		qcProgress := fmt.Sprintf("%d/%d QC", qcCompleted, expectedQC)
		translations := fmt.Sprintf("%d strings", len(lp.translations))

		fmt.Printf("║  %s %-26s %8s  %10s  %12s  ║\n", status, name, translateProgress, qcProgress, translations)
	}

	fmt.Println("╚════════════════════════════════════════════════════════════════════════════╝")
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

func truncateForError(s string) string {
	s = strings.ReplaceAll(s, "\r\n", "\n")
	s = strings.TrimSpace(s)
	if len(s) <= maxErrorSnippet {
		return s
	}
	return s[:maxErrorSnippet-3] + "..."
}

func retryAfterDuration(resp *http.Response) time.Duration {
	value := strings.TrimSpace(resp.Header.Get("Retry-After"))
	if value == "" {
		return 0
	}
	if seconds, err := strconv.Atoi(value); err == nil {
		return time.Duration(seconds) * time.Second
	}
	if ts, err := http.ParseTime(value); err == nil {
		wait := time.Until(ts)
		if wait < 0 {
			return 0
		}
		return wait
	}
	return 0
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

func validateAPIConfig(endpoint string, dryRun bool) error {
	if dryRun {
		return nil
	}
	if strings.TrimSpace(endpoint) == "" {
		return fmt.Errorf("api endpoint is required")
	}
	if _, err := url.ParseRequestURI(endpoint); err != nil {
		return fmt.Errorf("invalid api endpoint: %w", err)
	}
	if os.Getenv("OPENROUTER_API_KEY") == "" {
		return fmt.Errorf("OPENROUTER_API_KEY environment variable is required")
	}
	return nil
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

func buildChunks(entries []POEntry, chunkSize int, localeName, localeCode string) ([]chunk, error) {
	if chunkSize <= 0 {
		chunkSize = defaultChunkSize
	}
	var chunks []chunk
	for i := 0; i < len(entries); i += chunkSize {
		end := i + chunkSize
		if end > len(entries) {
			end = len(entries)
		}
		chunkEntries := append([]POEntry(nil), entries[i:end]...)
		chunks = append(chunks, chunk{Entries: chunkEntries})
	}
	assignChunkIndices(chunks)

	for {
		totalChunks := len(chunks)
		tooLong := -1
		for idx := range chunks {
			prompt := buildChunkPrompt(localeName, localeCode, chunks[idx], idx+1, totalChunks)
			if len(prompt) > maxPromptLength {
				tooLong = idx
				break
			}
		}
		if tooLong == -1 {
			break
		}
		if len(chunks[tooLong].Entries) == 1 {
			return nil, fmt.Errorf("%s chunk %d exceeds %d characters", localeCode, tooLong+1, maxPromptLength)
		}
		overflow := chunks[tooLong].Entries[len(chunks[tooLong].Entries)-1]
		chunks[tooLong].Entries = chunks[tooLong].Entries[:len(chunks[tooLong].Entries)-1]
		if tooLong+1 < len(chunks) {
			chunks[tooLong+1].Entries = append([]POEntry{overflow}, chunks[tooLong+1].Entries...)
		} else {
			chunks = append(chunks, chunk{Entries: []POEntry{overflow}})
		}
		assignChunkIndices(chunks)
	}

	return chunks, nil
}

func assignChunkIndices(chunks []chunk) {
	totalEntries := 0
	for _, c := range chunks {
		totalEntries += len(c.Entries)
	}
	nextStart := 1
	for i := range chunks {
		chunks[i].StartIndex = nextStart
		chunks[i].TotalEntries = totalEntries
		nextStart += len(chunks[i].Entries)
	}
}

func buildChunkPrompt(localeName, localeCode string, c chunk, chunkNumber, totalChunks int) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("# Localize chat-app UI strings to %s (%s) - chunk %d/%d\n\n", localeName, localeCode, chunkNumber, totalChunks))

	sb.WriteString("## IMPORTANT: Speed mode - do NOT use extended thinking\n")
	sb.WriteString("This is a FAST translation pass. Do NOT use extended thinking, chain-of-thought, or detailed reasoning.\n")
	sb.WriteString("Translate directly and immediately. Speed is critical - we have many strings to process.\n")
	sb.WriteString("A separate QC pass will review and refine translations later, so prioritize throughput over perfection.\n")
	sb.WriteString("Just read each string, translate it, and move on. No deliberation needed.\n\n")

	sb.WriteString(fmt.Sprintf(
		"You are a professional translator quickly localizing %d UI/chat strings from American English into %s for a modern chat application.\n\n",
		len(c.Entries), localeName,
	))

	sb.WriteString("## Your mission\n")
	sb.WriteString("Translate EVERY string below into natural " + localeName + ". Work quickly - a QC pass will catch any issues later.\n\n")

	sb.WriteString("## CRITICAL: Complete translation required\n")
	sb.WriteString("- You MUST translate EVERY string. Do not skip any.\n")
	sb.WriteString("- Do not leave any string in English unless it is a proper noun, brand name, or technical term that is genuinely used untranslated in " + localeName + " contexts.\n")
	sb.WriteString("- If you are unsure whether a word should remain in English, translate it. Only keep English words that are definitively used as-is in " + localeName + " tech/chat contexts.\n")
	sb.WriteString("- Common UI terms like \"server\", \"channel\", \"message\", \"notification\", \"settings\" etc. should be translated unless they are genuinely anglicisms in " + localeName + ".\n\n")

	writeLanguageSpecificGuidance(&sb, localeCode, localeName)

	sb.WriteString("## Unicode and character encoding\n")
	sb.WriteString("- Output is JSON with UTF-8 encoding. Use native characters for your language.\n")
	sb.WriteString("- Use proper diacritics, accents, and native script as appropriate for " + localeName + ".\n")
	sb.WriteString("- Do NOT transliterate to ASCII or strip diacritics.\n")
	sb.WriteString("- JSON structure uses ASCII punctuation, but msgstr content uses native characters.\n\n")

	sb.WriteString("## Formatting preservation (STRICT)\n")
	sb.WriteString("- Preserve ALL placeholders exactly: {name}, {0}, {count}, %@, %d, {{variable}}, <emoji>, <br/>, etc.\n")
	sb.WriteString("- Do not translate, rename, reorder, add, or remove placeholders.\n")
	sb.WriteString("- Preserve markdown formatting: **bold**, *italic*, `code`, [links](url)\n")
	sb.WriteString("- Preserve line breaks, leading/trailing spaces, and emoji positions.\n")
	sb.WriteString("- Preserve URLs, email addresses, @mentions, #hashtags, and file paths exactly.\n\n")

	sb.WriteString("## Tone and style\n")
	sb.WriteString("- Match the source tone: playful stays playful, formal stays formal, casual stays casual.\n")
	sb.WriteString("- Chat apps use friendly, concise language. Avoid overly formal or bureaucratic phrasing.\n")
	sb.WriteString("- Keep button labels and calls-to-action short and punchy.\n")
	sb.WriteString("- Preserve humor and personality - adapt jokes/wordplay to work in " + localeName + " while keeping the same spirit.\n")
	sb.WriteString("- If the source uses colloquialisms, use natural " + localeName + " equivalents rather than literal translations.\n\n")

	sb.WriteString("## Technical terms and anglicisms\n")
	sb.WriteString("- Translate technical UI terms when " + localeName + " has established equivalents.\n")
	sb.WriteString("- Only keep English terms that are genuinely used as anglicisms in " + localeName + " tech contexts.\n")
	sb.WriteString("- When in doubt, translate. Do not assume a term should stay in English.\n")
	sb.WriteString("- Brand names (Fluxer, iOS, Android, etc.) stay as-is.\n")
	sb.WriteString("- Product tier names (Fluxer Plutonium, etc.) stay as-is.\n\n")

	sb.WriteString("## Search syntax\n")
	sb.WriteString("- Translate search operator keywords like \"from:\", \"to:\", \"in:\", \"before:\", \"after:\", \"has:\"\n")
	sb.WriteString("- Keep the colon and query structure intact.\n")
	sb.WriteString("- Do not translate placeholder values, usernames, or technical identifiers.\n\n")

	sb.WriteString("## Capitalization\n")
	sb.WriteString("- Follow " + localeName + " capitalization conventions.\n")
	sb.WriteString("- For languages using sentence case (most languages), only capitalize first word and proper nouns.\n")
	sb.WriteString("- Preserve capitalization of brand names and acronyms.\n\n")

	sb.WriteString("## Quote characters (STRICT)\n")
	sb.WriteString("- Use only ASCII straight quotes in JSON output: \" (U+0022) and ' (U+0027)\n")
	sb.WriteString("- Never use curly/smart quotes in the output.\n\n")

	sb.WriteString("## Bidirectional text safety\n")
	sb.WriteString("- Do not include invisible Unicode characters (U+200B-U+200F, U+202A-U+202E, U+2066-U+2069, U+FEFF, U+00AD, U+061C).\n")
	sb.WriteString("- For RTL languages: rephrase to avoid placeholder ordering issues rather than adding direction markers.\n\n")

	sb.WriteString(fmt.Sprintf("## Strings %d-%d of %d\n\n", c.StartIndex, min(c.StartIndex+len(c.Entries)-1, c.TotalEntries), c.TotalEntries))
	sb.WriteString("Translate each msgid to msgstr:\n\n")

	for idx, entry := range c.Entries {
		sb.WriteString(fmt.Sprintf("%d.\n", c.StartIndex+idx))
		if len(entry.Comments) > 0 {
			sb.WriteString(fmt.Sprintf("Context/notes: %s\n", strings.Join(entry.Comments, " | ")))
		}
		sb.WriteString("```text\n")
		sb.WriteString(entry.MsgID)
		sb.WriteString("\n```\n\n")
	}

	sb.WriteString("## Pre-submission checklist\n")
	sb.WriteString("Before outputting, verify:\n")
	sb.WriteString("1. Every string is translated (no English unless it's a genuine anglicism)\n")
	sb.WriteString("2. All placeholders preserved exactly\n")
	sb.WriteString("3. Translations sound natural in " + localeName + "\n")
	sb.WriteString("4. JSON is valid\n\n")

	sb.WriteString("## Required output format\n")
	sb.WriteString("Output ONLY a single markdown code block with valid JSON. No other text.\n\n")
	sb.WriteString("```json\n")
	sb.WriteString("{\n")
	sb.WriteString("  \"translations\": [\n")
	sb.WriteString("    { \"msgid\": \"original text\", \"msgstr\": \"translated text\" }\n")
	sb.WriteString("  ]\n")
	sb.WriteString("}\n")
	sb.WriteString("```\n\n")
	sb.WriteString("The translations array must contain every msgid from this chunk, in the same order.\n")

	return sb.String()
}

func buildQCPrompt(localeName, localeCode string, c chunk, currentTranslations map[string]string, chunkNumber, totalChunks, qcPassNum, totalQCPasses int) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("# Quality Control Review: %s (%s) - chunk %d/%d - pass %d/%d\n\n", localeName, localeCode, chunkNumber, totalChunks, qcPassNum, totalQCPasses))

	sb.WriteString("## IMPORTANT: Speed mode - do NOT use extended thinking\n")
	sb.WriteString("This is a FAST QC pass. Do NOT use extended thinking, chain-of-thought, or detailed reasoning.\n")
	sb.WriteString("Review and improve translations quickly. Speed is critical - we have many strings to process.\n")
	sb.WriteString("We run multiple QC passes, so prioritize throughput over perfection on any single pass.\n")
	sb.WriteString("Just read each translation, fix obvious issues, and move on. No deliberation needed.\n\n")

	sb.WriteString(fmt.Sprintf(
		"You are a %s translator quickly reviewing and improving %d translated UI/chat strings.\n\n",
		localeName, len(c.Entries),
	))

	sb.WriteString("## Your mission\n")
	sb.WriteString("Quickly review each translation below and improve it if needed. Look for obvious issues:\n\n")

	sb.WriteString("### Issues to fix\n")
	sb.WriteString("1. **Untranslated text**: Any English words that should be translated (except brand names, accepted anglicisms)\n")
	sb.WriteString("2. **Unnatural phrasing**: Translations that sound machine-translated or awkward to native speakers\n")
	sb.WriteString("3. **Wrong register**: Too formal or too informal for a chat app context\n")
	sb.WriteString("4. **Grammar/spelling errors**: Including missing or incorrect diacritics, wrong cases, gender agreement issues\n")
	sb.WriteString("5. **Inconsistent terminology**: Different translations for the same concept within this chunk\n")
	sb.WriteString("6. **Tone mismatch**: Not matching the original tone (playful, serious, casual, etc.)\n")
	sb.WriteString("7. **Lost meaning**: Translation that doesn't convey the same meaning as the original\n")
	sb.WriteString("8. **Placeholder issues**: Modified, missing, or reordered placeholders\n")
	sb.WriteString("9. **Cultural inappropriateness**: Phrases that don't work well in " + localeName + " culture\n")
	sb.WriteString("10. **Verbosity**: Translations that are unnecessarily long for UI context\n\n")

	sb.WriteString("### What to preserve\n")
	sb.WriteString("- If a translation is already good, keep it exactly as-is\n")
	sb.WriteString("- All placeholders must remain exactly as in the original: {name}, {0}, {count}, etc.\n")
	sb.WriteString("- Brand names: Fluxer, iOS, Android, etc.\n")
	sb.WriteString("- Product tier names: Fluxer Plutonium, etc.\n")
	sb.WriteString("- Markdown formatting, URLs, @mentions, #hashtags\n\n")

	writeLanguageSpecificGuidance(&sb, localeCode, localeName)

	sb.WriteString("## Strings to review\n\n")
	sb.WriteString("For each item, you see the original English (msgid) and the current translation (msgstr).\n")
	sb.WriteString("Improve the msgstr if needed, or keep it exactly the same if it's already good.\n\n")

	for idx, entry := range c.Entries {
		currentTrans := currentTranslations[entry.MsgID]
		sb.WriteString(fmt.Sprintf("### %d.\n", c.StartIndex+idx))
		if len(entry.Comments) > 0 {
			sb.WriteString(fmt.Sprintf("**Context/notes:** %s\n\n", strings.Join(entry.Comments, " | ")))
		}
		sb.WriteString("**Original (English):**\n```text\n")
		sb.WriteString(entry.MsgID)
		sb.WriteString("\n```\n\n")
		sb.WriteString("**Current translation:**\n```text\n")
		sb.WriteString(currentTrans)
		sb.WriteString("\n```\n\n")
	}

	sb.WriteString("## Review checklist\n")
	sb.WriteString("Before outputting, verify each translation:\n")
	sb.WriteString("1. Sounds natural to a native " + localeName + " speaker\n")
	sb.WriteString("2. Uses correct spelling, grammar, and diacritics\n")
	sb.WriteString("3. Matches the tone of the original\n")
	sb.WriteString("4. Is appropriately concise for UI\n")
	sb.WriteString("5. All placeholders preserved exactly\n")
	sb.WriteString("6. No unnecessary English words (except accepted anglicisms)\n\n")

	sb.WriteString("## Required output format\n")
	sb.WriteString("Output ONLY a single markdown code block with valid JSON. No other text.\n")
	sb.WriteString("Include ALL strings from this chunk, whether you changed them or not.\n\n")
	sb.WriteString("```json\n")
	sb.WriteString("{\n")
	sb.WriteString("  \"translations\": [\n")
	sb.WriteString("    { \"msgid\": \"original text\", \"msgstr\": \"improved or unchanged translation\" }\n")
	sb.WriteString("  ]\n")
	sb.WriteString("}\n")
	sb.WriteString("```\n\n")
	sb.WriteString("Use only ASCII straight quotes in JSON: \" and ' (never curly quotes).\n")
	sb.WriteString("The translations array must contain every msgid from this chunk, in the same order.\n")

	return sb.String()
}

func writeLanguageSpecificGuidance(sb *strings.Builder, localeCode, localeName string) {
	sb.WriteString("## Language-specific guidance for " + localeName + "\n")

	switch localeCode {
	case "ar":
		sb.WriteString("- Use Modern Standard Arabic (MSA) for broad comprehension.\n")
		sb.WriteString("- Write right-to-left. Do not add LRM/RLM markers around placeholders.\n")
		sb.WriteString("- Use Arabic numerals (٠١٢٣٤٥٦٧٨٩) or Western numerals based on context - Western numerals are acceptable in tech contexts.\n")
		sb.WriteString("- Handle grammatical gender appropriately. For generic \"you\", prefer masculine forms unless context indicates otherwise.\n")
		sb.WriteString("- Translate common tech terms: server = خادم, channel = قناة, message = رسالة, notification = إشعار, settings = الإعدادات.\n")
		sb.WriteString("- Some anglicisms are acceptable in Arabic tech contexts: emoji, GIF, username, hashtag.\n")

	case "bg":
		sb.WriteString("- Use Bulgarian Cyrillic script throughout.\n")
		sb.WriteString("- Handle grammatical gender - use masculine for generic contexts.\n")
		sb.WriteString("- Translate tech terms: сървър (server), канал (channel), съобщение (message), известие (notification), настройки (settings).\n")
		sb.WriteString("- Use definite article suffixes appropriately (-ът/-ят for masculine, -та for feminine, -то for neuter).\n")

	case "cs":
		sb.WriteString("- Use proper Czech diacritics: á, č, ď, é, ě, í, ň, ó, ř, š, ť, ú, ů, ý, ž.\n")
		sb.WriteString("- Handle grammatical cases and gender correctly.\n")
		sb.WriteString("- Translate tech terms: server = server (accepted anglicism), channel = kanál, message = zpráva, notification = oznámení, settings = nastavení.\n")
		sb.WriteString("- Use formal \"vy\" or informal \"ty\" based on context - chat apps typically use informal.\n")
		sb.WriteString("- Czech prefers translated terms over anglicisms in most cases.\n")

	case "da":
		sb.WriteString("- Use proper Danish letters: æ, ø, å. NEVER substitute with ae, oe, aa.\n")
		sb.WriteString("- Danish typically accepts many English tech terms, but translate where natural Danish exists.\n")
		sb.WriteString("- Translate: server = server (accepted), channel = kanal, message = besked, notification = notifikation/meddelelse, settings = indstillinger.\n")
		sb.WriteString("- Use Danish compound words naturally (write as single words without spaces where appropriate).\n")
		sb.WriteString("- Address users informally with \"du\" rather than formal \"De\".\n")

	case "de":
		sb.WriteString("- Use proper German capitalization: all nouns capitalized.\n")
		sb.WriteString("- Use German umlauts: ä, ö, ü, ß. Never substitute with ae, oe, ue, ss.\n")
		sb.WriteString("- Handle compound words correctly (typically written as single words).\n")
		sb.WriteString("- Translate tech terms: Server = Server (accepted), Kanal (channel), Nachricht (message), Benachrichtigung (notification), Einstellungen (settings).\n")
		sb.WriteString("- Use informal \"du\" for chat apps, not formal \"Sie\".\n")
		sb.WriteString("- German often prefers translations over anglicisms - translate when a natural German word exists.\n")

	case "el":
		sb.WriteString("- Use Greek alphabet throughout. Do not transliterate to Latin.\n")
		sb.WriteString("- Use proper Greek diacritics (tonos accent marks).\n")
		sb.WriteString("- Translate tech terms: διακομιστής (server), κανάλι (channel), μήνυμα (message), ειδοποίηση (notification), ρυθμίσεις (settings).\n")
		sb.WriteString("- Handle grammatical gender and cases correctly.\n")
		sb.WriteString("- Some English tech terms are used in Greek contexts but prefer Greek translations where natural.\n")

	case "en-GB":
		sb.WriteString("- Use British spelling: colour, favourite, organise, licence (noun), center → centre, travelling.\n")
		sb.WriteString("- Use British punctuation conventions where appropriate.\n")
		sb.WriteString("- Keep American idioms if they're universally understood; adapt region-specific ones.\n")
		sb.WriteString("- Date format preferences: day-month-year.\n")
		sb.WriteString("- Maintain the same casual, friendly tone - don't make it more formal.\n")

	case "es-419":
		sb.WriteString("- Use Latin American Spanish conventions.\n")
		sb.WriteString("- Avoid Spain-specific vocabulary (ordenador → computadora, móvil → celular).\n")
		sb.WriteString("- Use \"ustedes\" instead of \"vosotros\" for plural you.\n")
		sb.WriteString("- Translate tech terms: servidor (server), canal (channel), mensaje (message), notificación (notification), configuración/ajustes (settings).\n")
		sb.WriteString("- Keep translations neutral across Latin American regions - avoid country-specific slang.\n")
		sb.WriteString("- Use tú for informal singular address (standard in most Latin American countries for apps).\n")

	case "es-ES":
		sb.WriteString("- Use European Spanish conventions.\n")
		sb.WriteString("- Use \"vosotros\" for informal plural address where appropriate.\n")
		sb.WriteString("- Spain-specific vocabulary is acceptable: ordenador, móvil.\n")
		sb.WriteString("- Translate tech terms: servidor (server), canal (channel), mensaje (message), notificación (notification), configuración/ajustes (settings).\n")
		sb.WriteString("- Use tú for informal singular address.\n")

	case "fi":
		sb.WriteString("- Use Finnish letters: ä, ö. Never substitute with a, o.\n")
		sb.WriteString("- Handle complex Finnish grammar: 15 grammatical cases, vowel harmony.\n")
		sb.WriteString("- Translate tech terms: palvelin (server), kanava (channel), viesti (message), ilmoitus (notification), asetukset (settings).\n")
		sb.WriteString("- Finnish often creates native terms rather than borrowing - prefer Finnish words.\n")
		sb.WriteString("- Use informal \"sinä\" (you) for chat apps.\n")
		sb.WriteString("- Finnish compound words are written as single words.\n")

	case "fr":
		sb.WriteString("- Use proper French accents: é, è, ê, ë, à, â, ô, ù, û, ç, î, ï. Never omit them.\n")
		sb.WriteString("- Handle grammatical gender and agreements correctly.\n")
		sb.WriteString("- Translate tech terms: serveur (server), salon/canal (channel), message (message), notification (notification), paramètres (settings).\n")
		sb.WriteString("- Use \"tu\" for informal address in chat apps, not \"vous\".\n")
		sb.WriteString("- French often has official translations for tech terms - use them.\n")
		sb.WriteString("- Respect French spacing rules: space before ; : ? ! (in formal contexts), but chat apps can be more relaxed.\n")

	case "he":
		sb.WriteString("- Write right-to-left using Hebrew script.\n")
		sb.WriteString("- Do not add RTL/LTR markers around placeholders - rephrase if needed.\n")
		sb.WriteString("- Translate tech terms: שרת (server), ערוץ (channel), הודעה (message), התראה (notification), הגדרות (settings).\n")
		sb.WriteString("- Handle grammatical gender - use masculine for generic contexts.\n")
		sb.WriteString("- Modern Hebrew accepts some English tech terms, but prefer Hebrew translations where natural.\n")
		sb.WriteString("- Numbers can use Western numerals.\n")

	case "hi":
		sb.WriteString("- Use Devanagari script throughout.\n")
		sb.WriteString("- Hindi tech vocabulary often mixes English terms - this is acceptable for widely-used terms.\n")
		sb.WriteString("- Common anglicisms in Hindi tech: server (सर्वर), message (मैसेज), notification (नोटिफिकेशन).\n")
		sb.WriteString("- But translate where Hindi words are natural: चैनल or channel, संदेश (message), सूचना (notification), सेटिंग्स (settings).\n")
		sb.WriteString("- Use informal \"तुम\" or \"आप\" appropriately - chat apps typically use respectful but not overly formal language.\n")
		sb.WriteString("- Use Hindi punctuation: पूर्ण विराम (।) for full stops in formal text, but English period is acceptable in chat UI.\n")

	case "hr":
		sb.WriteString("- Use Croatian diacritics: č, ć, đ, š, ž. Never substitute with c, d, s, z.\n")
		sb.WriteString("- Handle grammatical cases and gender correctly.\n")
		sb.WriteString("- Translate tech terms: poslužitelj (server), kanal (channel), poruka (message), obavijest (notification), postavke (settings).\n")
		sb.WriteString("- Croatian typically prefers native terms over anglicisms.\n")
		sb.WriteString("- Use informal \"ti\" for chat apps.\n")

	case "hu":
		sb.WriteString("- Use Hungarian diacritics: á, é, í, ó, ö, ő, ú, ü, ű. Never omit them.\n")
		sb.WriteString("- Note the difference between ö/ő and ü/ű (short vs long).\n")
		sb.WriteString("- Handle vowel harmony and complex suffixes correctly.\n")
		sb.WriteString("- Translate tech terms: szerver (server - accepted anglicism), csatorna (channel), üzenet (message), értesítés (notification), beállítások (settings).\n")
		sb.WriteString("- Hungarian typically creates native terms - prefer Hungarian words where natural.\n")
		sb.WriteString("- Use informal \"te\" for chat apps.\n")

	case "id":
		sb.WriteString("- Indonesian uses Latin alphabet without diacritics.\n")
		sb.WriteString("- Indonesian often adopts English tech terms with spelling adaptation.\n")
		sb.WriteString("- Common terms: server = server, channel = kanal/channel, message = pesan, notification = notifikasi, settings = pengaturan.\n")
		sb.WriteString("- Use informal register appropriate for chat apps.\n")
		sb.WriteString("- Indonesian is relatively straightforward grammatically - no gender, cases, or complex conjugation.\n")

	case "it":
		sb.WriteString("- Use proper Italian accents: à, è, é, ì, ò, ù. Never omit them.\n")
		sb.WriteString("- Handle grammatical gender and agreements correctly.\n")
		sb.WriteString("- Translate tech terms: server = server (accepted anglicism), canale (channel), messaggio (message), notifica (notification), impostazioni (settings).\n")
		sb.WriteString("- Use informal \"tu\" for chat apps, not \"Lei\".\n")
		sb.WriteString("- Italian accepts some anglicisms in tech contexts but has translations for most UI terms.\n")

	case "ja":
		sb.WriteString("- Use appropriate script: kanji, hiragana, and katakana as contextually appropriate.\n")
		sb.WriteString("- Foreign/tech terms typically use katakana: サーバー (server), チャンネル (channel), メッセージ (message).\n")
		sb.WriteString("- Native Japanese alternatives where natural: 通知 (notification), 設定 (settings).\n")
		sb.WriteString("- Use appropriate politeness level - chat apps typically use polite but not overly formal (~です/~ます).\n")
		sb.WriteString("- Japanese doesn't require spaces between words.\n")
		sb.WriteString("- Keep translations concise - Japanese UI text should be efficient.\n")

	case "ko":
		sb.WriteString("- Use Hangul throughout. Do not romanize.\n")
		sb.WriteString("- Korean often uses English loanwords in tech contexts: 서버 (server), 채널 (channel), 메시지 (message).\n")
		sb.WriteString("- Native alternatives: 알림 (notification), 설정 (settings).\n")
		sb.WriteString("- Use appropriate speech level - chat apps typically use 해요체 (polite informal).\n")
		sb.WriteString("- Handle particles correctly (은/는, 이/가, 을/를, etc.).\n")
		sb.WriteString("- Keep translations concise and natural for UI.\n")

	case "lt":
		sb.WriteString("- Use Lithuanian letters: ą, č, ę, ė, į, š, ų, ū, ž. Never substitute.\n")
		sb.WriteString("- Handle grammatical cases and gender correctly.\n")
		sb.WriteString("- Translate tech terms: serveris (server), kanalas (channel), žinutė (message), pranešimas (notification), nustatymai (settings).\n")
		sb.WriteString("- Lithuanian typically prefers native translations over anglicisms.\n")
		sb.WriteString("- Use informal \"tu\" for chat apps.\n")

	case "nl":
		sb.WriteString("- Dutch is close to English but has its own vocabulary - don't assume cognates.\n")
		sb.WriteString("- Translate tech terms: server = server (accepted), kanaal (channel), bericht (message), melding (notification), instellingen (settings).\n")
		sb.WriteString("- Use informal \"je/jij\" for chat apps, not formal \"u\".\n")
		sb.WriteString("- Handle compound words correctly (typically written as single words).\n")
		sb.WriteString("- Use proper Dutch spelling including IJ as a single letter where appropriate.\n")

	case "no":
		sb.WriteString("- Use Norwegian Bokmål conventions.\n")
		sb.WriteString("- Use Norwegian letters: æ, ø, å. Never substitute with ae, oe, aa.\n")
		sb.WriteString("- Translate tech terms: server = server/tjener, kanal (channel), melding (message), varsling (notification), innstillinger (settings).\n")
		sb.WriteString("- Norwegian typically accepts English tech terms but translate where natural Norwegian exists.\n")
		sb.WriteString("- Use informal \"du\" for chat apps.\n")

	case "pl":
		sb.WriteString("- Use Polish diacritics: ą, ć, ę, ł, ń, ó, ś, ź, ż. Never substitute.\n")
		sb.WriteString("- Handle complex Polish grammar: 7 cases, grammatical gender, verb aspects.\n")
		sb.WriteString("- Translate tech terms: serwer (server), kanał (channel), wiadomość (message), powiadomienie (notification), ustawienia (settings).\n")
		sb.WriteString("- Polish typically prefers native translations over anglicisms.\n")
		sb.WriteString("- Use informal \"ty\" for chat apps.\n")

	case "pt-BR":
		sb.WriteString("- Use Brazilian Portuguese conventions and spelling.\n")
		sb.WriteString("- Use proper accents: á, à, â, ã, é, ê, í, ó, ô, õ, ú, ç. Never omit them.\n")
		sb.WriteString("- Translate tech terms: servidor (server), canal (channel), mensagem (message), notificação (notification), configurações (settings).\n")
		sb.WriteString("- Use \"você\" for informal address (standard in Brazil), not \"tu\" or formal forms.\n")
		sb.WriteString("- Brazilian Portuguese differs from European Portuguese in vocabulary and phrasing.\n")

	case "ro":
		sb.WriteString("- Use Romanian diacritics: ă, â, î, ș, ț. Never substitute with a, s, t.\n")
		sb.WriteString("- Note: use ș and ț (with comma below), not ş and ţ (with cedilla).\n")
		sb.WriteString("- Translate tech terms: server = server (accepted), canal (channel), mesaj (message), notificare (notification), setări (settings).\n")
		sb.WriteString("- Romanian often accepts tech anglicisms but translate where natural Romanian exists.\n")
		sb.WriteString("- Use informal \"tu\" for chat apps.\n")

	case "ru":
		sb.WriteString("- Use Russian Cyrillic script throughout.\n")
		sb.WriteString("- Translate tech terms: сервер (server), канал (channel), сообщение (message), уведомление (notification), настройки (settings).\n")
		sb.WriteString("- Handle grammatical cases and gender correctly.\n")
		sb.WriteString("- Use informal \"ты\" for chat apps, not formal \"Вы\".\n")
		sb.WriteString("- Russian tech vocabulary is well-established - use standard Russian translations.\n")
		sb.WriteString("- Use proper Russian punctuation: guillemets «» for quotes where appropriate, but straight quotes in JSON.\n")

	case "sv-SE":
		sb.WriteString("- Use Swedish letters: å, ä, ö. Never substitute with a, o.\n")
		sb.WriteString("- Translate tech terms: server = server (accepted), kanal (channel), meddelande (message), avisering/notis (notification), inställningar (settings).\n")
		sb.WriteString("- Swedish accepts many English tech terms but translate where natural Swedish exists.\n")
		sb.WriteString("- Use informal \"du\" for chat apps.\n")
		sb.WriteString("- Swedish compound words are typically written as single words.\n")

	case "th":
		sb.WriteString("- Use Thai script throughout. Do not transliterate to Latin.\n")
		sb.WriteString("- Thai doesn't use spaces between words within sentences.\n")
		sb.WriteString("- Thai tech vocabulary often uses transliterated English: เซิร์ฟเวอร์ (server), แชนแนล (channel).\n")
		sb.WriteString("- Native terms where appropriate: ข้อความ (message), การแจ้งเตือน (notification), การตั้งค่า (settings).\n")
		sb.WriteString("- Use appropriate politeness particles (ครับ/ค่ะ) only where contextually appropriate - UI text often omits them.\n")
		sb.WriteString("- Keep UI text concise.\n")

	case "tr":
		sb.WriteString("- Use Turkish letters: ç, ğ, ı, İ, ö, ş, ü. Never substitute.\n")
		sb.WriteString("- Note the dotted İ and dotless ı distinction - this is important.\n")
		sb.WriteString("- Handle vowel harmony and agglutination correctly.\n")
		sb.WriteString("- Translate tech terms: sunucu (server), kanal (channel), mesaj (message), bildirim (notification), ayarlar (settings).\n")
		sb.WriteString("- Turkish typically prefers native translations over anglicisms.\n")
		sb.WriteString("- Use informal \"sen\" for chat apps.\n")

	case "uk":
		sb.WriteString("- Use Ukrainian Cyrillic script. This is NOT Russian.\n")
		sb.WriteString("- Use Ukrainian-specific letters: і, ї, є, ґ (different from Russian).\n")
		sb.WriteString("- Translate tech terms: сервер (server), канал (channel), повідомлення (message), сповіщення (notification), налаштування (settings).\n")
		sb.WriteString("- Handle grammatical cases and gender correctly.\n")
		sb.WriteString("- Use informal \"ти\" for chat apps, not formal \"Ви\".\n")
		sb.WriteString("- Ukrainian tech vocabulary is well-established - use standard Ukrainian translations.\n")

	case "vi":
		sb.WriteString("- Use Vietnamese with proper diacritics/tone marks. All 6 tones must be marked correctly.\n")
		sb.WriteString("- Diacritics are essential for meaning - never omit them.\n")
		sb.WriteString("- Vietnamese tech vocabulary often uses translated terms: máy chủ (server), kênh (channel), tin nhắn (message), thông báo (notification), cài đặt (settings).\n")
		sb.WriteString("- Use appropriate pronouns - chat apps typically use \"bạn\" (you).\n")
		sb.WriteString("- Keep translations natural and conversational for chat context.\n")

	case "zh-CN":
		sb.WriteString("- Use Simplified Chinese characters (简体中文).\n")
		sb.WriteString("- Do NOT use Traditional Chinese characters.\n")
		sb.WriteString("- Translate tech terms: 服务器 (server), 频道 (channel), 消息 (message), 通知 (notification), 设置 (settings).\n")
		sb.WriteString("- Chinese tech vocabulary is well-established - use standard Mainland China terminology.\n")
		sb.WriteString("- Keep UI text concise - Chinese is naturally compact.\n")
		sb.WriteString("- No spaces needed between Chinese characters.\n")
		sb.WriteString("- Use Chinese punctuation marks where appropriate in natural text, but ASCII in JSON structure.\n")

	case "zh-TW":
		sb.WriteString("- Use Traditional Chinese characters (繁體中文).\n")
		sb.WriteString("- Do NOT use Simplified Chinese characters.\n")
		sb.WriteString("- Translate tech terms: 伺服器 (server), 頻道 (channel), 訊息 (message), 通知 (notification), 設定 (settings).\n")
		sb.WriteString("- Use Taiwan-standard terminology which may differ from Hong Kong or Mainland usage.\n")
		sb.WriteString("- Keep UI text concise.\n")
		sb.WriteString("- No spaces needed between Chinese characters.\n")

	default:
		sb.WriteString("- Translate all UI terms into natural " + localeName + ".\n")
		sb.WriteString("- Only keep English terms that are established anglicisms in your language's tech context.\n")
		sb.WriteString("- When in doubt, translate.\n")
	}

	sb.WriteString("\n")
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func languageName(locale string) string {
	if name, ok := languageMap[locale]; ok {
		return name
	}
	return locale
}

func proxyForRequest(req *http.Request) (*url.URL, error) {
	host := req.URL.Hostname()
	if host == "localhost" {
		return nil, nil
	}
	ip := net.ParseIP(host)
	if ip != nil && ip.IsLoopback() {
		return nil, nil
	}
	return http.ProxyFromEnvironment(req)
}

func proxyLabelForEndpoint(endpoint string) (string, error) {
	parsed, err := url.Parse(endpoint)
	if err != nil {
		return "", err
	}
	req := &http.Request{URL: parsed}
	proxyURL, err := proxyForRequest(req)
	if err != nil {
		return "", err
	}
	if proxyURL == nil {
		return "none", nil
	}
	return proxyURL.String(), nil
}

func stripArgSeparators(args []string) []string {
	if len(args) == 0 {
		return args
	}
	cleaned := make([]string, 0, len(args))
	for _, arg := range args {
		if arg == "--" {
			continue
		}
		cleaned = append(cleaned, arg)
	}
	return cleaned
}

func newRequestTracker() *requestTracker {
	return &requestTracker{
		inFlight: make(map[string]requestState),
	}
}

func (rt *requestTracker) start(meta requestMeta) string {
	id := fmt.Sprintf("%s|%s|%d|%d|%d|%d", meta.locale, meta.phase, meta.chunkNumber, meta.totalChunks, meta.pass, time.Now().UnixNano())
	rt.mu.Lock()
	rt.inFlight[id] = requestState{meta: meta, start: time.Now()}
	rt.mu.Unlock()
	return id
}

func (rt *requestTracker) finish(id string) {
	rt.mu.Lock()
	delete(rt.inFlight, id)
	rt.mu.Unlock()
}

func (rt *requestTracker) oldest() (requestState, time.Duration, int, bool) {
	rt.mu.Lock()
	defer rt.mu.Unlock()
	if len(rt.inFlight) == 0 {
		return requestState{}, 0, 0, false
	}
	var oldest requestState
	var oldestAge time.Duration
	first := true
	now := time.Now()
	for _, state := range rt.inFlight {
		age := now.Sub(state.start)
		if first || age > oldestAge {
			oldest = state
			oldestAge = age
			first = false
		}
	}
	return oldest, oldestAge, len(rt.inFlight), true
}

func logRequestStart(meta requestMeta, attempt int) {
	log.Printf("[%s] request start %s", time.Now().Format(time.RFC3339), formatRequestMeta(meta, attempt))
}

func logRequestEnd(meta requestMeta, attempt int, duration time.Duration, status int, size int, err error) {
	if err != nil {
		log.Printf("[%s] request error %s in %s status=%d bytes=%d err=%v", time.Now().Format(time.RFC3339), formatRequestMeta(meta, attempt), duration.Round(time.Millisecond), status, size, err)
		return
	}
	log.Printf("[%s] request done %s in %s status=%d bytes=%d", time.Now().Format(time.RFC3339), formatRequestMeta(meta, attempt), duration.Round(time.Millisecond), status, size)
}

func logSlowRequest(state requestState, age time.Duration, inFlight int) {
	meta := state.meta
	log.Printf("[%s] slow request %s age=%s in_flight=%d", time.Now().Format(time.RFC3339), formatRequestMeta(meta, 0), age.Round(time.Second), inFlight)
}

func formatRequestMeta(meta requestMeta, attempt int) string {
	if attempt > 0 {
		return fmt.Sprintf("locale=%s phase=%s chunk=%d/%d pass=%d attempt=%d prompt=%d", meta.locale, meta.phase, meta.chunkNumber, meta.totalChunks, meta.pass, attempt, meta.promptChars)
	}
	return fmt.Sprintf("locale=%s phase=%s chunk=%d/%d pass=%d prompt=%d", meta.locale, meta.phase, meta.chunkNumber, meta.totalChunks, meta.pass, meta.promptChars)
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

func extractPayload(content string) ([]byte, error) {
	content = strings.ReplaceAll(content, "\r\n", "\n")
	content = strings.TrimSpace(content)

	if bytes, ok := validJSON(content); ok {
		return bytes, nil
	}

	if strings.HasPrefix(content, "```") {
		after := content
		if newline := strings.Index(after, "\n"); newline != -1 {
			after = after[newline+1:]
		} else {
			after = strings.TrimPrefix(after, "```json")
			after = strings.TrimPrefix(after, "```")
		}
		after = strings.TrimSpace(after)
		after = strings.TrimSuffix(after, "```")
		if bytes, ok := validJSON(after); ok {
			return bytes, nil
		}
		content = after 
	}

	if block := betweenFences(content, codeBlockStart, codeBlockEnd); block != "" {
		if bytes, ok := validJSON(block); ok {
			return bytes, nil
		}
	}
	if block := betweenFences(content, "```", "```"); block != "" {
		if bytes, ok := validJSON(block); ok {
			return bytes, nil
		}
	}

	if obj := firstJSONObject(content); obj != "" {
		if bytes, ok := validJSON(obj); ok {
			return bytes, nil
		}
	}

	return nil, fmt.Errorf("could not extract valid JSON from response (length: %d chars, last_json_error: %v)", len(content), lastJSONError)
}

func extractPartialTranslations(content string) translationPayload {
	var result translationPayload

	msgidPattern := `"msgid"\s*:\s*"((?:[^"\\]|\\.)*)"`
	msgstrPattern := `"msgstr"\s*:\s*"((?:[^"\\]|\\.)*)"`

	msgidRe := regexp.MustCompile(msgidPattern)
	msgstrRe := regexp.MustCompile(msgstrPattern)

	objectPattern := `\{\s*"msgid"\s*:\s*"(?:[^"\\]|\\.)*"\s*,\s*"msgstr"\s*:\s*"(?:[^"\\]|\\.)*"\s*\}`
	objectRe := regexp.MustCompile(objectPattern)

	matches := objectRe.FindAllString(content, -1)
	for _, match := range matches {
		msgidMatch := msgidRe.FindStringSubmatch(match)
		msgstrMatch := msgstrRe.FindStringSubmatch(match)
		if len(msgidMatch) >= 2 && len(msgstrMatch) >= 2 {
			msgid := unescapeJSON(msgidMatch[1])
			msgstr := unescapeJSON(msgstrMatch[1])
			if msgid != "" {
				result.Translations = append(result.Translations, struct {
					MsgID  string `json:"msgid"`
					MsgStr string `json:"msgstr"`
				}{MsgID: msgid, MsgStr: msgstr})
			}
		}
	}

	return result
}

func unescapeJSON(s string) string {
	s = strings.ReplaceAll(s, `\\`, "\x00")
	s = strings.ReplaceAll(s, `\"`, `"`)
	s = strings.ReplaceAll(s, `\n`, "\n")
	s = strings.ReplaceAll(s, `\r`, "\r")
	s = strings.ReplaceAll(s, `\t`, "\t")
	s = strings.ReplaceAll(s, "\x00", `\`)
	return s
}

var lastJSONError error

func validJSON(s string) ([]byte, bool) {
	if s == "" {
		return nil, false
	}
	var js json.RawMessage
	if err := json.Unmarshal([]byte(s), &js); err != nil {
		lastJSONError = err
		return nil, false
	}
	return []byte(s), true
}

func betweenFences(s, startFence, endFence string) string {
	start := strings.Index(s, startFence)
	if start == -1 {
		return ""
	}
	searchFrom := start + len(startFence)
	end := strings.LastIndex(s[searchFrom:], endFence)
	if end == -1 {
		return ""
	}
	payload := strings.TrimSpace(s[searchFrom : searchFrom+end])
	return payload
}

func firstJSONObject(s string) string {
	inString := false
	escape := false
	depth := 0
	start := -1

	for i := 0; i < len(s); i++ {
		ch := s[i]
		if escape {
			escape = false
			continue
		}
		if ch == '\\' {
			escape = true
			continue
		}
		if ch == '"' {
			inString = !inString
			continue
		}
		if inString {
			continue
		}
		if ch == '{' {
			if depth == 0 {
				start = i
			}
			depth++
			continue
		}
		if ch == '}' {
			if depth > 0 {
				depth--
				if depth == 0 && start != -1 {
					return strings.TrimSpace(s[start : i+1])
				}
			}
		}
	}
	return ""
}

func ensureCompleteTranslations(locale string, referenceIDs map[string]struct{}, translations map[string]string) error {
	if len(translations) == 0 {
		return fmt.Errorf("%s: no translations found", locale)
	}
	var missing []string
	for id := range referenceIDs {
		if _, ok := translations[id]; !ok {
			missing = append(missing, id)
		}
	}
	if len(missing) > 0 {
		log.Printf("[%s] %s: warning: missing %d translations (will write partial file)", time.Now().Format(time.RFC3339), locale, len(missing))
		for i, m := range missing {
			if i >= 5 {
				log.Printf("[%s] %s:   ... and %d more", time.Now().Format(time.RFC3339), locale, len(missing)-5)
				break
			}
			log.Printf("[%s] %s:   - %s", time.Now().Format(time.RFC3339), locale, truncateForError(m))
		}
	}
	return nil
}

func applyTranslations(po POFile, translations map[string]string) POFile {
	for idx := range po.Entries {
		if translated, ok := translations[po.Entries[idx].MsgID]; ok {
			po.Entries[idx].MsgStr = translated
		}
	}
	return po
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
