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
	"bytes"
	"encoding/json"
	"fmt"
	"image"
	"image/draw"
	"image/png"
	"io"
	"math"
	"math/rand"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/srwiley/oksvg"
	"github.com/srwiley/rasterx"
)

type EmojiSpritesConfig struct {
	NonDiversityPerRow int
	DiversityPerRow    int
	PickerPerRow       int
	PickerCount        int
}

var EMOJI_SPRITES = EmojiSpritesConfig{
	NonDiversityPerRow: 42,
	DiversityPerRow:    10,
	PickerPerRow:       11,
	PickerCount:        50,
}

const (
	EMOJI_SIZE  = 32
	TWEMOJI_CDN = "https://fluxerstatic.com/emoji"
)

var SPRITE_SCALES = []int{1, 2}

type EmojiObject struct {
	Surrogates string `json:"surrogates"`
	Skins      []struct {
		Surrogates string `json:"surrogates"`
	} `json:"skins,omitempty"`
}

type EmojiEntry struct {
	Surrogates string
}

type httpResp struct {
	Status int
	Body   string
}

func main() {
	rand.Seed(time.Now().UnixNano())

	cwd, _ := os.Getwd()
	appDir := filepath.Join(cwd, "..")

	outputDir := filepath.Join(appDir, "src", "assets", "emoji-sprites")
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		fmt.Fprintln(os.Stderr, "Failed to ensure output dir:", err)
		os.Exit(1)
	}

	emojiData, err := loadEmojiData(filepath.Join(appDir, "src", "data", "emojis.json"))
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error loading emoji data:", err)
		os.Exit(1)
	}

	client := &http.Client{
		Timeout: 30 * time.Second,
	}

	svgCache := newSVGCache()

	if err := generateMainSpriteSheet(client, svgCache, emojiData, outputDir); err != nil {
		fmt.Fprintln(os.Stderr, "Error generating main sprite sheet:", err)
		os.Exit(1)
	}
	if err := generateDiversitySpriteSheets(client, svgCache, emojiData, outputDir); err != nil {
		fmt.Fprintln(os.Stderr, "Error generating diversity sprite sheets:", err)
		os.Exit(1)
	}
	if err := generatePickerSpriteSheet(client, svgCache, outputDir); err != nil {
		fmt.Fprintln(os.Stderr, "Error generating picker sprite sheet:", err)
		os.Exit(1)
	}

	fmt.Println("Emoji sprites generated successfully.")
}

func loadEmojiData(path string) (map[string][]EmojiObject, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var data map[string][]EmojiObject
	if err := json.Unmarshal(b, &data); err != nil {
		return nil, err
	}
	return data, nil
}

// --- SVG fetching + caching ---

type svgCache struct {
	m map[string]*string
}

func newSVGCache() *svgCache {
	return &svgCache{m: make(map[string]*string)}
}

func (c *svgCache) get(codepoint string) (*string, bool) {
	v, ok := c.m[codepoint]
	return v, ok
}

func (c *svgCache) set(codepoint string, v *string) {
	c.m[codepoint] = v
}

func downloadSVG(client *http.Client, url string) (httpResp, error) {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return httpResp{}, err
	}
	req.Header.Set("User-Agent", "fluxer-emoji-sprites/1.0")

	resp, err := client.Do(req)
	if err != nil {
		return httpResp{}, err
	}
	defer resp.Body.Close()

	bodyBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return httpResp{}, err
	}

	return httpResp{
		Status: resp.StatusCode,
		Body:   string(bodyBytes),
	}, nil
}

func fetchTwemojiSVG(client *http.Client, cache *svgCache, codepoint string) *string {
	if v, ok := cache.get(codepoint); ok {
		return v
	}

	url := fmt.Sprintf("%s/%s.svg", TWEMOJI_CDN, codepoint)
	r, err := downloadSVG(client, url)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to fetch Twemoji %s: %v\n", codepoint, err)
		cache.set(codepoint, nil)
		return nil
	}

	if r.Status != 200 {
		fmt.Fprintf(os.Stderr, "Twemoji %s returned %d\n", codepoint, r.Status)
		cache.set(codepoint, nil)
		return nil
	}

	body := r.Body
	cache.set(codepoint, &body)
	return &body
}

// --- Emoji -> codepoint ---

func emojiToCodepoint(s string) string {
	parts := make([]string, 0, len(s))
	for _, r := range s {
		if r == 0xFE0F { 
			continue
		}
		parts = append(parts, strings.ToLower(strconv.FormatInt(int64(r), 16)))
	}
	return strings.Join(parts, "-")
}

// --- Rendering ---

var svgOpenTagRe = regexp.MustCompile(`(?i)<svg([^>]*)>`)

func fixSVGSize(svg string, size int) string {
	return svgOpenTagRe.ReplaceAllString(svg, fmt.Sprintf(`<svg$1 width="%d" height="%d">`, size, size))
}

func renderSVGToImage(svgContent string, size int) (*image.RGBA, error) {
	fixed := fixSVGSize(svgContent, size)

	icon, err := oksvg.ReadIconStream(strings.NewReader(fixed))
	if err != nil {
		return nil, err
	}
	icon.SetTarget(0, 0, float64(size), float64(size))

	dst := image.NewRGBA(image.Rect(0, 0, size, size))
	scanner := rasterx.NewScannerGV(size, size, dst, dst.Bounds())
	r := rasterx.NewDasher(size, size, scanner)
	icon.Draw(r, 1.0)
	return dst, nil
}

func createPlaceholder(size int) *image.RGBA {
	img := image.NewRGBA(image.Rect(0, 0, size, size))

	h := rand.Float64() * 360.0
	r, g, b := hslToRGB(h, 0.70, 0.60)

	cx := float64(size) / 2.0
	cy := float64(size) / 2.0
	radius := float64(size) * 0.4
	r2 := radius * radius

	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			dx := (float64(x) + 0.5) - cx
			dy := (float64(y) + 0.5) - cy
			if dx*dx+dy*dy <= r2 {
				i := img.PixOffset(x, y)
				img.Pix[i+0] = r
				img.Pix[i+1] = g
				img.Pix[i+2] = b
				img.Pix[i+3] = 0xFF
			}
		}
	}
	return img
}

func hslToRGB(h, s, l float64) (uint8, uint8, uint8) {
	h = math.Mod(h, 360.0) / 360.0

	var r, g, b float64
	if s == 0 {
		r, g, b = l, l, l
	} else {
		var q float64
		if l < 0.5 {
			q = l * (1 + s)
		} else {
			q = l + s - l*s
		}
		p := 2*l - q
		r = hueToRGB(p, q, h+1.0/3.0)
		g = hueToRGB(p, q, h)
		b = hueToRGB(p, q, h-1.0/3.0)
	}

	return uint8(clamp01(r) * 255), uint8(clamp01(g) * 255), uint8(clamp01(b) * 255)
}

func hueToRGB(p, q, t float64) float64 {
	if t < 0 {
		t += 1
	}
	if t > 1 {
		t -= 1
	}
	if t < 1.0/6.0 {
		return p + (q-p)*6*t
	}
	if t < 1.0/2.0 {
		return q
	}
	if t < 2.0/3.0 {
		return p + (q-p)*(2.0/3.0-t)*6
	}
	return p
}

func clamp01(v float64) float64 {
	if v < 0 {
		return 0
	}
	if v > 1 {
		return 1
	}
	return v
}

func loadEmojiImage(client *http.Client, cache *svgCache, surrogate string, size int) *image.RGBA {
	codepoint := emojiToCodepoint(surrogate)

	if svg := fetchTwemojiSVG(client, cache, codepoint); svg != nil {
		if img, err := renderSVGToImage(*svg, size); err == nil {
			return img
		}
	}

	if strings.Contains(codepoint, "-200d-") {
		basePart := strings.Split(codepoint, "-200d-")[0]
		if svg := fetchTwemojiSVG(client, cache, basePart); svg != nil {
			if img, err := renderSVGToImage(*svg, size); err == nil {
				return img
			}
		}
	}

	fmt.Fprintf(os.Stderr, "Missing SVG for %s (%s), using placeholder\n", codepoint, surrogate)
	return createPlaceholder(size)
}

func renderSpriteSheet(client *http.Client, cache *svgCache, emojiEntries []EmojiEntry, perRow int, fileNameBase string, outputDir string) error {
	if perRow <= 0 {
		return fmt.Errorf("perRow must be > 0")
	}
	rows := int(math.Ceil(float64(len(emojiEntries)) / float64(perRow)))

	for _, scale := range SPRITE_SCALES {
		size := EMOJI_SIZE * scale
		dstW := perRow * size
		dstH := rows * size

		sheet := image.NewRGBA(image.Rect(0, 0, dstW, dstH))

		for i, item := range emojiEntries {
			emojiImg := loadEmojiImage(client, cache, item.Surrogates, size)
			row := i / perRow
			col := i % perRow
			x := col * size
			y := row * size

			r := image.Rect(x, y, x+size, y+size)
			draw.Draw(sheet, r, emojiImg, image.Point{}, draw.Over)
		}

		suffix := ""
		if scale != 1 {
			suffix = fmt.Sprintf("@%dx", scale)
		}
		outPath := filepath.Join(outputDir, fmt.Sprintf("%s%s.png", fileNameBase, suffix))
		if err := writePNG(outPath, sheet); err != nil {
			return err
		}
		fmt.Printf("Wrote %s\n", outPath)
	}

	return nil
}

func writePNG(path string, img image.Image) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	var buf bytes.Buffer
	if err := png.Encode(&buf, img); err != nil {
		return err
	}
	return os.WriteFile(path, buf.Bytes(), 0o644)
}

// --- Generators ---

func generateMainSpriteSheet(client *http.Client, cache *svgCache, emojiData map[string][]EmojiObject, outputDir string) error {
	base := make([]EmojiEntry, 0, 4096)
	for _, objs := range emojiData {
		for _, obj := range objs {
			base = append(base, EmojiEntry{Surrogates: obj.Surrogates})
		}
	}
	return renderSpriteSheet(client, cache, base, EMOJI_SPRITES.NonDiversityPerRow, "spritesheet-emoji", outputDir)
}

func generateDiversitySpriteSheets(client *http.Client, cache *svgCache, emojiData map[string][]EmojiObject, outputDir string) error {
	skinTones := []string{"🏻", "🏼", "🏽", "🏾", "🏿"}

	for skinIndex, skinTone := range skinTones {
		skinCodepoint := emojiToCodepoint(skinTone)

		skinEntries := make([]EmojiEntry, 0, 2048)
		for _, objs := range emojiData {
			for _, obj := range objs {
				if len(obj.Skins) > skinIndex && obj.Skins[skinIndex].Surrogates != "" {
					skinEntries = append(skinEntries, EmojiEntry{Surrogates: obj.Skins[skinIndex].Surrogates})
				}
			}
		}

		if len(skinEntries) == 0 {
			continue
		}
		if err := renderSpriteSheet(client, cache, skinEntries, EMOJI_SPRITES.DiversityPerRow, "spritesheet-"+skinCodepoint, outputDir); err != nil {
			return err
		}
	}

	return nil
}

func generatePickerSpriteSheet(client *http.Client, cache *svgCache, outputDir string) error {
	basicEmojis := []string{
		"😀", "😃", "😄", "😁", "😆", "😅", "😂", "🤣", "😊", "😇",
		"🙂", "😉", "😌", "😍", "🥰", "😘", "😗", "😙", "😚", "😋",
		"😛", "😝", "😜", "🤪", "🤨", "🧐", "🤓", "😎", "🥳", "😏",
	}

	entries := make([]EmojiEntry, 0, len(basicEmojis))
	for _, e := range basicEmojis {
		entries = append(entries, EmojiEntry{Surrogates: e})
	}

	return renderSpriteSheet(client, cache, entries, EMOJI_SPRITES.PickerPerRow, "spritesheet-picker", outputDir)
}
