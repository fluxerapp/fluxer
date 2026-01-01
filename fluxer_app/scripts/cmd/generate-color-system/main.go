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
	"fmt"
	"math"
	"os"
	"path/filepath"
	"strings"

	"gopkg.in/yaml.v3"
)

type ColorFamily struct {
	Hue                 int  `yaml:"hue" json:"hue"`
	Saturation          int  `yaml:"saturation" json:"saturation"`
	UseSaturationFactor bool `yaml:"useSaturationFactor" json:"useSaturationFactor"`
}

type ScaleStop struct {
	Name     string   `yaml:"name"`
	Position *float64 `yaml:"position"`
}

type Scale struct {
	Family string      `yaml:"family"`
	Range  [2]float64  `yaml:"range"`
	Curve  string      `yaml:"curve"`
	Stops  []ScaleStop `yaml:"stops"`
}

type TokenDef struct {
	Name                string   `yaml:"name,omitempty"`
	Scale               string   `yaml:"scale,omitempty"`
	Value               string   `yaml:"value,omitempty"`
	Family              string   `yaml:"family,omitempty"`
	Hue                 *int     `yaml:"hue,omitempty"`
	Saturation          *int     `yaml:"saturation,omitempty"`
	Lightness           *float64 `yaml:"lightness,omitempty"`
	Alpha               *float64 `yaml:"alpha,omitempty"`
	UseSaturationFactor *bool    `yaml:"useSaturationFactor,omitempty"`
}

type Config struct {
	Families map[string]ColorFamily `yaml:"families"`
	Scales   map[string]Scale       `yaml:"scales"`
	Tokens   struct {
		Root  []TokenDef `yaml:"root"`
		Light []TokenDef `yaml:"light"`
		Coal  []TokenDef `yaml:"coal"`
	} `yaml:"tokens"`
}

type OutputToken struct {
	Type                string   `json:"type"`
	Name                string   `json:"name"`
	Family              string   `json:"family,omitempty"`
	Hue                 *int     `json:"hue,omitempty"`
	Saturation          *int     `json:"saturation,omitempty"`
	Lightness           *float64 `json:"lightness,omitempty"`
	Alpha               *float64 `json:"alpha,omitempty"`
	UseSaturationFactor *bool    `json:"useSaturationFactor,omitempty"`
	Value               string   `json:"value,omitempty"`
}

func clamp01(value float64) float64 {
	return math.Min(1, math.Max(0, value))
}

func applyCurve(curve string, t float64) float64 {
	switch curve {
	case "easeIn":
		return t * t
	case "easeOut":
		return 1 - (1-t)*(1-t)
	case "easeInOut":
		if t < 0.5 {
			return 2 * t * t
		}
		return 1 - 2*(1-t)*(1-t)
	default:
		return t
	}
}

func buildScaleTokens(scale Scale) []OutputToken {
	lastIndex := float64(max(len(scale.Stops)-1, 1))
	tokens := make([]OutputToken, 0, len(scale.Stops))

	for i, stop := range scale.Stops {
		pos := 0.0
		if stop.Position != nil {
			pos = clamp01(*stop.Position)
		} else {
			pos = float64(i) / lastIndex
		}

		eased := applyCurve(scale.Curve, pos)
		lightness := scale.Range[0] + (scale.Range[1]-scale.Range[0])*eased
		lightness = math.Round(lightness*1000) / 1000

		tokens = append(tokens, OutputToken{
			Type:      "tone",
			Name:      stop.Name,
			Family:    scale.Family,
			Lightness: &lightness,
		})
	}
	return tokens
}

func expandTokens(defs []TokenDef, scales map[string]Scale) []OutputToken {
	var tokens []OutputToken

	for _, def := range defs {
		if def.Scale != "" {
			scale, ok := scales[def.Scale]
			if !ok {
				fmt.Fprintf(os.Stderr, "Warning: unknown scale %q\n", def.Scale)
				continue
			}
			tokens = append(tokens, buildScaleTokens(scale)...)
			continue
		}

		if def.Value != "" {
			tokens = append(tokens, OutputToken{
				Type:  "literal",
				Name:  def.Name,
				Value: strings.TrimSpace(def.Value),
			})
		} else {
			tokens = append(tokens, OutputToken{
				Type:                "tone",
				Name:                def.Name,
				Family:              def.Family,
				Hue:                 def.Hue,
				Saturation:          def.Saturation,
				Lightness:           def.Lightness,
				Alpha:               def.Alpha,
				UseSaturationFactor: def.UseSaturationFactor,
			})
		}
	}

	return tokens
}

func formatNumber(value float64) string {
	if value == float64(int(value)) {
		return fmt.Sprintf("%d", int(value))
	}
	s := fmt.Sprintf("%.2f", value)
	s = strings.TrimRight(s, "0")
	s = strings.TrimRight(s, ".")
	return s
}

func formatTone(token OutputToken, families map[string]ColorFamily) string {
	var family *ColorFamily
	if token.Family != "" {
		if f, ok := families[token.Family]; ok {
			family = &f
		}
	}

	var hue, saturation int
	var lightness float64
	var useFactor bool

	if token.Hue != nil {
		hue = *token.Hue
	} else if family != nil {
		hue = family.Hue
	}

	if token.Saturation != nil {
		saturation = *token.Saturation
	} else if family != nil {
		saturation = family.Saturation
	}

	if token.Lightness != nil {
		lightness = *token.Lightness
	}

	if token.UseSaturationFactor != nil {
		useFactor = *token.UseSaturationFactor
	} else if family != nil {
		useFactor = family.UseSaturationFactor
	}

	var satStr string
	if useFactor {
		satStr = fmt.Sprintf("calc(%s%% * var(--saturation-factor))", formatNumber(float64(saturation)))
	} else {
		satStr = fmt.Sprintf("%s%%", formatNumber(float64(saturation)))
	}

	if token.Alpha == nil {
		return fmt.Sprintf("hsl(%s, %s, %s%%)", formatNumber(float64(hue)), satStr, formatNumber(lightness))
	}

	return fmt.Sprintf("hsla(%s, %s, %s%%, %s)", formatNumber(float64(hue)), satStr, formatNumber(lightness), formatNumber(*token.Alpha))
}

func formatValue(token OutputToken, families map[string]ColorFamily) string {
	if token.Type == "tone" {
		return formatTone(token, families)
	}
	return strings.TrimSpace(token.Value)
}

func renderBlock(selector string, tokens []OutputToken, families map[string]ColorFamily) string {
	var lines []string
	for _, token := range tokens {
		lines = append(lines, fmt.Sprintf("\t%s: %s;", token.Name, formatValue(token, families)))
	}
	return fmt.Sprintf("%s {\n%s\n}", selector, strings.Join(lines, "\n"))
}

func generateCSS(cfg *Config, rootTokens, lightTokens, coalTokens []OutputToken) string {
	header := `/*
 * This file is auto-generated by scripts/cmd/generate-color-system.
 * Do not edit directly — update color-system.yaml instead.
 */`

	blocks := []string{
		renderBlock(":root", rootTokens, cfg.Families),
		renderBlock(".theme-light", lightTokens, cfg.Families),
		renderBlock(".theme-coal", coalTokens, cfg.Families),
	}

	return header + "\n\n" + strings.Join(blocks, "\n\n") + "\n"
}

func main() {
	cwd, _ := os.Getwd()
	configPath := filepath.Join(cwd, "color-system.yaml")

	data, err := os.ReadFile(configPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading config: %v\n", err)
		os.Exit(1)
	}

	var cfg Config
	if err := yaml.Unmarshal(data, &cfg); err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing config: %v\n", err)
		os.Exit(1)
	}

	rootTokens := expandTokens(cfg.Tokens.Root, cfg.Scales)
	lightTokens := expandTokens(cfg.Tokens.Light, cfg.Scales)
	coalTokens := expandTokens(cfg.Tokens.Coal, cfg.Scales)

	parentDir := filepath.Join(cwd, "..")
	cssPath := filepath.Join(parentDir, "src", "styles", "generated", "color-system.css")

	if err := os.MkdirAll(filepath.Dir(cssPath), 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating CSS directory: %v\n", err)
		os.Exit(1)
	}

	css := generateCSS(&cfg, rootTokens, lightTokens, coalTokens)
	if err := os.WriteFile(cssPath, []byte(css), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing CSS file: %v\n", err)
		os.Exit(1)
	}
	relCSS, _ := filepath.Rel(parentDir, cssPath)
	fmt.Printf("Wrote %s\n", relCSS)
}
