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
	"flag"
	"fmt"
	"image"
	"image/png"
	"os"
	"path/filepath"

	"github.com/chai2010/webp"
	"github.com/disintegration/imaging"
	"github.com/gen2brain/avif"
)

type imageConfig struct {
	input string
	name  string
	sizes []int
}

func main() {
	android := flag.String("android", "", "Path to Android screenshot (required)")
	ios := flag.String("ios", "", "Path to iOS screenshot (required)")
	desktop := flag.String("desktop", "", "Path to Desktop screenshot (required)")
	outputDir := flag.String("output", "../fluxer_static/marketing/pwa-install", "Output directory")
	flag.Parse()

	if *android == "" || *ios == "" || *desktop == "" {
		fmt.Fprintln(os.Stderr, "Usage: preprocess-pwa-images -android <path> -ios <path> -desktop <path> [-output <dir>]")
		flag.PrintDefaults()
		os.Exit(1)
	}

	if err := os.MkdirAll(*outputDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create output directory: %v\n", err)
		os.Exit(1)
	}

	// For dialog thumbnails, we use smaller sizes
	// Android is portrait (360x780), so we use height-based sizing
	// iOS and Desktop are landscape, so we use width-based sizing
	images := []imageConfig{
		{input: *android, name: "android", sizes: []int{240, 320, 480}},
		{input: *ios, name: "ios", sizes: []int{320, 480, 640}},
		{input: *desktop, name: "desktop", sizes: []int{320, 480, 640, 960}},
	}

	fmt.Println("Starting PWA image preprocessing...")
	fmt.Printf("Output directory: %s\n\n", *outputDir)

	totalFiles := 0
	for _, img := range images {
		if _, err := os.Stat(img.input); os.IsNotExist(err) {
			fmt.Fprintf(os.Stderr, "Input file not found: %s\n", img.input)
			continue
		}

		fmt.Printf("Processing %s...\n", img.name)

		src, err := imaging.Open(img.input)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to open %s: %v\n", img.input, err)
			continue
		}

		bounds := src.Bounds()
		fmt.Printf("  Original size: %dx%d\n\n", bounds.Dx(), bounds.Dy())

		for _, width := range img.sizes {
			if err := processImage(src, img.name, width, *outputDir); err != nil {
				fmt.Fprintf(os.Stderr, "Error processing %s at %d: %v\n", img.name, width, err)
			} else {
				totalFiles += 3 // avif, webp, png
			}
		}
		fmt.Println()
	}

	fmt.Println("PWA image preprocessing complete!")
	fmt.Printf("Generated %d image files\n", totalFiles)
}

func processImage(src image.Image, name string, width int, outputDir string) error {
	fmt.Printf("Processing %s at %dpx width...\n", name, width)

	// Resize maintaining aspect ratio
	resized := imaging.Resize(src, width, 0, imaging.Lanczos)

	// AVIF
	if err := saveAVIF(resized, filepath.Join(outputDir, fmt.Sprintf("%s-%dw.avif", name, width))); err != nil {
		fmt.Printf("  Failed to generate avif: %v\n", err)
	} else {
		printFileSize(filepath.Join(outputDir, fmt.Sprintf("%s-%dw.avif", name, width)))
	}

	// WebP
	if err := saveWebP(resized, filepath.Join(outputDir, fmt.Sprintf("%s-%dw.webp", name, width))); err != nil {
		fmt.Printf("  Failed to generate webp: %v\n", err)
	} else {
		printFileSize(filepath.Join(outputDir, fmt.Sprintf("%s-%dw.webp", name, width)))
	}

	// PNG
	if err := savePNG(resized, filepath.Join(outputDir, fmt.Sprintf("%s-%dw.png", name, width))); err != nil {
		fmt.Printf("  Failed to generate png: %v\n", err)
	} else {
		printFileSize(filepath.Join(outputDir, fmt.Sprintf("%s-%dw.png", name, width)))
	}

	return nil
}

func saveAVIF(img image.Image, path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	return avif.Encode(f, img, avif.Options{Quality: 80, Speed: 6})
}

func saveWebP(img image.Image, path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	return webp.Encode(f, img, &webp.Options{Quality: 85})
}

func savePNG(img image.Image, path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	encoder := png.Encoder{CompressionLevel: png.BestCompression}
	return encoder.Encode(f, img)
}

func printFileSize(path string) {
	info, err := os.Stat(path)
	if err != nil {
		return
	}
	sizeKB := float64(info.Size()) / 1024
	fmt.Printf("  OK %s (%.2f KB)\n", filepath.Base(path), sizeKB)
}
