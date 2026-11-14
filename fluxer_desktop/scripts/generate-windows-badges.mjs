// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import * as path from 'node:path';
import * as zlib from 'node:zlib';
import {app, BrowserWindow} from 'electron';

const ROOT_DIR = path.resolve(import.meta.dirname, '..');
const BUILD_RESOURCES_DIR = path.join(ROOT_DIR, 'build_resources');
const FONT_FILE = path.join(ROOT_DIR, '..', 'packages', 'fonts', 'files', 'FluxerSans', 'FluxerSans-SemiBold.woff2');
const ICON_DIRECTORIES = Object.freeze(['icons-stable', 'icons-canary', 'icons-development']);
const FRAME_SIZES = Object.freeze([16, 20, 24, 32, 40, 48]);
const SUPERSAMPLE = 8;
const BADGE_COUNT = 11;
const DOT_INDEX = 11;
const CIRCLE_DIAMETER_RATIO = 0.875;
const DIGIT_HEIGHT_RATIO = 0.49;
const WIDE_LABEL_MAX_WIDTH_RATIO = 0.7;
const DOT_DIAMETER_RATIO = 0.375;
const GLYPH_COLOR = '#ffffff';
const VARIANTS = Object.freeze([
	{prefix: 'badge', fill: '#f35a5a'},
	{prefix: 'badge-dark', fill: '#26252d'},
]);

function labelFor(index) {
	if (index === DOT_INDEX) return null;
	if (index === BADGE_COUNT - 1) return '9+';
	return String(index);
}

function renderInPage(options) {
	return document.fonts.load(`600 100px BadgeFont`).then(() => {
		const results = [];
		for (const job of options.jobs) {
			const size = job.size * options.supersample;
			const canvas = document.createElement('canvas');
			canvas.width = size;
			canvas.height = size;
			const context = canvas.getContext('2d', {willReadFrequently: true});
			context.fillStyle = job.fill;
			context.beginPath();
			context.arc(size / 2, size / 2, (size * options.circleDiameterRatio) / 2, 0, Math.PI * 2);
			context.fill();
			context.fillStyle = options.glyphColor;
			if (job.label === null) {
				context.beginPath();
				context.arc(size / 2, size / 2, (size * options.dotDiameterRatio) / 2, 0, Math.PI * 2);
				context.fill();
			} else {
				context.font = '600 100px BadgeFont';
				const reference = context.measureText('8');
				const referenceHeight = reference.actualBoundingBoxAscent + reference.actualBoundingBoxDescent;
				let fontSize = (100 * size * options.digitHeightRatio) / referenceHeight;
				context.font = `600 ${fontSize}px BadgeFont`;
				let metrics = context.measureText(job.label);
				let width = metrics.actualBoundingBoxLeft + metrics.actualBoundingBoxRight;
				const maxWidth = size * options.wideLabelMaxWidthRatio;
				if (width > maxWidth) {
					fontSize *= maxWidth / width;
					context.font = `600 ${fontSize}px BadgeFont`;
					metrics = context.measureText(job.label);
					width = metrics.actualBoundingBoxLeft + metrics.actualBoundingBoxRight;
				}
				const height = metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent;
				const x = (size - width) / 2 + metrics.actualBoundingBoxLeft;
				const y = (size - height) / 2 + metrics.actualBoundingBoxAscent;
				context.fillText(job.label, x, y);
			}
			results.push(Array.from(context.getImageData(0, 0, size, size).data));
		}
		return results;
	});
}

function downsample(rgba, size, factor) {
	const source = size * factor;
	const output = Buffer.alloc(size * size * 4);
	const samples = factor * factor;
	for (let y = 0; y < size; y++) {
		for (let x = 0; x < size; x++) {
			let red = 0;
			let green = 0;
			let blue = 0;
			let alpha = 0;
			for (let sy = 0; sy < factor; sy++) {
				for (let sx = 0; sx < factor; sx++) {
					const offset = ((y * factor + sy) * source + (x * factor + sx)) * 4;
					const weight = rgba[offset + 3];
					red += rgba[offset] * weight;
					green += rgba[offset + 1] * weight;
					blue += rgba[offset + 2] * weight;
					alpha += weight;
				}
			}
			const target = (y * size + x) * 4;
			if (alpha === 0) continue;
			output[target] = Math.round(red / alpha);
			output[target + 1] = Math.round(green / alpha);
			output[target + 2] = Math.round(blue / alpha);
			output[target + 3] = Math.round(alpha / samples);
		}
	}
	return output;
}

function pngChunk(type, data) {
	const chunk = Buffer.alloc(12 + data.length);
	chunk.writeUInt32BE(data.length, 0);
	chunk.write(type, 4, 'latin1');
	data.copy(chunk, 8);
	chunk.writeUInt32BE(zlib.crc32(chunk.subarray(4, 8 + data.length)), 8 + data.length);
	return chunk;
}

function encodePng(rgba, size) {
	const header = Buffer.alloc(13);
	header.writeUInt32BE(size, 0);
	header.writeUInt32BE(size, 4);
	header.writeUInt8(8, 8);
	header.writeUInt8(6, 9);
	const rows = Buffer.alloc(size * (size * 4 + 1));
	for (let y = 0; y < size; y++) {
		rgba.copy(rows, y * (size * 4 + 1) + 1, y * size * 4, (y + 1) * size * 4);
	}
	return Buffer.concat([
		Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]),
		pngChunk('IHDR', header),
		pngChunk('IDAT', zlib.deflateSync(rows, {level: 9})),
		pngChunk('IEND', Buffer.alloc(0)),
	]);
}

function encodeIcon(frames) {
	const header = Buffer.alloc(6 + frames.length * 16);
	header.writeUInt16LE(0, 0);
	header.writeUInt16LE(1, 2);
	header.writeUInt16LE(frames.length, 4);
	let offset = header.length;
	frames.forEach(({size, data}, index) => {
		const entry = 6 + index * 16;
		header.writeUInt8(size, entry);
		header.writeUInt8(size, entry + 1);
		header.writeUInt16LE(1, entry + 4);
		header.writeUInt16LE(32, entry + 6);
		header.writeUInt32LE(data.length, entry + 8);
		header.writeUInt32LE(offset, entry + 12);
		offset += data.length;
	});
	return Buffer.concat([header, ...frames.map(({data}) => data)]);
}

async function generate() {
	const font = fs.readFileSync(FONT_FILE).toString('base64');
	const window = new BrowserWindow({show: false, webPreferences: {offscreen: true}});
	const html = `<style>@font-face{font-family:BadgeFont;font-weight:600;src:url(data:font/woff2;base64,${font}) format('woff2')}</style>`;
	await window.loadURL(`data:text/html;charset=utf-8,${encodeURIComponent(html)}`);
	const jobs = VARIANTS.flatMap((variant) =>
		Array.from({length: BADGE_COUNT}, (_, offset) => offset + 1).flatMap((index) =>
			FRAME_SIZES.map((size) => ({variant: variant.prefix, index, size, fill: variant.fill, label: labelFor(index)})),
		),
	);
	const options = {
		jobs: jobs.map(({size, fill, label}) => ({size, fill, label})),
		supersample: SUPERSAMPLE,
		circleDiameterRatio: CIRCLE_DIAMETER_RATIO,
		digitHeightRatio: DIGIT_HEIGHT_RATIO,
		wideLabelMaxWidthRatio: WIDE_LABEL_MAX_WIDTH_RATIO,
		dotDiameterRatio: DOT_DIAMETER_RATIO,
		glyphColor: GLYPH_COLOR,
	};
	const rendered = await window.webContents.executeJavaScript(
		`(${renderInPage.toString()})(${JSON.stringify(options)})`,
	);
	window.destroy();
	const icons = new Map();
	jobs.forEach((job, position) => {
		const name = `${job.variant}-${job.index}.ico`;
		const frames = icons.get(name) ?? [];
		const pixels = downsample(rendered[position], job.size, SUPERSAMPLE);
		frames.push({size: job.size, data: encodePng(pixels, job.size)});
		icons.set(name, frames);
	});
	for (const directory of ICON_DIRECTORIES) {
		const target = path.join(BUILD_RESOURCES_DIR, directory, 'badges');
		fs.mkdirSync(target, {recursive: true});
		for (const [name, frames] of icons) {
			fs.writeFileSync(path.join(target, name), encodeIcon(frames));
		}
	}
	console.log(`Wrote ${icons.size} badge icons to ${ICON_DIRECTORIES.length} channel directories`);
}

app
	.whenReady()
	.then(generate)
	.then(() => app.exit(0))
	.catch((error) => {
		console.error(error);
		app.exit(1);
	});
