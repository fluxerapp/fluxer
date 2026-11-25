// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';

const [, , outFile, browserRoot] = process.argv;
const require = createRequire(browserRoot ?? import.meta.url);
const {chromium} = require('playwright');

const tokens = readFileSync(new URL('../src/styles/tokens.css', import.meta.url), 'utf8');
const code = readFileSync(new URL('../src/styles/CodeTheme.ts', import.meta.url), 'utf8');

const textPairs = [
	['--flx-text', '--flx-bg'],
	['--flx-text', '--flx-card'],
	['--flx-text', '--flx-step'],
	['--flx-text', '--flx-code-bg'],
	['--flx-text-strong', '--flx-bg'],
	['--flx-text-strong', '--flx-card'],
	['--flx-text-strong', '--flx-step'],
	['--flx-text-muted', '--flx-bg'],
	['--flx-text-muted', '--flx-card'],
	['--flx-text-muted', '--flx-step'],
	['--flx-text-muted', '--flx-code-bg'],
	['--flx-link', '--flx-bg'],
	['--flx-link', '--flx-card'],
	['--flx-link', '--flx-step'],
	['--flx-link', '--flx-tint'],
];
const rulePairs = [
	['--flx-caution', '--flx-bg'],
	['--flx-danger', '--flx-bg'],
	['--flx-link', '--flx-bg'],
	['--flx-line-strong', '--flx-bg'],
	['--flx-mark', '--flx-bg'],
];

function palette(name) {
	const block = code.slice(code.indexOf(`const ${name} = {`));
	const body = block.slice(0, block.indexOf('};'));
	return Object.fromEntries([...body.matchAll(/(\w+): '(#[0-9a-f]{6})'/gu)].map((m) => [m[1], m[2]]));
}

const browser = await chromium.launch({headless: true});
const page = await browser.newPage();
await page.setContent(`<style>${tokens}</style><div id="probe"></div>`);
const lines = [];
let misses = 0;
for (const theme of ['dark', 'light']) {
	const rows = await page.evaluate(
		({theme, textPairs, rulePairs}) => {
			document.documentElement.dataset.theme = theme;
			const probe = document.getElementById('probe');
			const canvas = document.createElement('canvas');
			canvas.width = canvas.height = 1;
			const ctx = canvas.getContext('2d', {willReadFrequently: true});
			const rgb = (token, under) => {
				probe.style.color = `var(${token})`;
				const value = getComputedStyle(probe).color;
				ctx.clearRect(0, 0, 1, 1);
				if (under) {
					ctx.fillStyle = under;
					ctx.fillRect(0, 0, 1, 1);
				}
				ctx.fillStyle = value;
				ctx.fillRect(0, 0, 1, 1);
				const [r, g, b] = ctx.getImageData(0, 0, 1, 1).data;
				return `rgb(${r}, ${g}, ${b})`;
			};
			const lum = (c) => {
				const [r, g, b] = c
					.match(/\d+/g)
					.map(Number)
					.map((v) => {
						const s = v / 255;
						return s <= 0.03928 ? s / 12.92 : ((s + 0.055) / 1.055) ** 2.4;
					});
				return 0.2126 * r + 0.7152 * g + 0.0722 * b;
			};
			const ratio = (a, b) => {
				const [x, y] = [lum(a), lum(b)].sort((m, n) => n - m);
				return (x + 0.05) / (y + 0.05);
			};
			const bg = rgb('--flx-bg');
			const out = [];
			for (const [kind, pairs, min] of [
				['text', textPairs, 4.5],
				['rule', rulePairs, 3],
			]) {
				for (const [fg, back] of pairs) {
					const base = rgb(back, bg);
					out.push({kind, fg, back, min, value: ratio(rgb(fg, base), base)});
				}
			}
			return out;
		},
		{theme, textPairs, rulePairs},
	);
	for (const row of rows) {
		const ok = row.value >= row.min;
		misses += ok ? 0 : 1;
		lines.push(`${theme}\t${row.kind}\t${row.fg} on ${row.back}\t${row.value.toFixed(2)}\t${ok ? 'pass' : 'FAIL'}`);
	}
	const codeBg = theme === 'dark' ? '#16151a' : '#f6f7f9';
	const colours = palette(theme === 'dark' ? 'darkPalette' : 'lightPalette');
	const codeRows = await page.evaluate(
		({codeBg, colours}) => {
			const lum = (hex) => {
				const v = [1, 3, 5].map((i) => Number.parseInt(hex.slice(i, i + 2), 16) / 255);
				const [r, g, b] = v.map((s) => (s <= 0.03928 ? s / 12.92 : ((s + 0.055) / 1.055) ** 2.4));
				return 0.2126 * r + 0.7152 * g + 0.0722 * b;
			};
			return Object.entries(colours).map(([name, hex]) => {
				const [x, y] = [lum(hex), lum(codeBg)].sort((m, n) => n - m);
				return {name, value: (x + 0.05) / (y + 0.05)};
			});
		},
		{codeBg, colours},
	);
	for (const row of codeRows) {
		const ok = row.value >= 4.5;
		misses += ok ? 0 : 1;
		lines.push(`${theme}\tcode\t${row.name} on code background\t${row.value.toFixed(2)}\t${ok ? 'pass' : 'FAIL'}`);
	}
}
await browser.close();
const report = `${lines.join('\n')}\n${misses === 0 ? 'every pair passes' : `${misses} pairs fail`}\n`;
if (outFile) {
	writeFileSync(outFile, report);
}
process.stdout.write(report);
process.exitCode = misses === 0 ? 0 : 1;
