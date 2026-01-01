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

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

let tails: Array<string> | undefined;
let scales: Array<string> | undefined;

function getTails(): Array<string> {
	if (!tails) {
		initWords();
	}
	return tails!;
}

function getScales(): Array<string> {
	if (!scales) {
		initWords();
	}
	return scales!;
}

export function generateConnectionId(): string {
	const scaleWords = getScales();
	const tailWords = getTails();

	const scale = scaleWords[Math.floor(Math.random() * scaleWords.length)];
	const tail = tailWords[Math.floor(Math.random() * tailWords.length)];

	return `${tail}-${scale}`;
}

function initWords(): void {
	const wordsDir = path.join(__dirname);
	tails = parseWordsFile(path.join(wordsDir, 'tails.txt'));
	scales = parseWordsFile(path.join(wordsDir, 'scales.txt'));
}

function parseWordsFile(filePath: string): Array<string> {
	const content = fs.readFileSync(filePath, 'utf-8');
	const lines = content.split('\n');
	const words: Array<string> = [];

	for (const line of lines) {
		const trimmed = line.trim();
		if (trimmed && !trimmed.startsWith('#')) {
			words.push(trimmed);
		}
	}

	return words;
}
