// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {readFileSync} from 'node:fs';
import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {resolveAssetPath} from './AssetPaths';

const scales = readFileSync(resolveAssetPath('words', 'scales.txt'), 'utf-8').trim().split('\n').filter(Boolean);
const tails = readFileSync(resolveAssetPath('words', 'tails.txt'), 'utf-8').trim().split('\n').filter(Boolean);

function capitalize(word: string): string {
	return word.charAt(0).toUpperCase() + word.slice(1);
}

function pickRandom(words: Array<string>): string {
	return words[randomInt(words.length)];
}

export function generateRandomUsername(): string {
	const MAX_LENGTH = 32;
	const MAX_ATTEMPTS = 100;
	for (let i = 0; i < MAX_ATTEMPTS; i++) {
		const username = capitalize(pickRandom(scales)) + capitalize(pickRandom(tails));
		if (username.length <= MAX_LENGTH && UsernameType.safeParse(username).success) {
			return username;
		}
	}
	for (const tail of tails) {
		const candidate = capitalize(tail);
		if (candidate.length <= MAX_LENGTH && UsernameType.safeParse(candidate).success) {
			return candidate;
		}
	}
	return 'BotUser';
}
