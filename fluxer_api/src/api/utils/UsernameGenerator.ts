// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {readFileSync} from 'node:fs';
import {resolveAssetPath} from '@app/api/utils/AssetPaths';
import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';

const scales = readFileSync(resolveAssetPath('words', 'scales.txt'), 'utf-8').trim().split('\n').filter(Boolean);
const tails = readFileSync(resolveAssetPath('words', 'tails.txt'), 'utf-8').trim().split('\n').filter(Boolean);

type IndexPicker = (size: number) => number;

function capitalize(word: string): string {
	return word.charAt(0).toUpperCase() + word.slice(1);
}

function generateUsername(pick: IndexPicker): string {
	const MAX_LENGTH = 32;
	const MAX_ATTEMPTS = 100;
	for (let i = 0; i < MAX_ATTEMPTS; i++) {
		const username = capitalize(scales[pick(scales.length)]) + capitalize(tails[pick(tails.length)]);
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

function seededPicker(seed: Uint8Array): IndexPicker {
	const view = new DataView(seed.buffer, seed.byteOffset, seed.byteLength);
	const words = Math.floor(seed.byteLength / 4);
	let next = 0;
	return (size) => {
		const value = view.getUint32((next % words) * 4);
		next++;
		return value % size;
	};
}

export function generateRandomUsername(): string {
	return generateUsername((size) => randomInt(size));
}

export function generateSeededUsername(seed: Uint8Array): string {
	if (seed.byteLength < 4) {
		throw new Error('Username seed must hold at least four bytes');
	}
	return generateUsername(seededPicker(seed));
}
