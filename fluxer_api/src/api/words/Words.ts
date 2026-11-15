// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {scales, tails} from '@app/api/utils/UsernameGenerator';

const SUFFIX_ALPHABET = 'abcdefghijklmnopqrstuvwxyz0123456789';
const SUFFIX_LENGTH = 4;

function randomSuffix(): string {
	let suffix = '';
	for (let i = 0; i < SUFFIX_LENGTH; i++) {
		suffix += SUFFIX_ALPHABET[randomInt(SUFFIX_ALPHABET.length)];
	}
	return suffix;
}

export function generateConnectionId(): string {
	return `${tails[randomInt(tails.length)]}-${scales[randomInt(scales.length)]}-${randomSuffix()}`;
}
