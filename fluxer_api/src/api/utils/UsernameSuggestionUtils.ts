// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {generateRandomUsername} from '@app/api/utils/UsernameGenerator';
import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {transliterate as tr} from 'transliteration';

const MAX_USERNAME_LENGTH = 32;
const RESERVED_TERMS = /fluxer|system_?message/giu;

export function normaliseUsernameCandidate(value: string): string {
	const trimmed = value.trim();
	if (!trimmed) return '';
	let sanitized = tr(trimmed);
	sanitized = sanitized.replace(/[\s\-.]+/g, '_');
	sanitized = sanitized.replace(/[^a-zA-Z0-9_]/g, '');
	if (sanitized.length > MAX_USERNAME_LENGTH) {
		sanitized = sanitized.substring(0, MAX_USERNAME_LENGTH);
	}
	return sanitized;
}

function tidyUnderscores(value: string): string {
	return value.replace(/_+/g, '_').replace(/^_+|_+$/g, '');
}

function displayNameCandidate(globalName: string): string {
	const transliterated = tidyUnderscores(normaliseUsernameCandidate(globalName.normalize('NFKC')));
	return tidyUnderscores(transliterated.replace(RESERVED_TERMS, '_'));
}

function isValidUsername(value: string): boolean {
	return UsernameType.safeParse(value).success;
}

export function deriveUsernameFromDisplayName(globalName: string): string | null {
	const candidate = displayNameCandidate(globalName);
	return candidate && isValidUsername(candidate) ? candidate : null;
}

function withNumericSuffix(base: string, suffix: number): string {
	const suffixText = String(suffix);
	return `${base.slice(0, MAX_USERNAME_LENGTH - suffixText.length)}${suffixText}`;
}

export function randomUsernameVariant(base: string): string {
	return withNumericSuffix(base, randomInt(1, 10000));
}

export function usernameBaseFromDisplayName(globalName: string, isBlocked: (username: string) => boolean): string {
	const candidate = displayNameCandidate(globalName);
	if (candidate) {
		for (const option of [candidate, randomUsernameVariant(candidate)]) {
			if (isValidUsername(option) && !isBlocked(option)) return option;
		}
	}
	return generateRandomUsername();
}

export function generateUsernameSuggestions(globalName: string): Array<string> {
	const candidate = deriveUsernameFromDisplayName(globalName);
	return candidate ? [candidate] : [];
}
