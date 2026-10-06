// SPDX-License-Identifier: AGPL-3.0-or-later

export const RECOVERY_KEY_ALPHABET = '0123456789ABCDEFGHJKMNPQRSTVWXYZ';
export const RECOVERY_KEY_BYTE_LENGTH = 20;
export const RECOVERY_KEY_LENGTH = 32;
export const RECOVERY_KEY_GROUP_SIZE = 4;
export const RECOVERY_KEY_SEPARATOR = '-';
export const RECOVERY_KEY_FORMATTED_LENGTH = RECOVERY_KEY_LENGTH + RECOVERY_KEY_LENGTH / RECOVERY_KEY_GROUP_SIZE - 1;

const RECOVERY_KEY_ALIASES: Readonly<Record<string, string>> = {
	O: '0',
	I: '1',
	L: '1',
};

const IGNORED_INPUT_CHARACTERS = /[\s\u2010-\u2015\u2212-]/gu;

function mapInputCharacter(character: string): string | null {
	const upper = character.toUpperCase();
	const mapped = RECOVERY_KEY_ALIASES[upper] ?? upper;
	return mapped.length === 1 && RECOVERY_KEY_ALPHABET.includes(mapped) ? mapped : null;
}

export function encodeRecoveryKey(bytes: Uint8Array): string {
	if (bytes.length !== RECOVERY_KEY_BYTE_LENGTH) {
		throw new Error(`Recovery key needs exactly ${RECOVERY_KEY_BYTE_LENGTH} bytes`);
	}
	let output = '';
	let buffer = 0;
	let bits = 0;
	for (const byte of bytes) {
		buffer = (buffer << 8) | byte;
		bits += 8;
		while (bits >= 5) {
			bits -= 5;
			output += RECOVERY_KEY_ALPHABET[(buffer >> bits) & 31];
		}
		buffer &= (1 << bits) - 1;
	}
	return output;
}

export function formatRecoveryKey(key: string): string {
	const groups: Array<string> = [];
	for (let index = 0; index < key.length; index += RECOVERY_KEY_GROUP_SIZE) {
		groups.push(key.slice(index, index + RECOVERY_KEY_GROUP_SIZE));
	}
	return groups.join(RECOVERY_KEY_SEPARATOR);
}

export function generateRecoveryKey(randomBytes: Uint8Array): {key: string; formatted: string} {
	const key = encodeRecoveryKey(randomBytes);
	return {key, formatted: formatRecoveryKey(key)};
}

export function normalizeRecoveryKey(input: string): string | null {
	const stripped = input.replace(IGNORED_INPUT_CHARACTERS, '');
	if (stripped.length !== RECOVERY_KEY_LENGTH) {
		return null;
	}
	let normalized = '';
	for (const character of stripped) {
		const mapped = mapInputCharacter(character);
		if (mapped === null) {
			return null;
		}
		normalized += mapped;
	}
	return normalized;
}

export function isValidRecoveryKey(input: string): boolean {
	return normalizeRecoveryKey(input) !== null;
}

export function formatRecoveryKeyInput(input: string): string {
	let normalized = '';
	for (const character of input.replace(IGNORED_INPUT_CHARACTERS, '')) {
		const mapped = mapInputCharacter(character);
		if (mapped === null) {
			continue;
		}
		normalized += mapped;
		if (normalized.length >= RECOVERY_KEY_LENGTH) {
			break;
		}
	}
	return formatRecoveryKey(normalized);
}
