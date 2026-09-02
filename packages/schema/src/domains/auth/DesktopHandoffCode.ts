// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_HANDOFF_CODE_ALPHABET = 'ABCDEFGHJKMNPQRSTUVWXYZ23456789';
export const DESKTOP_HANDOFF_CODE_LENGTH = 12;

const DESKTOP_HANDOFF_CODE_GROUP_LENGTH = 6;
const DESKTOP_HANDOFF_CODE_SEPARATOR = '-';
const DESKTOP_HANDOFF_CODE_SEPARATOR_PATTERN = /[^A-Za-z0-9]/gu;

function normalizeDesktopHandoffCode(value: string): string {
	return value.replace(DESKTOP_HANDOFF_CODE_SEPARATOR_PATTERN, '').toUpperCase();
}

function isDesktopHandoffCode(value: string): boolean {
	if (value.length !== DESKTOP_HANDOFF_CODE_LENGTH) {
		return false;
	}
	return Array.from(value).every((character) => DESKTOP_HANDOFF_CODE_ALPHABET.includes(character));
}

function parseDesktopHandoffCodeInput(value: string): string {
	return normalizeDesktopHandoffCode(value).slice(0, DESKTOP_HANDOFF_CODE_LENGTH);
}

export function parseDesktopHandoffCode(value: string | null | undefined): string | null {
	if (value == null) {
		return null;
	}
	const normalized = normalizeDesktopHandoffCode(value);
	if (!isDesktopHandoffCode(normalized)) {
		return null;
	}
	return normalized;
}

export function formatDesktopHandoffCode(value: string): string {
	const normalized = parseDesktopHandoffCodeInput(value);
	const groups: Array<string> = [];
	for (let index = 0; index < normalized.length; index += DESKTOP_HANDOFF_CODE_GROUP_LENGTH) {
		groups.push(normalized.slice(index, index + DESKTOP_HANDOFF_CODE_GROUP_LENGTH));
	}
	return groups.join(DESKTOP_HANDOFF_CODE_SEPARATOR);
}
