// SPDX-License-Identifier: AGPL-3.0-or-later

const MAX_PASTED_URL_LENGTH = 2048;
const MAX_WRAPPED_SELECTION_LENGTH = 1024;
const ALLOWED_PROTOCOLS = new Set(['http:', 'https:']);

export function parsePastedUrl(clipboardText: string | null | undefined): string | null {
	if (clipboardText == null) {
		return null;
	}
	const candidate = clipboardText.trim();
	if (candidate.length === 0 || candidate.length > MAX_PASTED_URL_LENGTH) {
		return null;
	}
	if (/[\s<>]/.test(candidate)) {
		return null;
	}
	let parsed: URL;
	try {
		parsed = new URL(candidate);
	} catch {
		return null;
	}
	if (!ALLOWED_PROTOCOLS.has(parsed.protocol)) {
		return null;
	}
	return candidate;
}

export function canWrapSelectionAsLink(selectedText: string): boolean {
	if (selectedText.trim().length === 0 || selectedText.length > MAX_WRAPPED_SELECTION_LENGTH) {
		return false;
	}
	if (/[\r\n]/.test(selectedText)) {
		return false;
	}
	if (/[[\]]/.test(selectedText)) {
		return false;
	}
	return parsePastedUrl(selectedText) === null;
}

export function buildMaskedLink(selectedText: string, url: string): string {
	return `[${selectedText}](<${url}>)`;
}

export function resolvePastedLinkInsertion(pastedText: string, selectedText: string): string | null {
	const url = parsePastedUrl(pastedText);
	if (url === null || !canWrapSelectionAsLink(selectedText)) {
		return null;
	}
	return buildMaskedLink(selectedText, url);
}
