// SPDX-License-Identifier: AGPL-3.0-or-later

const SILENT_MESSAGE_PREFIX_RE = /^(\s*)(@silent)(?:\s|$)/;

interface SilentMessagePrefix {
	tokenStart: number;
	tokenEnd: number;
	end: number;
}

export function parseSilentMessagePrefix(content: string): SilentMessagePrefix | null {
	const match = SILENT_MESSAGE_PREFIX_RE.exec(content);
	if (match == null) {
		return null;
	}
	const tokenStart = match[1]!.length;
	return {tokenStart, tokenEnd: tokenStart + match[2]!.length, end: match[0].length};
}
