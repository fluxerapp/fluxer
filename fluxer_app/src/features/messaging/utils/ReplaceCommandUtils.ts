// SPDX-License-Identifier: AGPL-3.0-or-later

import {escapeRegex} from '@app/features/messaging/utils/RegexUtils';

const REPLACE_REGEX = /^s\/(.+?)\/(.*?)(?:\/(g)?)?$/;

interface ReplaceCommand {
	source: string;
	replacement: string;
	global: boolean;
}

export function parseReplaceCommand(content: string): ReplaceCommand | null {
	const match = content.match(REPLACE_REGEX);
	if (!match) {
		return null;
	}
	const [, source, replacement, globalFlag] = match;
	if (!source) {
		return null;
	}
	return {
		source,
		replacement: replacement ?? '',
		global: !!globalFlag,
	};
}

const REPLACE_PATTERN_CACHE = new Map<string, RegExp>();
const REPLACE_PATTERN_LIMIT = 64;
const REPLACE_PATTERN_KEY_SEPARATOR = '\u0000';

function getReplacePattern(source: string, global: boolean): RegExp {
	const key = `${global ? 'g' : ''}${REPLACE_PATTERN_KEY_SEPARATOR}${source}`;
	const cached = REPLACE_PATTERN_CACHE.get(key);
	if (cached) return cached;
	const compiled = new RegExp(escapeRegex(source), global ? 'g' : '');
	if (REPLACE_PATTERN_CACHE.size >= REPLACE_PATTERN_LIMIT) REPLACE_PATTERN_CACHE.clear();
	REPLACE_PATTERN_CACHE.set(key, compiled);
	return compiled;
}

export function executeReplaceCommand(text: string, command: ReplaceCommand): string {
	const regex = getReplacePattern(command.source, command.global);
	return text.replace(regex, command.replacement.replace(/\$/g, '$$$$'));
}

export function isReplaceCommand(content: string): boolean {
	return REPLACE_REGEX.test(content);
}
