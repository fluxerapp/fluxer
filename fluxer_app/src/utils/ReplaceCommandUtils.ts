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

const REPLACE_REGEX = /^s\/([^/]+)\/([^/]+)(\/g)?$/;

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
	return {
		source,
		replacement,
		global: !!globalFlag,
	};
}

export function executeReplaceCommand(text: string, command: ReplaceCommand): string {
	const regex = new RegExp(command.source, command.global ? 'g' : '');
	return text.replace(regex, command.replacement);
}

export function isReplaceCommand(content: string): boolean {
	return REPLACE_REGEX.test(content);
}
