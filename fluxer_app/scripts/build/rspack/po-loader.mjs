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

import fs from 'node:fs/promises';

export default function poLoader(source) {
	const callback = this.async();

	(async () => {
		try {
			this.cacheable?.();

			const poPath = this.resourcePath;
			const compiledPath = `${poPath}.mjs`;

			this.addDependency?.(poPath);

			try {
				await fs.access(compiledPath);
				this.addDependency?.(compiledPath);
				const compiledSource = await fs.readFile(compiledPath, 'utf8');
				callback(null, compiledSource);
				return;
			} catch {}

			const content = Buffer.isBuffer(source) ? source.toString('utf8') : String(source);
			const messages = parsePoFile(content);

			const code = `export const messages = ${JSON.stringify(messages, null, 2)};\nexport default messages;\n`;

			callback(null, code);
		} catch (err) {
			callback(err);
		}
	})();
}

function parsePoFile(content) {
	const messages = {};
	const entries = splitEntries(content);

	for (const entry of entries) {
		const parsed = parseEntry(entry);
		if (!parsed) continue;
		messages[parsed.key] = parsed.value;
	}

	return messages;
}

function splitEntries(content) {
	const normalized = content.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
	return normalized
		.split(/\n{2,}/g)
		.map((s) => s.trim())
		.filter(Boolean);
}

function parseEntry(entry) {
	const lines = entry.split('\n');

	let msgctxt = null;
	let msgid = null;
	let msgidPlural = null;

	const msgstrMap = new Map();

	let active = null;
	let activeMsgstrIndex = 0;

	for (const rawLine of lines) {
		const line = rawLine.trim();
		if (!line || line.startsWith('#')) continue;

		if (line.startsWith('msgctxt ')) {
			active = 'msgctxt';
			activeMsgstrIndex = 0;
			msgctxt = extractPoString(line.slice('msgctxt '.length));
			continue;
		}

		if (line.startsWith('msgid_plural ')) {
			active = 'msgidPlural';
			activeMsgstrIndex = 0;
			msgidPlural = extractPoString(line.slice('msgid_plural '.length));
			continue;
		}

		if (line.startsWith('msgid ')) {
			active = 'msgid';
			activeMsgstrIndex = 0;
			msgid = extractPoString(line.slice('msgid '.length));
			continue;
		}

		const msgstrIndexed = line.match(/^msgstr\[(\d+)\]\s+/);
		if (msgstrIndexed) {
			active = 'msgstr';
			activeMsgstrIndex = Number(msgstrIndexed[1]);
			const rest = line.slice(msgstrIndexed[0].length);
			msgstrMap.set(activeMsgstrIndex, extractPoString(rest));
			continue;
		}

		if (line.startsWith('msgstr ')) {
			active = 'msgstr';
			activeMsgstrIndex = 0;
			msgstrMap.set(0, extractPoString(line.slice('msgstr '.length)));
			continue;
		}

		if (line.startsWith('"') && line.endsWith('"')) {
			const part = extractPoString(line);

			if (active === 'msgctxt') msgctxt = (msgctxt ?? '') + part;
			else if (active === 'msgid') msgid = (msgid ?? '') + part;
			else if (active === 'msgidPlural') msgidPlural = (msgidPlural ?? '') + part;
			else if (active === 'msgstr') msgstrMap.set(activeMsgstrIndex, (msgstrMap.get(activeMsgstrIndex) ?? '') + part);
		}
	}

	if (msgid == null) return null;

	if (msgid === '') return null;

	const key = msgctxt ? `${msgctxt}\u0004${msgid}` : msgid;

	if (msgidPlural != null) {
		const keys = Array.from(msgstrMap.keys());
		const maxIndex = keys.length ? Math.max(...keys.map((n) => Number(n))) : 0;

		const arr = [];
		for (let i = 0; i <= maxIndex; i++) {
			arr[i] = msgstrMap.get(i) ?? '';
		}

		return {key, value: arr};
	}

	return {key, value: msgstrMap.get(0) ?? ''};
}

function extractPoString(str) {
	const trimmed = str.trim();
	if (!trimmed.startsWith('"') || !trimmed.endsWith('"')) return trimmed;

	return trimmed.slice(1, -1).replace(/\\n/g, '\n').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
}
