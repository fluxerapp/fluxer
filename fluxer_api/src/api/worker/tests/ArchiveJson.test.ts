// SPDX-License-Identifier: AGPL-3.0-or-later

import {createArchiveJsonBuffer, stringifyArchiveJson} from '@app/api/worker/utils/ArchiveJson';
import {describe, expect, test} from 'vitest';

describe('ArchiveJson', () => {
	test('serializes non-ASCII text as JSON escapes while preserving parsed values', () => {
		const guildName = '\u00c61';
		const emoji = '\ud83d\ude00';
		const json = stringifyArchiveJson({guild_name: guildName, emoji});

		expect(json).toContain('"guild_name": "\\u00c61"');
		expect(json).toContain('"emoji": "\\ud83d\\ude00"');
		expect(json).not.toContain('\u00c3');
		expect(JSON.parse(json)).toEqual({guild_name: guildName, emoji});
	});

	test('creates ASCII-only UTF-8 buffers for archive entries', () => {
		const guildName = '\u00c61';
		const buffer = createArchiveJsonBuffer({guild_name: guildName});

		expect(buffer.equals(Buffer.from(buffer.toString('ascii'), 'ascii'))).toBe(true);
		expect(JSON.parse(buffer.toString('utf8'))).toEqual({guild_name: guildName});
	});
});
