// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {assertNoUndefinedParams} from './CassandraTypes';

function messageFor(path: string): string {
	return `Undefined value at "${path}". This project forbids undefined in Cassandra params; use null explicitly or omit the column via PATCH.`;
}

function guardError(params: Record<string, unknown>): string {
	try {
		assertNoUndefinedParams(params);
	} catch (error) {
		return (error as Error).message;
	}
	throw new Error('Expected assertNoUndefinedParams to throw');
}

describe('assertNoUndefinedParams', () => {
	it('accepts a realistic message row without throwing', () => {
		expect(() =>
			assertNoUndefinedParams({
				message_id: 123n,
				channel_id: 456n,
				content: 'hello',
				edited_at: null,
				pinned: false,
				created_at: new Date(0),
				blob: Buffer.from('x'),
				mention_users: [1n, 2n, 3n],
				mention_roles: new Set(['a', 'b']),
				reactions: new Map([['a', 1]]),
				embeds: {title: 'a', fields: [{name: 'n', value: 'v'}], footer: {text: null}},
			}),
		).not.toThrow();
	});

	it('reports the dotted path of a top level undefined param', () => {
		expect(guardError({user_id: undefined})).toBe(messageFor(':user_id'));
	});

	it('reports the dotted path of an undefined array element', () => {
		expect(guardError({mention_users: [1n, undefined, 3n]})).toBe(messageFor(':mention_users[1]'));
	});

	it('reports the dotted path of an undefined set member', () => {
		expect(guardError({mention_roles: new Set(['a', undefined])})).toBe(messageFor(':mention_roles{set:1}'));
	});

	it('reports the dotted path of an undefined map key', () => {
		expect(guardError({reactions: new Map([['a', 1] as const, [undefined, 2] as const])})).toBe(
			messageFor(':reactions{mapKey:1}'),
		);
	});

	it('reports the dotted path of an undefined map value', () => {
		expect(
			guardError({
				reactions: new Map([
					['a', 1],
					['b', undefined],
				]),
			}),
		).toBe(messageFor(':reactions{mapVal:1}'));
	});

	it('reports the dotted path of an undefined nested object property', () => {
		expect(guardError({embeds: {footer: {icon_url: undefined}}})).toBe(messageFor(':embeds.footer.icon_url'));
	});

	it('reports the dotted path of an undefined value nested through mixed containers', () => {
		expect(guardError({embeds: [{fields: new Set([new Map([['inline', undefined]])])}]})).toBe(
			messageFor(':embeds[0].fields{set:0}{mapVal:0}'),
		);
	});

	it('reports the first undefined in parameter order', () => {
		expect(guardError({a: 1, b: [undefined], c: undefined})).toBe(messageFor(':b[0]'));
	});
});
