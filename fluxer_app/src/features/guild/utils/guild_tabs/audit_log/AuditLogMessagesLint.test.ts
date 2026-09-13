// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogChannelMessages';
import * as ExpressionMessageMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogExpressionMessageMessages';
import * as GuildMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogGuildMessages';
import * as InviteWebhookMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogInviteWebhookMessages';
import * as MemberMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberMessages';
import * as RoleMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogRoleMessages';
import * as SharedMessages from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogSharedMessages';
import {toText} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogTestUtils';
import {type MessageDescriptor, setupI18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

const MESSAGE_MODULES: Record<string, Record<string, unknown>> = {
	AuditLogChannelMessages: ChannelMessages,
	AuditLogExpressionMessageMessages: ExpressionMessageMessages,
	AuditLogGuildMessages: GuildMessages,
	AuditLogInviteWebhookMessages: InviteWebhookMessages,
	AuditLogMemberMessages: MemberMessages,
	AuditLogRoleMessages: RoleMessages,
	AuditLogSharedMessages: SharedMessages,
};

const ARGUMENT_PATTERN = /\{\s*(\w+)\s*[,}]/g;
const NUMERIC_ARGUMENT_PATTERN = /\{\s*(\w+)\s*,\s*(?:plural|selectordinal)\s*,/g;
const VAGUE_SET_PATTERN = /^(Set|Updated) \{\w+\} (to|from)/;
const DASH_OR_SEMICOLON_PATTERN = /[–—;]/;

const i18n = setupI18n({locale: 'en', messages: {en: {}}});

const DESCRIPTORS = Object.entries(MESSAGE_MODULES).flatMap(([moduleName, exports]) =>
	Object.entries(exports).map(([exportName, descriptor]) => ({
		name: `${moduleName}.${exportName}`,
		descriptor: descriptor as MessageDescriptor,
	})),
);

function sampleValues(message: string): Record<string, string | number> {
	const numeric = new Set(Array.from(message.matchAll(NUMERIC_ARGUMENT_PATTERN), (match) => match[1]));
	const values: Record<string, string | number> = {};
	for (const match of message.matchAll(ARGUMENT_PATTERN)) {
		const name = match[1];
		values[name] = numeric.has(name) ? 2 : `sample ${name}`;
	}
	return values;
}

describe('audit log messages', () => {
	it('finds the descriptors of every Messages file', () => {
		for (const [moduleName, exports] of Object.entries(MESSAGE_MODULES)) {
			expect(Object.keys(exports).length, moduleName).toBeGreaterThan(0);
		}
	});

	it.each(DESCRIPTORS)('$name is a descriptor with a message and a translator comment', ({descriptor}) => {
		expect(typeof descriptor.message).toBe('string');
		expect(descriptor.message?.trim()).not.toBe('');
		expect(typeof descriptor.comment).toBe('string');
		expect(descriptor.comment?.trim()).not.toBe('');
	});

	it.each(DESCRIPTORS)('$name formats with sample values and leaves no braces', ({descriptor}) => {
		const message = descriptor.message ?? '';
		const formatted = i18n._(descriptor, sampleValues(message));
		expect(formatted).not.toBe('');
		expect(formatted).not.toContain('{');
		expect(formatted).not.toContain('}');
	});

	it.each(DESCRIPTORS)('$name names every placeholder in its translator comment', ({descriptor}) => {
		const comment = descriptor.comment ?? '';
		for (const match of (descriptor.message ?? '').matchAll(ARGUMENT_PATTERN)) {
			expect(comment, match[1]).toContain(`{${match[1]}}`);
		}
	});

	it.each(DESCRIPTORS)('$name has no dash or semicolon in its message or translator comment', ({descriptor}) => {
		expect(descriptor.message ?? '').not.toMatch(DASH_OR_SEMICOLON_PATTERN);
		expect(descriptor.comment ?? '').not.toMatch(DASH_OR_SEMICOLON_PATTERN);
	});

	it.each(DESCRIPTORS)('$name does not use the vague set or updated pattern', ({descriptor}) => {
		const message = descriptor.message ?? '';
		expect(message).not.toMatch(VAGUE_SET_PATTERN);
		expect(message).not.toContain('Value for');
	});

	it('rejects a sentence whose values do not match its message placeholders', () => {
		const descriptor = ChannelMessages.CHANNEL_NAME_CHANGED_ROW;
		expect(() => toText({descriptor, values: {oldName: {kind: 'name', value: 'a'}}})).toThrow('Not passed: [newName]');
		expect(() =>
			toText({
				descriptor,
				values: {
					oldName: {kind: 'name', value: 'a'},
					newName: {kind: 'name', value: 'b'},
					name: {kind: 'name', value: 'c'},
				},
			}),
		).toThrow('Not in the message: [name]');
	});
});
