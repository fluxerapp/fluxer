// SPDX-License-Identifier: AGPL-3.0-or-later

import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import {ERROR_I18N_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import {describe, expect, it} from 'vitest';

const FORBIDDEN = [
	/[–—]/,
	/;/,
	/\b(carry|carries|carried|matters|sitting)\b/i,
	/\bservers?\b/i,
	/platform admin/i,
	/self-?bot|modified client|unofficial client/i,
	/@fluxer\.app/i,
];

const ENFORCEMENT_EMAILS = new Set(['account_deletion_scheduled_requested', 'report_resolved', 'dsa_report_resolved']);

const ENFORCEMENT_ERRORS = new Set([
	'admin_and_system.moderation_basis_required',
	'moderation_and_reports.report_outcome_invalid',
	'moderation_and_reports.ip_ban_linked_to_decision',
	'permissions.global_ip_banned',
	'permissions.global_ip_temporarily_banned',
]);

const SERVER_CATALOG_TEXT: Array<[string, string]> = [
	...Object.entries(CONTENT_I18N_MESSAGES),
	...Object.entries(ERROR_I18N_MESSAGES),
	...Object.entries(EMAIL_I18N_MESSAGES).map(([key, template]): [string, string] => [
		key,
		`${template.subject}\n${template.body}`,
	]),
];

const ENFORCEMENT_COPY: Array<[string, string]> = [
	...Object.entries(CONTENT_I18N_MESSAGES).filter(([key]) => key.startsWith('enforcement.')),
	...Object.entries(EMAIL_I18N_MESSAGES)
		.filter(([key]) => key.startsWith('enforcement_') || ENFORCEMENT_EMAILS.has(key))
		.flatMap(
			([key, template]): Array<[string, string]> => [
				[`${key}.subject`, template.subject],
				[`${key}.body`, template.body],
			],
		),
	...Object.entries(ERROR_I18N_MESSAGES).filter(([key]) => key.includes('enforcement') || ENFORCEMENT_ERRORS.has(key)),
];

describe('enforcement copy', () => {
	it('covers every catalog', () => {
		expect(ENFORCEMENT_COPY.length).toBeGreaterThan(100);
	});

	it.each(ENFORCEMENT_COPY)('%s follows the house style', (_key, text) => {
		for (const pattern of FORBIDDEN) {
			expect(text).not.toMatch(pattern);
		}
	});

	it('no server catalog blames platform administrators', () => {
		expect(SERVER_CATALOG_TEXT.filter(([, text]) => /platform admin/i.test(text)).map(([key]) => key)).toEqual([]);
	});

	it.each(['permissions.global_ip_banned', 'permissions.global_ip_temporarily_banned'] as const)(
		'%s never says the reader did something or should sign in',
		(key) => {
			expect(ERROR_I18N_MESSAGES[key]).not.toMatch(/\byou (did|broke|violated)\b|sign in/i);
			expect(ERROR_I18N_MESSAGES[key]).toContain('this block may have nothing to do with you');
		},
	);

	it.each(['report_resolved', 'dsa_report_resolved'] as const)(
		'%s states the decision about the item instead of a generic review line',
		(key) => {
			const {subject, body} = EMAIL_I18N_MESSAGES[key];
			expect(body).toContain('{outcome, select, removed {');
			expect(body).toContain('} action_taken {We found that what you reported broke our rules, and we took action. ');
			expect(`${subject}\n${body}`).not.toMatch(/already reviewed|take all reports seriously/i);
		},
	);
});
