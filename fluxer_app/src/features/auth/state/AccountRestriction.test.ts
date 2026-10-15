// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseAccountRestriction} from '@app/features/auth/state/AccountRestriction';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {EnforcementNotice, NetworkBlock} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {describe, expect, test} from 'vitest';

const notice: EnforcementNotice = {
	decision_id: '1234567890123456789',
	kind: 'suspension',
	permanent: false,
	issued_at: '2026-10-01T10:00:00.000Z',
	ends_at: '2026-10-08T10:00:00.000Z',
	deletion_at: null,
	summary: 'We suspended your account until Oct 8, 2026, 10:00 AM UTC.',
	effects: ["You can't sign in or use Fluxer until then. Your account, messages and communities are kept."],
	rule: {title: 'Spam', url: 'https://fluxer.com/guidelines#6-spam-and-service-abuse'},
	facts: ['We found that your account sent unwanted messages in bulk.'],
	statement: null,
	how_we_decided: ['We reviewed this after a report.'],
	appeal: {email: 'appeals@fluxer.com', deadline: 'Nov 30, 2026', account_has_email: true},
	redress: [],
};

const block: NetworkBlock = {
	ip_address: '203.0.113.7',
	appeal_email: 'support@fluxer.com',
	appeals_supported: true,
	ban_kind: 'permanent',
	expires_at: null,
	reason: null,
	product_name: 'Fluxer',
};

function forbidden(body: unknown, status = 403): HttpError {
	return new HttpError({method: 'POST', path: '/auth/login', status, body});
}

describe('parseAccountRestriction', () => {
	test.each([APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY, APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY])(
		'%s with a notice is a notice',
		(code) => {
			expect(parseAccountRestriction(forbidden({code, message: 'Suspended.', notice}))).toEqual({
				type: 'notice',
				notice,
			});
		},
	);

	test('a permanent suspension without a notice is not a restriction screen', () => {
		const error = forbidden({code: APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY, message: 'Suspended.', notice: null});
		expect(parseAccountRestriction(error)).toBeNull();
	});

	test('a malformed notice is ignored', () => {
		const error = forbidden({
			code: APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY,
			message: 'Suspended.',
			notice: {...notice, summary: 7},
		});
		expect(parseAccountRestriction(error)).toBeNull();
	});

	test.each([
		[APIErrorCodes.GLOBAL_IP_BANNED, block],
		[
			APIErrorCodes.GLOBAL_IP_TEMPORARILY_BANNED,
			{
				...block,
				ban_kind: 'temporary_24h' as const,
				expires_at: '2026-10-15T10:00:00.000Z',
				reason: {title: 'Spam', url: null},
			},
		],
	])('%s is a network block', (code, data) => {
		expect(parseAccountRestriction(forbidden({code, message: 'Blocked.', ...data}))).toEqual({
			type: 'network_blocked',
			block: data,
		});
	});

	test('a self-hosted network block keeps its null contact', () => {
		const data = {...block, appeal_email: null, appeals_supported: false};
		expect(
			parseAccountRestriction(forbidden({code: APIErrorCodes.GLOBAL_IP_BANNED, message: 'Blocked.', ...data})),
		).toEqual({type: 'network_blocked', block: data});
	});

	test('other errors are not restrictions', () => {
		expect(parseAccountRestriction(forbidden({code: APIErrorCodes.MISSING_ACCESS, message: 'No.'}))).toBeNull();
		expect(
			parseAccountRestriction(forbidden({code: APIErrorCodes.GLOBAL_IP_BANNED, message: 'Blocked.', ...block}, 400)),
		).toBeNull();
		expect(parseAccountRestriction(new Error('offline'))).toBeNull();
		expect(parseAccountRestriction(forbidden('Forbidden'))).toBeNull();
	});
});
