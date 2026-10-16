// SPDX-License-Identifier: AGPL-3.0-or-later

import {DsaReportFlowRequest, DsaReportRequest} from '@fluxer/schema/src/domains/report/ReportSchemas';
import {describe, expect, it} from 'vitest';

const legacyBodies: Array<Record<string, unknown>> = [
	{
		ticket: 'ticket-message',
		report_type: 'message',
		category: 'harassment',
		message_link: 'https://web.fluxer.app/channels/1/2/3',
		reported_user_tag: 'target#0001',
		reporter_full_legal_name: 'John Doe',
		reporter_country_of_residence: 'DE',
		reporter_fluxer_tag: 'reporter#1234',
		additional_info: 'DSA report for harassment',
	},
	{
		ticket: 'ticket-user-id',
		report_type: 'user',
		category: 'inappropriate_profile',
		user_id: '1427410010101010101',
		reporter_full_legal_name: 'John Doe',
		reporter_country_of_residence: 'SE',
	},
	{
		ticket: 'ticket-user-tag',
		report_type: 'user',
		category: 'underage_user',
		user_tag: 'target#0001',
		reporter_full_legal_name: 'John Doe',
		reporter_country_of_residence: 'FR',
	},
	{
		ticket: 'ticket-guild',
		report_type: 'guild',
		category: 'illegal_activity',
		guild_id: '1427410010101010102',
		invite_code: 'abcdef',
		reporter_full_legal_name: 'Jane Doe',
		reporter_country_of_residence: 'FR',
	},
];

const flowFields = {
	revision_hash: '6b1f0c9e2a7d4413',
	good_faith_confirmed: true,
	locale: 'de',
	additional_info: 'This message breaks section 130 of the German criminal code.',
	reporter_country_of_residence: 'DE',
};

const flowBodies: Array<Record<string, unknown>> = [
	{
		...flowFields,
		ticket: 'ticket-message',
		report_type: 'message',
		message_link: 'https://web.fluxer.app/channels/1/2/3',
		reporter_full_legal_name: 'John Doe',
		steps: [
			{screen_id: 'root_message', option_id: 'abuse'},
			{screen_id: 'abuse', option_id: 'hate'},
			{screen_id: 'hate', option_id: 'hate_incitement'},
		],
	},
	{
		...flowFields,
		ticket: 'ticket-user',
		report_type: 'user',
		user_id: '1427410010101010101',
		steps: [
			{screen_id: 'profile_parts', item_ids: ['photo']},
			{screen_id: 'root_user', option_id: 'sexual'},
			{screen_id: 'profile_sexual', option_id: 'minor_sexual'},
			{screen_id: 'profile_minor_sexual', option_id: 'csam'},
		],
	},
	{
		...flowFields,
		ticket: 'ticket-guild',
		report_type: 'guild',
		guild_id: '1427410010101010102',
		reporter_full_legal_name: 'Jane Doe',
		steps: [
			{screen_id: 'community_parts', item_ids: ['messages']},
			{screen_id: 'root_guild', option_id: 'raid'},
		],
	},
];

function issuePaths(result: {success: boolean; error?: {issues: Array<{path: Array<PropertyKey>}>}}): Array<string> {
	return (result.error?.issues ?? []).map((issue) => issue.path.map(String).join('.'));
}

describe('DsaReportRequest', () => {
	it('accepts every legacy body shape', () => {
		for (const body of legacyBodies) {
			expect(DsaReportRequest.safeParse(body).success, String(body.ticket)).toBe(true);
		}
	});

	it('accepts every flow body shape', () => {
		for (const body of flowBodies) {
			expect(DsaReportRequest.safeParse(body).success, String(body.ticket)).toBe(true);
		}
	});

	it('rejects a legacy body without a category', () => {
		const {category: _category, ...body} = legacyBodies[0];
		const result = DsaReportRequest.safeParse(body);
		expect(result.success).toBe(false);
		expect(issuePaths(result)).toContain('category');
	});

	it('rejects a legacy body without a legal name', () => {
		const {reporter_full_legal_name: _name, ...body} = legacyBodies[3];
		const result = DsaReportRequest.safeParse(body);
		expect(result.success).toBe(false);
		expect(issuePaths(result)).toContain('reporter_full_legal_name');
	});

	it('keeps the user_id or user_tag rule on the user branch', () => {
		const {user_id: _userId, ...body} = legacyBodies[1];
		expect(DsaReportRequest.safeParse(body).success).toBe(false);
		const {user_id: _flowUserId, ...flowBody} = flowBodies[1];
		expect(DsaReportRequest.safeParse(flowBody).success).toBe(false);
	});

	it('requires the revision hash, good-faith statement and explanation with steps', () => {
		for (const field of ['revision_hash', 'good_faith_confirmed', 'additional_info'] as const) {
			const {[field]: _removed, ...body} = flowBodies[0];
			const result = DsaReportRequest.safeParse(body);
			expect(result.success, field).toBe(false);
			expect(issuePaths(result)).toContain(field);
		}
		expect(DsaReportRequest.safeParse({...flowBodies[0], good_faith_confirmed: false}).success).toBe(false);
	});

	it('accepts a body that still sends reporter_fluxer_tag and drops the field', () => {
		const parsed = DsaReportRequest.parse(legacyBodies[0]);
		expect(legacyBodies[0]).toHaveProperty('reporter_fluxer_tag');
		expect(parsed).not.toHaveProperty('reporter_fluxer_tag');
		const flowParsed = DsaReportRequest.parse({...flowBodies[0], reporter_fluxer_tag: 'reporter#1234'});
		expect(flowParsed).not.toHaveProperty('reporter_fluxer_tag');
	});

	it('accepts a flow body without a legal name or with a category alongside', () => {
		const {reporter_full_legal_name: _name, ...body} = flowBodies[0];
		expect(DsaReportRequest.safeParse(body).success).toBe(true);
		const withCategory = DsaReportRequest.safeParse({...flowBodies[0], category: 'spam'});
		expect(withCategory.success).toBe(true);
	});
});

describe('DsaReportFlowRequest', () => {
	it('accepts every flow body, and each one also passes DsaReportRequest', () => {
		for (const body of flowBodies) {
			expect(DsaReportFlowRequest.safeParse(body).success, String(body.ticket)).toBe(true);
			expect(DsaReportRequest.safeParse(body).success, String(body.ticket)).toBe(true);
		}
	});

	it('rejects legacy bodies', () => {
		for (const body of legacyBodies) {
			expect(DsaReportFlowRequest.safeParse(body).success, String(body.ticket)).toBe(false);
		}
	});

	it('drops reporter_fluxer_tag from flow bodies', () => {
		const parsed = DsaReportFlowRequest.parse({...flowBodies[1], reporter_fluxer_tag: 'reporter#1234'});
		expect(parsed).not.toHaveProperty('reporter_fluxer_tag');
	});

	it('has no category field', () => {
		const parsed = DsaReportFlowRequest.parse({...flowBodies[2], category: 'spam'});
		expect('category' in parsed).toBe(false);
	});

	it('requires steps, revision hash, good-faith statement and a non-empty explanation', () => {
		for (const field of ['steps', 'revision_hash', 'good_faith_confirmed', 'additional_info'] as const) {
			const {[field]: _removed, ...body} = flowBodies[2];
			expect(DsaReportFlowRequest.safeParse(body).success, field).toBe(false);
		}
		expect(DsaReportFlowRequest.safeParse({...flowBodies[2], additional_info: ''}).success).toBe(false);
	});
});
