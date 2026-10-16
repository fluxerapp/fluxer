import {
	BulkDeleteSelfMessagesFilter,
	BulkDeleteSelfMessagesRequest,
	HarvestSelfDataRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {describe, expect, it} from 'vitest';

const bulkDeleteDefaults = {
	scope: 'selected',
	include_dms: true,
	include_dms_closed: true,
	include_group_dms: true,
	include_guilds: true,
	guild_filter_mode: 'exclude',
	excluded_guild_ids: [],
	included_guild_ids: [],
};
const noSelectedContexts = {
	include_dms: false,
	include_dms_closed: false,
	include_group_dms: false,
	include_guilds: false,
};
const earlierDate = '2026-01-01T00:00:00Z';
const laterDate = '2026-02-01T00:00:00Z';
const selectedContextIssue = {
	code: 'custom',
	message: 'Enable at least one of include_dms, include_dms_closed, include_group_dms, or include_guilds.',
	path: ['include_dms'],
};
const dateRangeIssue = {
	code: 'custom',
	message: 'start_date must be earlier than end_date.',
	path: ['end_date'],
};

describe.each([
	{name: 'bulk-delete filter', schema: BulkDeleteSelfMessagesFilter},
	{name: 'bulk-delete request', schema: BulkDeleteSelfMessagesRequest},
	{name: 'harvest request', schema: HarvestSelfDataRequest},
])('$name', ({schema}) => {
	it('applies the shared defaults to an empty request', () => {
		expect(schema.parse({})).toEqual(bulkDeleteDefaults);
	});

	it.each(['include_dms', 'include_dms_closed', 'include_group_dms', 'include_guilds'] as const)(
		'accepts %s as the only selected context',
		(field) => {
			const selection = {...noSelectedContexts, [field]: true};
			expect(schema.parse(selection)).toEqual({...bulkDeleteDefaults, ...selection});
		},
	);

	it('does not require selected contexts for inaccessible-only scope', () => {
		const selection = {...noSelectedContexts, scope: 'inaccessible_only'};
		expect(schema.parse(selection)).toEqual({...bulkDeleteDefaults, ...selection});
	});

	it('preserves the guild filter and transforms both guild ID lists', () => {
		expect(
			schema.parse({guild_filter_mode: 'include_only', included_guild_ids: ['123'], excluded_guild_ids: ['456']}),
		).toEqual({
			...bulkDeleteDefaults,
			guild_filter_mode: 'include_only',
			included_guild_ids: [123n],
			excluded_guild_ids: [456n],
		});
	});

	it.each([
		{start_date: null, end_date: null},
		{start_date: earlierDate},
		{end_date: laterDate},
		{start_date: earlierDate, end_date: null},
		{start_date: null, end_date: laterDate},
		{start_date: earlierDate, end_date: laterDate},
	])('accepts the date bounds %j', (dates) => {
		expect(schema.parse(dates)).toEqual({...bulkDeleteDefaults, ...dates});
	});

	it.each([
		{input: noSelectedContexts, issues: [selectedContextIssue]},
		{input: {start_date: earlierDate, end_date: earlierDate}, issues: [dateRangeIssue]},
		{input: {start_date: laterDate, end_date: earlierDate}, issues: [dateRangeIssue]},
		{
			input: {...noSelectedContexts, start_date: laterDate, end_date: earlierDate},
			issues: [selectedContextIssue, dateRangeIssue],
		},
	])('reports exact refinement issues in order for $input', ({input, issues}) => {
		expect(schema.safeParse(input).error?.issues).toEqual(issues);
	});
});

describe('bulk-delete sudo verification', () => {
	it.each([
		{password: 'correct horse battery staple'},
		{mfa_method: 'totp', mfa_code: '123456'},
		{mfa_method: 'webauthn', webauthn_challenge: 'challenge'},
	])('preserves sudo fields only on the delete request: %j', (verification) => {
		expect(BulkDeleteSelfMessagesRequest.parse(verification)).toEqual({...bulkDeleteDefaults, ...verification});
		expect(BulkDeleteSelfMessagesFilter.parse(verification)).toEqual(bulkDeleteDefaults);
		expect(HarvestSelfDataRequest.parse(verification)).toEqual(bulkDeleteDefaults);
	});

	it('still validates sudo fields on the extended request', () => {
		expect(BulkDeleteSelfMessagesRequest.safeParse({mfa_method: 'unknown'})).toMatchObject({
			success: false,
			error: {issues: [{code: 'invalid_union', path: ['mfa_method']}]},
		});
	});
});
