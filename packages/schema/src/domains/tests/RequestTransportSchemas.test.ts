import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	CreateVoiceServerRequest,
	CreateVoiceServerRequestBody,
	UpdateVoiceRegionRequest,
	UpdateVoiceRegionRequestBody,
	UpdateVoiceServerRequest,
	UpdateVoiceServerRequestBody,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import {ChannelUpdateRequest, ChannelUpdateRequestBody} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {
	BulkDeleteSelfMessagesFilter,
	BulkDeleteSelfMessagesRequest,
	HarvestSelfDataRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {describe, expect, it} from 'vitest';

const server = {
	server_id: 'primary',
	endpoint: 'wss://voice.example.com',
	api_key: 'key',
	api_secret: 'secret',
};

describe('voice request bodies', () => {
	it('requires server creation fields but not the URL-supplied region', () => {
		const body = CreateVoiceServerRequestBody.parse(server);
		expect(body).toEqual({
			...server,
			is_active: true,
			soft_connection_limit: null,
			vip_only: false,
			required_guild_features: [],
			allowed_guild_ids: [],
			allowed_user_ids: [],
		});
		expect(CreateVoiceServerRequestBody.safeParse({}).success).toBe(false);
		expect(CreateVoiceServerRequest.safeParse(server).success).toBe(false);
		expect(CreateVoiceServerRequest.parse({...server, region_id: 'eu'})).toEqual({...body, region_id: 'eu'});
	});

	it.each(['server_id', 'endpoint', 'api_key', 'api_secret'] as const)('requires creation field %s', (field) => {
		expect(CreateVoiceServerRequestBody.safeParse({...server, [field]: undefined}).success).toBe(false);
	});

	it('keeps patch bodies empty without injecting defaults', () => {
		expect(UpdateVoiceRegionRequestBody.parse({})).toEqual({});
		expect(UpdateVoiceServerRequestBody.parse({})).toEqual({});
		expect(UpdateVoiceRegionRequest.parse({id: 'eu'})).toEqual({id: 'eu'});
		expect(UpdateVoiceServerRequest.parse({region_id: 'eu', server_id: 'primary'})).toEqual({
			region_id: 'eu',
			server_id: 'primary',
		});
	});

	it('does not treat client-supplied path fields as body properties', () => {
		expect(UpdateVoiceRegionRequestBody.parse({id: 'ignored', name: 'Europe'})).toEqual({name: 'Europe'});
		expect(UpdateVoiceServerRequestBody.parse({region_id: 'ignored', server_id: 'ignored'})).toEqual({});
	});

	it('retains explicit false, null, and snowflake transformations', () => {
		expect(
			UpdateVoiceServerRequestBody.parse({is_active: false, soft_connection_limit: null, allowed_user_ids: ['123']}),
		).toEqual({is_active: false, soft_connection_limit: null, allowed_user_ids: [123n]});
	});
});

describe.each([
	{name: 'create body', schema: CreateVoiceServerRequestBody, required: server},
	{name: 'create service request', schema: CreateVoiceServerRequest, required: {...server, region_id: 'eu'}},
	{name: 'update body', schema: UpdateVoiceServerRequestBody, required: {}},
	{name: 'update service request', schema: UpdateVoiceServerRequest, required: {region_id: 'eu', server_id: 'primary'}},
])('$name coordinates', ({schema, required}) => {
	it.each([
		{},
		{latitude: 0, longitude: 0},
		{latitude: null, longitude: null},
	])('accepts the paired state %j', (coordinates) => {
		expect(schema.parse({...required, ...coordinates})).toMatchObject(coordinates);
	});

	it.each([
		{latitude: 0},
		{longitude: 0},
		{latitude: null},
		{longitude: null},
		{latitude: 0, longitude: null},
		{latitude: null, longitude: 0},
	])('rejects the unpaired state %j', (coordinates) => {
		expect(schema.safeParse({...required, ...coordinates})).toMatchObject({
			success: false,
			error: {
				issues: [{path: ['latitude'], message: 'Latitude and longitude must both be provided or both be omitted'}],
			},
		});
	});
});

describe('channel update request body', () => {
	it('permits an empty body while the service validator still requires the stored channel type', () => {
		expect(ChannelUpdateRequestBody.parse({})).toEqual({});
		expect(ChannelUpdateRequest.safeParse({}).success).toBe(false);
	});

	it.each([
		ChannelTypes.GUILD_TEXT,
		ChannelTypes.GUILD_VOICE,
		ChannelTypes.GUILD_CATEGORY,
		ChannelTypes.GUILD_LINK,
		ChannelTypes.GROUP_DM,
	])('preserves field transformations for stored channel type %i', (type) => {
		const input = {name: '  General  ', owner_id: '123'};
		const expected = {name: 'General', owner_id: 123n};
		expect(ChannelUpdateRequestBody.parse(input)).toEqual(expected);
		expect(ChannelUpdateRequest.parse({...input, type})).toEqual({...expected, type});
	});

	it('preserves guild-only fields in the documented body', () => {
		const input = {topic: '  Topic  ', parent_id: '123', nsfw_override: false, bitrate: 64000, rtc_region: null};
		expect(ChannelUpdateRequestBody.parse(input)).toEqual({...input, topic: 'Topic', parent_id: 123n});
	});

	it('does not accept the injected discriminator as a body property', () => {
		expect(ChannelUpdateRequestBody.parse({type: ChannelTypes.GUILD_VOICE})).toEqual({});
	});
});

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

	it.each([
		'include_dms',
		'include_dms_closed',
		'include_group_dms',
		'include_guilds',
	] as const)('accepts %s as the only selected context', (field) => {
		const selection = {...noSelectedContexts, [field]: true};
		expect(schema.parse(selection)).toEqual({...bulkDeleteDefaults, ...selection});
	});

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
