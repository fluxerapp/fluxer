// SPDX-License-Identifier: AGPL-3.0-or-later

import {RICH_EMBED_DESCRIPTION_MAX_LENGTH} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {InstatusWebhook} from '@fluxer/schema/src/domains/webhook/InstatusWebhookSchemas';
import {describe, expect, it} from 'vitest';
import {transformInstatusWebhook} from '../transformers/InstatusTransformer';

function createMeta(): InstatusWebhook['meta'] {
	return {unsubscribe: 'https://fluxerstatus.com/unsubscribe?id=1&token=abc', documentation: ''};
}

function createPage(): InstatusWebhook['page'] {
	return {
		id: 'pg_123',
		status_indicator: 'HASISSUES',
		status_description: 'Partially degraded service',
		url: 'https://fluxerstatus.com',
	};
}

describe('Instatus transformer', () => {
	describe('incidents', () => {
		it('transforms an investigating incident', () => {
			const payload: InstatusWebhook = {
				meta: {unsubscribe: 'https://fluxerstatus.com/unsubscribe?id=1&token=abc', documentation: ''},
				page: createPage(),
				incident: {
					id: 'inc_1',
					name: 'Elevated API latency',
					url: 'https://fluxerstatus.com/incident/inc_1',
					status: 'INVESTIGATING',
					backfilled: false,
					created_at: '2026-07-06T10:00:00.000Z',
					updated_at: '2026-07-06T10:00:00.000Z',
					resolved_at: '',
					incident_updates: [
						{
							id: 'u_1',
							incident_id: 'inc_1',
							markdown: 'We are investigating serious levels of pain in the API. Ambulances are on standby.',
							status: 'INVESTIGATING',
							created_at: '2026-07-06T10:00:00.000Z',
							updated_at: '2026-07-06T10:00:00.000Z',
						},
					],
				},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed).not.toBeNull();
			expect(embed?.title).toBe('Elevated API latency');
			expect(embed?.url).toBe('https://fluxerstatus.com/incident/inc_1');
			expect(embed?.color).toBe(0xe23c39);
			expect(embed?.description).toBe(
				'We are investigating serious levels of pain in the API. Ambulances are on standby.',
			);
			expect(embed?.fields?.[0]).toEqual({name: 'Status', value: 'Investigating', inline: true});
			expect(embed?.footer?.text).toBe('Partially degraded service');
			expect(embed?.timestamp).toBeInstanceOf(Date);
		});

		it('colours a resolved incident green and omits the description when there is no update', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: createPage(),
				incident: {
					name: 'Elevated API latency',
					url: 'https://fluxerstatus.com/incident/inc_1',
					status: 'RESOLVED',
					incident_updates: [],
				},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.title).toBe('Elevated API latency');
			expect(embed?.color).toBe(0x3ecf8e);
			expect(embed?.description).toBeUndefined();
		});

		it('returns null when the incident has no name', () => {
			const payload: InstatusWebhook = {meta: createMeta(), page: createPage(), incident: {status: 'INVESTIGATING'}};
			expect(transformInstatusWebhook(payload)).toBeNull();
		});

		it('appends "Backfilled" to the footer when the incident is backfilled', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: createPage(),
				incident: {name: 'Backdated outage', status: 'RESOLVED', backfilled: true},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.footer?.text).toBe('Partially degraded service | Backfilled');
		});
	});

	describe('maintenances', () => {
		it('transforms an in-progress maintenance', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: {...createPage(), status_indicator: 'UNDERMAINTENANCE', status_description: 'Under Maintenance'},
				maintenance: {
					name: 'Database upgrade',
					url: 'https://fluxerstatus.com/maintenance/m_1',
					status: 'INPROGRESS',
					maintenance_updates: [
						{
							markdown: 'Maintenance has started. No idea what to maintain, though.',
							created_at: '2026-07-06T09:00:00.000Z',
						},
					],
				},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.title).toBe('Database upgrade');
			expect(embed?.url).toBe('https://fluxerstatus.com/maintenance/m_1');
			expect(embed?.color).toBe(0x3b82f6);
			expect(embed?.description).toBe('Maintenance has started. No idea what to maintain, though.');
			expect(embed?.fields?.[0]).toEqual({name: 'Status', value: 'In progress', inline: true});
			expect(embed?.footer?.text).toBe('Under Maintenance');
		});

		it('colours a completed maintenance green', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: createPage(),
				maintenance: {name: 'Database downgrade (trust)', status: 'COMPLETED'},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.title).toBe('Database downgrade (trust)');
			expect(embed?.color).toBe(0x3ecf8e);
		});
	});

	describe('component updates', () => {
		it('transforms a component going down', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: {...createPage(), status_description: 'Major outage'},
				component_update: {
					created_at: '2026-07-06T11:00:00.000Z',
					new_status: 'MAJOROUTAGE',
					component_id: 'c_1',
				},
				component: {id: 'c_1', name: 'API', status: 'MAJOROUTAGE', created_at: '2026-01-01T00:00:00.000Z'},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.title).toBe('API - major outage');
			expect(embed?.url).toBe('https://fluxerstatus.com');
			expect(embed?.color).toBe(0xe23c39);
			expect(embed?.footer?.text).toBe('Major outage');
			expect(embed?.timestamp).toBeInstanceOf(Date);
		});

		it('colours an operational component green', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: createPage(),
				component_update: {new_status: 'OPERATIONAL', component_id: 'c_1'},
				component: {id: 'c_1', name: 'API', status: 'OPERATIONAL'},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.title).toBe('API - operational');
			expect(embed?.color).toBe(0x3ecf8e);
		});

		it('normalises title case statuses with spaces (real casing)', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: createPage(),
				component_update: {new_status: 'Major outage', component_id: 'c_1'},
				component: {id: 'c_1', name: 'API', status: 'Major outage'},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.title).toBe('API - major outage');
			expect(embed?.color).toBe(0xe23c39);
		});
	});

	describe('robustness', () => {
		it('accepts an empty payload and returns null', () => {
			expect(transformInstatusWebhook(InstatusWebhook.parse({}))).toBeNull();
		});

		it('accepts nullish event sections and returns null', () => {
			expect(
				transformInstatusWebhook(
					InstatusWebhook.parse({
						meta: null,
						page: null,
						incident: null,
						maintenance: null,
						component_update: null,
						component: null,
					}),
				),
			).toBeNull();
		});

		it('drops non-http(s) urls', () => {
			const payload: InstatusWebhook = {
				meta: createMeta(),
				page: {url: 'javascript:alert(1)'},
				incident: {name: 'Outage', status: 'INVESTIGATING', url: 'not-a-url'},
			};
			const embed = transformInstatusWebhook(payload);
			expect(embed?.url).toBeUndefined();
		});
	});

	describe('Instatus delivery', () => {
		it('parses and renders an incident payload', () => {
			const raw = {
				meta: {
					unsubscribe:
						'https://jiralite.instatus.com/unsubscribe?id=cmr9oat950nga0kqg1688424b&token=221d8f1d-7e2f-43d1-b528-af72e7bb098c',
					documentation: '',
				},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'UP',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				incident: {
					backfilled: false,
					created_at: '2026-07-06T20:34:47.965Z',
					name: 'test',
					resolved_at: null,
					status: 'Investigating',
					updated_at: '2026-07-06T20:34:47.965Z',
					id: 'cmr9oi6n11bde0rmj0fooyzcc',
					incident_updates: [
						{
							id: 'cmr9oi6wh1bdf0rmj5ii7bf0d',
							incident_id: 'cmr9oi6n11bde0rmj0fooyzcc',
							markdown: 'Hope Kevin Fang picks this incident up because we are so bored.',
							status: 'Investigating',
							created_at: '2026-07-06T20:34:48.305Z',
							updated_at: '2026-07-06T20:34:48.305Z',
						},
					],
					affected_components: [],
					url: 'https://jiralite.instatus.com/cmr9oi6n11bde0rmj0fooyzcc',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('test');
			expect(embed?.color).toBe(0xe23c39);
			expect(embed?.description).toBe('Hope Kevin Fang picks this incident up because we are so bored.');
			expect(embed?.url).toBe('https://jiralite.instatus.com/cmr9oi6n11bde0rmj0fooyzcc');
			expect(embed?.fields?.[0]).toEqual({name: 'Status', value: 'Investigating', inline: true});
			expect(embed?.footer?.text).toBe('All systems operational');
			expect(embed?.timestamp).toBeInstanceOf(Date);
		});

		it('uses the latest update and lists affected components', () => {
			const raw = {
				meta: {
					unsubscribe:
						'https://jiralite.instatus.com/unsubscribe?id=cmr9oat950nga0kqg1688424b&token=221d8f1d-7e2f-43d1-b528-af72e7bb098c',
					documentation: '',
				},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'HASISSUES',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				incident: {
					backfilled: false,
					created_at: '2026-07-06T20:43:40.300Z',
					name: 'Basic incident',
					resolved_at: null,
					status: 'Identified',
					updated_at: '2026-07-06T20:44:41.287Z',
					id: 'cmr9otle41c9q0rqff1okyv7q',
					incident_updates: [
						{
							id: 'cmr9otlob1c9r0rqf1mvgixhn',
							incident_id: 'cmr9otle41c9q0rqff1okyv7q',
							markdown: 'Hope Kevin Fang picks this incident up because we are so bored.',
							status: 'Investigating',
							created_at: '2026-07-06T20:43:40.667Z',
							updated_at: '2026-07-06T20:44:41.281Z',
						},
						{
							id: 'cmr9ouwgg1ca00rqfy39xauwg',
							incident_id: 'cmr9otle41c9q0rqff1okyv7q',
							markdown: 'Update - identified.',
							status: 'Identified',
							created_at: '2026-07-06T20:44:41.296Z',
							updated_at: '2026-07-06T20:44:41.296Z',
						},
					],
					affected_components: [
						{id: 'cmr9osz9s1c9p0rqfgeiwp97t', name: 'Component', status: 'Major outage'},
						{id: 'cmr9ot5ld15is1aqfjptygehv', name: 'Component with no group', status: 'Major outage'},
					],
					url: 'https://jiralite.instatus.com/cmr9otle41c9q0rqff1okyv7q',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('Basic incident');
			expect(embed?.color).toBe(0xf97316);
			expect(embed?.description).toBe('Update - identified.');
			const affected = embed?.fields?.find((entry) => entry.name === 'Affected components');
			expect(affected?.inline).toBe(false);
			expect(affected?.value).toBe('- Component (major outage)\n- Component with no group (major outage)');
		});

		it('parses a component major outage', () => {
			const raw = {
				meta: {
					unsubscribe:
						'https://jiralite.instatus.com/unsubscribe?id=cmr9oat950nga0kqg1688424b&token=221d8f1d-7e2f-43d1-b528-af72e7bb098c',
					documentation: '',
				},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'HASISSUES',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				component_update: {
					created_at: '2026-07-06T20:44:44.403Z',
					new_status: 'MAJOROUTAGE',
					component_id: 'cmr9osz9s1c9p0rqfgeiwp97t',
				},
				component: {
					created_at: '2026-07-06T20:43:11.632Z',
					id: 'cmr9osz9s1c9p0rqfgeiwp97t',
					name: 'Component',
					status: 'MAJOROUTAGE',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('Component - major outage');
			expect(embed?.color).toBe(0xe23c39);
			expect(embed?.url).toBe('https://jiralite.instatus.com');
		});

		it('parses a component going under maintenance', () => {
			const raw = {
				meta: {
					unsubscribe:
						'https://jiralite.instatus.com/unsubscribe?id=cmr9oat950nga0kqg1688424b&token=221d8f1d-7e2f-43d1-b528-af72e7bb098c',
					documentation: '',
				},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'HASISSUES',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				component_update: {
					created_at: '2026-07-06T20:43:43.775Z',
					new_status: 'UNDERMAINTENANCE',
					component_id: 'cmr9ot5ld15is1aqfjptygehv',
				},
				component: {
					created_at: '2026-07-06T20:43:19.825Z',
					id: 'cmr9ot5ld15is1aqfjptygehv',
					name: 'Component with no group',
					status: 'UNDERMAINTENANCE',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('Component with no group - under maintenance');
			expect(embed?.color).toBe(0x3b82f6);
		});

		it('accepts a very long update and truncates the rendered description', () => {
			const sentence =
				'We implemented a fix somewhere. We have no idea where. In fact, not even sure it was implemented.';
			const raw = {
				meta: {unsubscribe: '', documentation: ''},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'HASISSUES',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				incident: {
					backfilled: false,
					created_at: '2026-07-06T21:51:54.422Z',
					name: 'Monitoring',
					resolved_at: null,
					status: 'Monitoring',
					updated_at: '2026-07-06T21:51:54.422Z',
					id: 'cmr9r9cfq008f1apmu4mt0g4f',
					incident_updates: [
						{
							id: 'cmr9r9cg7008g1apm0cvk6ob4',
							incident_id: 'cmr9r9cfq008f1apmu4mt0g4f',
							markdown: Array.from({length: 150}, () => sentence).join('\n\n'),
							status: 'Monitoring',
							created_at: '2026-07-06T21:51:54.439Z',
							updated_at: '2026-07-06T21:51:54.439Z',
						},
					],
					affected_components: [{id: 'cmr9osz9s1c9p0rqfgeiwp97t', name: 'Component', status: 'Major outage'}],
					url: 'https://jiralite.instatus.com/cmr9r9cfq008f1apmu4mt0g4f',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('Monitoring');
			expect(embed?.color).toBe(0xf5a623);
			expect((embed?.description ?? '').length).toBeLessThanOrEqual(RICH_EMBED_DESCRIPTION_MAX_LENGTH);
			expect(embed?.description?.startsWith('We implemented a fix')).toBe(true);
		});

		it('renders a resolved incident from its latest update', () => {
			const sentence =
				'We implemented a fix somewhere. We have no idea where. In fact, not even sure it was implemented.';
			const raw = {
				meta: {unsubscribe: '', documentation: ''},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'UNDERMAINTENANCE',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				incident: {
					backfilled: false,
					created_at: '2026-07-06T21:51:54.422Z',
					name: 'Monitoring',
					resolved_at: '2026-07-06T21:53:27.289Z',
					status: 'Resolved',
					updated_at: '2026-07-06T21:53:27.663Z',
					id: 'cmr9r9cfq008f1apmu4mt0g4f',
					incident_updates: [
						{
							id: 'cmr9r9cg7008g1apm0cvk6ob4',
							incident_id: 'cmr9r9cfq008f1apmu4mt0g4f',
							markdown: Array.from({length: 150}, () => sentence).join('\n\n'),
							status: 'Monitoring',
							created_at: '2026-07-06T21:51:54.439Z',
							updated_at: '2026-07-06T21:53:27.655Z',
						},
						{
							id: 'cmr9rbce7008v1apmlpf6zh5x',
							incident_id: 'cmr9r9cfq008f1apmu4mt0g4f',
							markdown: 'This incident has been resolved.',
							status: 'Resolved',
							created_at: '2026-07-06T21:53:27.679Z',
							updated_at: '2026-07-06T21:53:27.679Z',
						},
					],
					affected_components: [{id: 'cmr9osz9s1c9p0rqfgeiwp97t', name: 'Component', status: 'Under maintenance'}],
					url: 'https://jiralite.instatus.com/cmr9r9cfq008f1apmu4mt0g4f',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('Monitoring');
			expect(embed?.color).toBe(0x3ecf8e);
			expect(embed?.description).toBe('This incident has been resolved.');
			const affected = embed?.fields?.find((entry) => entry.name === 'Affected components');
			expect(affected?.value).toBe('Component (under maintenance)');
		});

		it('renders a scheduled maintenance', () => {
			const raw = {
				meta: {unsubscribe: '', documentation: ''},
				page: {
					id: 'cmr9kdnbr0mh40rqg5opm1nrj',
					status_indicator: 'UNDERMAINTENANCE',
					status_description: '',
					url: 'https://jiralite.instatus.com',
				},
				maintenance: {
					backfilled: false,
					maintenance_start_date: '2026-07-07T08:00:00.000Z',
					maintenance_end_date: '2026-07-07T09:00:00.000Z',
					created_at: '2026-07-06T21:56:13.780Z',
					name: 'scheduled',
					resolved_at: null,
					status: 'Planned',
					updated_at: '2026-07-06T21:56:13.780Z',
					id: 'cmr9rewk400811ao2gp7ytf92',
					maintenance_updates: [
						{
							id: 'cmr9rewtu00821ao2agy436tp',
							maintenance_id: 'cmr9rewk400811ao2gp7ytf92',
							markdown:
								'We are planning maintenance at this time. Just kidding, our database administrators just want to go outside. We used to call them on their cell phone too.',
							created_at: '2026-07-06T21:56:14.130Z',
							updated_at: '2026-07-06T21:56:14.130Z',
						},
					],
					affected_components: [
						{id: 'cmr9ot5ld15is1aqfjptygehv', name: 'Component with no group', status: 'Operational'},
					],
					url: 'https://jiralite.instatus.com/cmr9rewk400811ao2gp7ytf92',
				},
			};
			const embed = transformInstatusWebhook(InstatusWebhook.parse(raw));
			expect(embed?.title).toBe('scheduled');
			expect(embed?.color).toBe(0x3b82f6);
			expect(embed?.description).toBe(
				'We are planning maintenance at this time. Just kidding, our database administrators just want to go outside. We used to call them on their cell phone too.',
			);
			const affected = embed?.fields?.find((entry) => entry.name === 'Affected components');
			expect(affected?.value).toBe('Component with no group (operational)');
			const windowEntry = embed?.fields?.find((entry) => entry.name === 'Window');
			const startUnix = Math.floor(Date.parse('2026-07-07T08:00:00.000Z') / 1000);
			const endUnix = Math.floor(Date.parse('2026-07-07T09:00:00.000Z') / 1000);
			expect(windowEntry?.value).toBe(`<t:${startUnix}:F>-<t:${endUnix}:F>`);
			expect(windowEntry?.inline).toBe(false);
		});
	});
});
