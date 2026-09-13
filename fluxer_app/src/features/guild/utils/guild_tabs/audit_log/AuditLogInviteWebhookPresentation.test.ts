// SPDX-License-Identifier: AGPL-3.0-or-later

import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import type {AuditLogPresentationContext} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
	TEST_GUILD_ID,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogTestUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
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

type EntryFixture = Parameters<typeof makeEntry>[0];
type ChangeFixture = NonNullable<EntryFixture['changes']>;

const ALICE_ID = '1400000000000000002';
const CHANNEL_ID = '1400000000000000200';
const OTHER_CHANNEL_ID = '1400000000000000201';
const WEBHOOK_ID = '1400000000000000300';
const INVITE_CODE = 'aB3dE9xZ';
const USER_NAMES = {[TEST_ACTOR_ID]: 'Hampus', [ALICE_ID]: 'Alice'};

function present(fixture: EntryFixture, context: AuditLogPresentationContext = fakeContext()) {
	const presentation = presentAuditLogEntry(makeEntry(fixture), context);
	return {...resultToText(presentation, USER_NAMES), expandable: presentation.expandable};
}

function snapshot(side: 'old_value' | 'new_value', values: Record<string, unknown>): ChangeFixture {
	return Object.entries(values).map(([key, value]) => ({key, [side]: value}));
}

interface InviteSettingsFixture {
	max_age: number;
	max_uses: number;
	temporary: boolean;
}

function inviteCreate(settings: InviteSettingsFixture): EntryFixture {
	return {
		action_type: AuditLogActionType.INVITE_CREATE,
		target_id: INVITE_CODE,
		options: {...settings, channel_id: CHANNEL_ID, inviter_id: TEST_ACTOR_ID},
		changes: snapshot('new_value', {
			code: INVITE_CODE,
			channel_id: CHANNEL_ID,
			guild_id: TEST_GUILD_ID,
			inviter_id: TEST_ACTOR_ID,
			uses: 0,
			...settings,
			created_at: '2026-09-01T11:59:59.990Z',
		}),
	};
}

interface InviteDeleteFixture extends InviteSettingsFixture {
	user_id?: string;
	inviter_id: string;
	uses: number;
	created_at?: string;
}

function inviteDelete({user_id, inviter_id, uses, created_at, ...settings}: InviteDeleteFixture): EntryFixture {
	return {
		action_type: AuditLogActionType.INVITE_DELETE,
		user_id: user_id ?? TEST_ACTOR_ID,
		target_id: INVITE_CODE,
		options: {...settings, channel_id: CHANNEL_ID, inviter_id},
		changes: snapshot('old_value', {
			code: INVITE_CODE,
			channel_id: CHANNEL_ID,
			guild_id: TEST_GUILD_ID,
			inviter_id,
			uses,
			...settings,
			...(created_at === undefined ? {} : {created_at}),
		}),
	};
}

function webhookSnapshot(side: 'old_value' | 'new_value', creatorId: string): ChangeFixture {
	return snapshot(side, {
		id: WEBHOOK_ID,
		guild_id: TEST_GUILD_ID,
		channel_id: CHANNEL_ID,
		name: 'Captain Hook',
		creator_id: creatorId,
		avatar_hash: null,
		type: 1,
	});
}

function webhookUpdate(changes: ChangeFixture | undefined): EntryFixture {
	return {
		action_type: AuditLogActionType.WEBHOOK_UPDATE,
		target_id: WEBHOOK_ID,
		options: {channel_id: CHANNEL_ID},
		changes,
	};
}

const WEBHOOK_NAMES = fakeContext({webhooks: {[WEBHOOK_ID]: 'Captain Hook'}});

describe('presentInviteCreate', () => {
	it('describes an invite created with the app defaults', () => {
		expect(present(inviteCreate({max_age: 604800, max_uses: 0, temporary: false}))).toEqual({
			summary: `Hampus created the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: ['~ The invite was set to expire after 7 days.', '~ The invite was set to allow unlimited uses.'],
			blocks: [],
			expandable: true,
		});
	});

	it('describes a 30 minute single use invite that grants temporary membership', () => {
		expect(present(inviteCreate({max_age: 1800, max_uses: 1, temporary: true}))).toEqual({
			summary: `Hampus created the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was set to expire after 30 minutes.',
				'~ The invite was limited to 1 use.',
				'~ The invite was set to grant temporary membership.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('renders a 5400 second expiry as 90 minutes and pluralizes the use limit', () => {
		expect(present(inviteCreate({max_age: 5400, max_uses: 25, temporary: false})).rows).toEqual([
			'~ The invite was set to expire after 90 minutes.',
			'~ The invite was limited to 25 uses.',
		]);
	});

	it('reads the dispatch string form of the options', () => {
		expect(
			present({
				action_type: AuditLogActionType.INVITE_CREATE,
				target_id: INVITE_CODE,
				options: {
					max_uses: '10',
					max_age: '0',
					temporary: 'true',
					channel_id: CHANNEL_ID,
					inviter_id: TEST_ACTOR_ID,
				},
			}),
		).toEqual({
			summary: `Hampus created the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was set to never expire.',
				'~ The invite was limited to 10 uses.',
				'~ The invite was set to grant temporary membership.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('leaves the channel out of the summary when no channel id was recorded', () => {
		const {options, changes, ...fixture} = inviteCreate({max_age: 604800, max_uses: 0, temporary: false});
		const {channel_id: _channelId, ...optionsWithoutChannel} = options ?? {};
		expect(
			present({
				...fixture,
				options: optionsWithoutChannel,
				changes: changes?.filter((change) => change.key !== 'channel_id'),
			}),
		).toEqual({
			summary: 'Hampus created the invite aB3dE9xZ.',
			rows: ['~ The invite was set to expire after 7 days.', '~ The invite was set to allow unlimited uses.'],
			blocks: [],
			expandable: true,
		});
	});

	it('falls back to the changes for the code, channel and settings when options are missing', () => {
		const {options: _options, ...fixture} = inviteCreate({max_age: 3600, max_uses: 5, temporary: true});
		expect(present({...fixture, target_id: null})).toEqual({
			summary: `Hampus created the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was set to expire after 1 hour.',
				'~ The invite was limited to 5 uses.',
				'~ The invite was set to grant temporary membership.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('omits the channel and every row whose value cannot be read', () => {
		expect(
			present({
				action_type: AuditLogActionType.INVITE_CREATE,
				user_id: '0',
				target_id: INVITE_CODE,
				options: {max_age: -60, max_uses: 1.5, temporary: 'yes', channel_id: 'general'},
				changes: [
					{key: 'max_age', new_value: 'forever'},
					{key: 'max_uses', new_value: null},
					{key: 'temporary', new_value: 1},
					{key: 'channel_id', new_value: 42},
				],
			}),
		).toEqual({
			summary: 'System created the invite aB3dE9xZ.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});
});

describe('presentInviteUpdate', () => {
	it('summarizes the invite without rows', () => {
		expect(
			present({
				action_type: AuditLogActionType.INVITE_UPDATE,
				target_id: INVITE_CODE,
				options: {max_age: 0, max_uses: 3, channel_id: CHANNEL_ID},
			}),
		).toEqual({
			summary: 'Hampus updated the invite aB3dE9xZ.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});
});

describe('presentInviteDelete', () => {
	it('describes an own invite deleted after 3 uses with its expiry date', () => {
		expect(
			present(
				inviteDelete({
					inviter_id: TEST_ACTOR_ID,
					uses: 3,
					max_uses: 0,
					max_age: 604800,
					temporary: false,
					created_at: '2026-08-30T12:00:00.000Z',
				}),
			),
		).toEqual({
			summary: `Hampus deleted the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was created on 2026-08-30T12:00:00.000Z.',
				'~ The invite was used 3 times.',
				'~ The invite was scheduled to expire on 2026-09-06T12:00:00.000Z.',
				'~ The invite was set to allow unlimited uses.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it("names the creator when a moderator deletes another member's temporary invite", () => {
		expect(
			present(
				inviteDelete({
					inviter_id: ALICE_ID,
					uses: 1,
					max_uses: 5,
					max_age: 0,
					temporary: true,
					created_at: '2026-08-31T08:30:00.000Z',
				}),
			),
		).toEqual({
			summary: `Hampus deleted the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was created by Alice on 2026-08-31T08:30:00.000Z.',
				'~ The invite was used 1 time.',
				'~ The invite was set to never expire.',
				'~ The invite was limited to 5 uses.',
				'~ The invite was set to grant temporary membership.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('names the creator without a date and gives the expiry as a duration when created_at is missing', () => {
		expect(
			present(inviteDelete({inviter_id: ALICE_ID, uses: 0, max_uses: 1, max_age: 86400, temporary: false})),
		).toEqual({
			summary: `Hampus deleted the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was created by Alice.',
				'~ The invite was never used.',
				'~ The invite was set to expire after 1 day.',
				'~ The invite was limited to 1 use.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('gives no creator row for an own invite without created_at and says a max_age of 0 never expires', () => {
		expect(
			present(inviteDelete({inviter_id: TEST_ACTOR_ID, uses: 0, max_uses: 0, max_age: 0, temporary: false})).rows,
		).toEqual([
			'~ The invite was never used.',
			'~ The invite was set to never expire.',
			'~ The invite was set to allow unlimited uses.',
		]);
	});

	it('reads the dispatch string form of the options', () => {
		expect(
			present({
				action_type: AuditLogActionType.INVITE_DELETE,
				target_id: INVITE_CODE,
				options: {
					max_uses: '2',
					max_age: '3600',
					temporary: 'false',
					channel_id: CHANNEL_ID,
					inviter_id: ALICE_ID,
				},
				changes: [
					{key: 'uses', old_value: 2},
					{key: 'created_at', old_value: '2026-09-01T10:00:00.000Z'},
				],
			}).rows,
		).toEqual([
			'~ The invite was created by Alice on 2026-09-01T10:00:00.000Z.',
			'~ The invite was used 2 times.',
			'~ The invite was scheduled to expire on 2026-09-01T11:00:00.000Z.',
			'~ The invite was limited to 2 uses.',
		]);
	});

	it('falls back to the changes for the code, channel, inviter and settings when options are missing', () => {
		const {options: _options, ...fixture} = inviteDelete({
			inviter_id: ALICE_ID,
			uses: 4,
			max_uses: 10,
			max_age: 1800,
			temporary: true,
			created_at: '2026-09-01T11:00:00.000Z',
		});
		expect(present({...fixture, target_id: null})).toEqual({
			summary: `Hampus deleted the invite aB3dE9xZ for #${CHANNEL_ID}.`,
			rows: [
				'~ The invite was created by Alice on 2026-09-01T11:00:00.000Z.',
				'~ The invite was used 4 times.',
				'~ The invite was scheduled to expire on 2026-09-01T11:30:00.000Z.',
				'~ The invite was limited to 10 uses.',
				'~ The invite was set to grant temporary membership.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('omits the channel and every row whose value cannot be read', () => {
		expect(
			present({
				action_type: AuditLogActionType.INVITE_DELETE,
				target_id: INVITE_CODE,
				options: {inviter_id: 'alice', max_age: 'never', channel_id: ''},
				changes: [
					{key: 'uses', old_value: 'many'},
					{key: 'created_at', old_value: 'yesterday'},
					{key: 'inviter_id', old_value: 42},
					{key: 'max_uses', old_value: -1},
					{key: 'temporary', old_value: 'maybe'},
				],
			}),
		).toEqual({
			summary: 'Hampus deleted the invite aB3dE9xZ.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});
});

describe('presentWebhookCreate', () => {
	it('names the webhook and its channel', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_CREATE,
				target_id: WEBHOOK_ID,
				options: {channel_id: CHANNEL_ID},
				changes: webhookSnapshot('new_value', TEST_ACTOR_ID),
			}),
		).toEqual({
			summary: `Hampus created the webhook Captain Hook in #${CHANNEL_ID}.`,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('names the webhook without a channel when none was recorded', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_CREATE,
				target_id: WEBHOOK_ID,
				changes: webhookSnapshot('new_value', TEST_ACTOR_ID).filter((change) => change.key !== 'channel_id'),
			}).summary,
		).toBe('Hampus created the webhook Captain Hook.');
	});

	it('says a webhook was created when no name was recorded', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_CREATE,
				target_id: WEBHOOK_ID,
				options: {channel_id: CHANNEL_ID},
			}),
		).toEqual({summary: 'Hampus created a webhook.', rows: [], blocks: [], expandable: false});
	});

	it('takes the channel from the changes when the option is missing', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_CREATE,
				target_id: WEBHOOK_ID,
				changes: webhookSnapshot('new_value', TEST_ACTOR_ID),
			}).summary,
		).toBe(`Hampus created the webhook Captain Hook in #${CHANNEL_ID}.`);
	});

	it('is expandable only for the decoded Reason block', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_CREATE,
				target_id: WEBHOOK_ID,
				reason: 'Release%20notes',
				options: {channel_id: CHANNEL_ID},
				changes: webhookSnapshot('new_value', TEST_ACTOR_ID),
			}),
		).toEqual({
			summary: `Hampus created the webhook Captain Hook in #${CHANNEL_ID}.`,
			rows: [],
			blocks: [{kind: 'reason', text: 'Release notes'}],
			expandable: true,
		});
	});
});

describe('presentWebhookUpdate', () => {
	it('summarizes a rename alone without rows', () => {
		expect(present(webhookUpdate([{key: 'name', old_value: 'Hook A', new_value: 'Hook B'}]), WEBHOOK_NAMES)).toEqual({
			summary: 'Hampus renamed the webhook Hook A to Hook B.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('marks an added avatar', () => {
		expect(
			present(webhookUpdate([{key: 'avatar_hash', old_value: null, new_value: 'a1b2c3d4'}]), WEBHOOK_NAMES),
		).toEqual({
			summary: 'Hampus updated the webhook Captain Hook.',
			rows: ['+ Added an avatar.'],
			blocks: [],
			expandable: true,
		});
	});

	it('marks a replaced avatar', () => {
		expect(
			present(webhookUpdate([{key: 'avatar_hash', old_value: 'a1b2c3d4', new_value: 'a_9f8e7d6c'}]), WEBHOOK_NAMES)
				.rows,
		).toEqual(['~ Changed the avatar.']);
	});

	it('marks a removed avatar', () => {
		expect(
			present(webhookUpdate([{key: 'avatar_hash', old_value: 'a1b2c3d4', new_value: null}]), WEBHOOK_NAMES).rows,
		).toEqual(['- Removed the avatar.']);
	});

	it('describes a move between channels', () => {
		expect(
			present(webhookUpdate([{key: 'channel_id', old_value: CHANNEL_ID, new_value: OTHER_CHANNEL_ID}]), WEBHOOK_NAMES),
		).toEqual({
			summary: 'Hampus updated the webhook Captain Hook.',
			rows: [`~ Moved from #${CHANNEL_ID} to #${OTHER_CHANNEL_ID}.`],
			blocks: [],
			expandable: true,
		});
	});

	it('lists every change of a multi-field update in name, channel and avatar order under the new name', () => {
		expect(
			present(
				webhookUpdate([
					{key: 'channel_id', old_value: CHANNEL_ID, new_value: OTHER_CHANNEL_ID},
					{key: 'name', old_value: 'Hook A', new_value: 'Hook B'},
					{key: 'avatar_hash', old_value: 'a1b2c3d4', new_value: null},
				]),
				WEBHOOK_NAMES,
			),
		).toEqual({
			summary: 'Hampus updated the webhook Hook B.',
			rows: [
				'~ Changed the name from Hook A to Hook B.',
				`~ Moved from #${CHANNEL_ID} to #${OTHER_CHANNEL_ID}.`,
				'- Removed the avatar.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('takes the name from the context when the name did not change', () => {
		expect(present(webhookUpdate(undefined), WEBHOOK_NAMES)).toEqual({
			summary: 'Hampus updated the webhook Captain Hook.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('says a webhook was updated when the name is unknown', () => {
		expect(present(webhookUpdate([{key: 'avatar_hash', old_value: null, new_value: 'a1b2c3d4'}]))).toEqual({
			summary: 'Hampus updated a webhook.',
			rows: ['+ Added an avatar.'],
			blocks: [],
			expandable: true,
		});
	});

	it('omits rows for unchanged or unreadable values', () => {
		expect(
			present(
				webhookUpdate([
					{key: 'name', old_value: 'Hook', new_value: ' Hook '},
					{key: 'channel_id', old_value: CHANNEL_ID, new_value: 'general'},
					{key: 'avatar_hash', old_value: 12345, new_value: null},
				]),
			),
		).toEqual({summary: 'Hampus updated the webhook Hook.', rows: [], blocks: [], expandable: false});
		expect(
			present(
				webhookUpdate([
					{key: 'channel_id', old_value: CHANNEL_ID, new_value: CHANNEL_ID},
					{key: 'avatar_hash', old_value: '', new_value: null},
				]),
			),
		).toEqual({summary: 'Hampus updated a webhook.', rows: [], blocks: [], expandable: false});
	});

	it('does not use the rename summary when the previous name cannot be read', () => {
		expect(present(webhookUpdate([{key: 'name', old_value: '   ', new_value: 'Hook B'}]), WEBHOOK_NAMES)).toEqual({
			summary: 'Hampus updated the webhook Hook B.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});
});

describe('presentWebhookDelete', () => {
	it('describes an own webhook deleted from its channel without rows', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_DELETE,
				target_id: WEBHOOK_ID,
				options: {channel_id: CHANNEL_ID},
				changes: webhookSnapshot('old_value', TEST_ACTOR_ID),
			}),
		).toEqual({
			summary: `Hampus deleted the webhook Captain Hook from #${CHANNEL_ID}.`,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it("names the creator of another member's webhook", () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_DELETE,
				target_id: WEBHOOK_ID,
				options: {channel_id: CHANNEL_ID},
				changes: webhookSnapshot('old_value', ALICE_ID),
			}),
		).toEqual({
			summary: `Hampus deleted the webhook Captain Hook from #${CHANNEL_ID}.`,
			rows: ['~ The webhook was created by Alice.'],
			blocks: [],
			expandable: true,
		});
	});

	it('leaves the channel out when none was recorded', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_DELETE,
				target_id: WEBHOOK_ID,
				changes: webhookSnapshot('old_value', ALICE_ID).filter((change) => change.key !== 'channel_id'),
			}),
		).toEqual({
			summary: 'Hampus deleted the webhook Captain Hook.',
			rows: ['~ The webhook was created by Alice.'],
			blocks: [],
			expandable: true,
		});
	});

	it('says a webhook was deleted when no name was recorded and ignores an unreadable creator', () => {
		expect(
			present({
				action_type: AuditLogActionType.WEBHOOK_DELETE,
				target_id: WEBHOOK_ID,
				options: {channel_id: CHANNEL_ID},
				changes: [{key: 'creator_id', old_value: 1400}],
			}),
		).toEqual({summary: 'Hampus deleted a webhook.', rows: [], blocks: [], expandable: false});
	});

	it('ignores a name from the context for a deleted webhook', () => {
		expect(
			present(
				{action_type: AuditLogActionType.WEBHOOK_DELETE, target_id: WEBHOOK_ID, options: {channel_id: CHANNEL_ID}},
				WEBHOOK_NAMES,
			).summary,
		).toBe('Hampus deleted a webhook.');
	});
});
