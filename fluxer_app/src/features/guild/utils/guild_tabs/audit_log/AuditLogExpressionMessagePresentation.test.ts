// SPDX-License-Identifier: AGPL-3.0-or-later

import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import type {AuditLogPresentationContext} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
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

const OTHER_USER_ID = '1400000000000000100';
const EMOJI_ID = '1400000000000000200';
const STICKER_ID = '1400000000000000300';
const CHANNEL_ID = '1400000000000000400';
const MESSAGE_ID = '1400000000000000500';
const USER_NAMES = {[TEST_ACTOR_ID]: 'Hampus', [OTHER_USER_ID]: 'ender'};
const EXPRESSION_MESSAGE_ACTION_TYPES = [
	AuditLogActionType.EMOJI_CREATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.EMOJI_DELETE,
	AuditLogActionType.STICKER_CREATE,
	AuditLogActionType.STICKER_UPDATE,
	AuditLogActionType.STICKER_DELETE,
	AuditLogActionType.MESSAGE_DELETE,
	AuditLogActionType.MESSAGE_BULK_DELETE,
	AuditLogActionType.MESSAGE_PIN,
	AuditLogActionType.MESSAGE_UNPIN,
];

function present(fixture: EntryFixture, context: AuditLogPresentationContext = fakeContext()) {
	const presentation = presentAuditLogEntry(makeEntry(fixture), context);
	return {...resultToText(presentation, USER_NAMES), expandable: presentation.expandable};
}

function summaryOnly(summary: string) {
	return {summary, rows: [], blocks: [], expandable: false};
}

function emojiSnapshot(side: 'old_value' | 'new_value', name: unknown, creatorId: unknown = TEST_ACTOR_ID) {
	return [
		{key: 'emoji_id', [side]: EMOJI_ID},
		{key: 'name', [side]: name},
		{key: 'animated', [side]: false},
		{key: 'creator_id', [side]: creatorId},
	];
}

function stickerSnapshot(
	side: 'old_value' | 'new_value',
	name: unknown,
	description: unknown,
	creatorId: unknown = TEST_ACTOR_ID,
) {
	return [
		{key: 'sticker_id', [side]: STICKER_ID},
		{key: 'name', [side]: name},
		{key: 'description', [side]: description},
		{key: 'animated', [side]: true},
		{key: 'creator_id', [side]: creatorId},
	];
}

describe('presentEmojiCreate', () => {
	it('names the added emoji from the snapshot and shows its image by id', () => {
		const fixture = {
			action_type: AuditLogActionType.EMOJI_CREATE,
			target_id: EMOJI_ID,
			changes: emojiSnapshot('new_value', 'blobcat'),
		};
		expect(present(fixture)).toEqual(summaryOnly('Hampus added the emoji :blobcat:.'));
		expect(presentAuditLogEntry(makeEntry(fixture), fakeContext()).summary.values.emoji).toEqual({
			kind: 'emoji',
			id: EMOJI_ID,
			name: 'blobcat',
		});
	});

	it('uses the unnamed summary when the name is missing, blank or not a string', () => {
		for (const name of [undefined, null, '   ', 42]) {
			expect(
				present({
					action_type: AuditLogActionType.EMOJI_CREATE,
					target_id: EMOJI_ID,
					changes: emojiSnapshot('new_value', name),
				}),
			).toEqual(summaryOnly('Hampus added an emoji.'));
		}
		expect(present({action_type: AuditLogActionType.EMOJI_CREATE, target_id: EMOJI_ID})).toEqual(
			summaryOnly('Hampus added an emoji.'),
		);
	});

	it('keeps the name but drops the image id when the target is not a snowflake', () => {
		const presentation = presentAuditLogEntry(
			makeEntry({
				action_type: AuditLogActionType.EMOJI_CREATE,
				target_id: 'blobcat',
				changes: emojiSnapshot('new_value', 'blobcat'),
			}),
			fakeContext(),
		);
		expect(presentation.summary.values.emoji).toEqual({kind: 'emoji', id: null, name: 'blobcat'});
	});

	it('is expandable only for the Reason block', () => {
		expect(
			present({
				action_type: AuditLogActionType.EMOJI_CREATE,
				target_id: EMOJI_ID,
				reason: 'Event%20emoji',
				changes: emojiSnapshot('new_value', 'blobcat'),
			}),
		).toEqual({
			summary: 'Hampus added the emoji :blobcat:.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Event emoji'}],
			expandable: true,
		});
	});
});

describe('presentEmojiUpdate', () => {
	it('renames from the old name without an image to the new emoji', () => {
		const fixture = {
			action_type: AuditLogActionType.EMOJI_UPDATE,
			target_id: EMOJI_ID,
			changes: [{key: 'name', old_value: 'blob', new_value: 'blobcat'}],
		};
		expect(present(fixture, fakeContext({emojis: {[EMOJI_ID]: 'ignored'}}))).toEqual(
			summaryOnly('Hampus renamed the emoji :blob: to :blobcat:.'),
		);
		const {values} = presentAuditLogEntry(makeEntry(fixture), fakeContext()).summary;
		expect(values.oldEmoji).toEqual({kind: 'emoji', id: null, name: 'blob'});
		expect(values.newEmoji).toEqual({kind: 'emoji', id: EMOJI_ID, name: 'blobcat'});
	});

	it('names a legacy no-op from the emoji store', () => {
		expect(
			present(
				{action_type: AuditLogActionType.EMOJI_UPDATE, target_id: EMOJI_ID},
				fakeContext({emojis: {[EMOJI_ID]: 'blobcat'}}),
			),
		).toEqual(summaryOnly('Hampus updated the emoji :blobcat:.'));
	});

	it('uses the unnamed summary for a legacy no-op the emoji store does not know', () => {
		expect(present({action_type: AuditLogActionType.EMOJI_UPDATE, target_id: EMOJI_ID})).toEqual(
			summaryOnly('Hampus updated an emoji.'),
		);
		expect(
			present(
				{action_type: AuditLogActionType.EMOJI_UPDATE, target_id: null},
				fakeContext({emojis: {[EMOJI_ID]: 'blobcat'}}),
			),
		).toEqual(summaryOnly('Hampus updated an emoji.'));
	});

	it('treats names that are equal after trimming as no rename', () => {
		expect(
			present({
				action_type: AuditLogActionType.EMOJI_UPDATE,
				target_id: EMOJI_ID,
				changes: [{key: 'name', old_value: 'blobcat ', new_value: 'blobcat'}],
			}),
		).toEqual(summaryOnly('Hampus updated the emoji :blobcat:.'));
	});

	it('names the emoji from the recorded new name when the old name is unreadable', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.EMOJI_UPDATE,
					target_id: EMOJI_ID,
					changes: [{key: 'name', old_value: null, new_value: 'blobcat'}],
				},
				fakeContext({emojis: {[EMOJI_ID]: 'stale'}}),
			),
		).toEqual(summaryOnly('Hampus updated the emoji :blobcat:.'));
	});
});

describe('presentEmojiDelete', () => {
	it('shows no rows when the actor uploaded the emoji', () => {
		const fixture = {
			action_type: AuditLogActionType.EMOJI_DELETE,
			target_id: EMOJI_ID,
			changes: emojiSnapshot('old_value', 'blobcat'),
		};
		expect(present(fixture)).toEqual(summaryOnly('Hampus deleted the emoji :blobcat:.'));
		expect(presentAuditLogEntry(makeEntry(fixture), fakeContext()).summary.values.emoji).toEqual({
			kind: 'emoji',
			id: null,
			name: 'blobcat',
		});
	});

	it('names the uploader when someone else uploaded the emoji', () => {
		expect(
			present({
				action_type: AuditLogActionType.EMOJI_DELETE,
				target_id: EMOJI_ID,
				changes: emojiSnapshot('old_value', 'blobcat', OTHER_USER_ID),
			}),
		).toEqual({
			summary: 'Hampus deleted the emoji :blobcat:.',
			rows: ['~ The emoji was uploaded by ender.'],
			blocks: [],
			expandable: true,
		});
	});

	it('names the uploader when the system deleted the emoji', () => {
		expect(
			present({
				action_type: AuditLogActionType.EMOJI_DELETE,
				user_id: '0',
				target_id: EMOJI_ID,
				changes: emojiSnapshot('old_value', 'blobcat', TEST_ACTOR_ID),
			}),
		).toEqual({
			summary: 'System deleted the emoji :blobcat:.',
			rows: ['~ The emoji was uploaded by Hampus.'],
			blocks: [],
			expandable: true,
		});
	});

	it('omits the uploader row when the uploader is not a snowflake, and the name when it is missing', () => {
		for (const creatorId of [null, 1400, 'ender', '']) {
			expect(
				present({
					action_type: AuditLogActionType.EMOJI_DELETE,
					target_id: EMOJI_ID,
					changes: emojiSnapshot('old_value', undefined, creatorId),
				}),
			).toEqual(summaryOnly('Hampus deleted an emoji.'));
		}
	});
});

describe('presentStickerCreate', () => {
	it('sets the description when one was given', () => {
		expect(
			present({
				action_type: AuditLogActionType.STICKER_CREATE,
				target_id: STICKER_ID,
				changes: stickerSnapshot('new_value', 'Wave', 'hello there'),
			}),
		).toEqual({
			summary: 'Hampus added the sticker Wave.',
			rows: ['~ Set the description to hello there.'],
			blocks: [],
			expandable: true,
		});
	});

	it('shows no rows without a description', () => {
		for (const description of [null, '', '  ', 7]) {
			expect(
				present({
					action_type: AuditLogActionType.STICKER_CREATE,
					target_id: STICKER_ID,
					changes: stickerSnapshot('new_value', 'Wave', description),
				}),
			).toEqual(summaryOnly('Hampus added the sticker Wave.'));
		}
	});

	it('uses the unnamed summary when the name is missing', () => {
		expect(
			present({
				action_type: AuditLogActionType.STICKER_CREATE,
				target_id: STICKER_ID,
				changes: stickerSnapshot('new_value', null, 'hello there'),
			}),
		).toEqual({
			summary: 'Hampus added a sticker.',
			rows: ['~ Set the description to hello there.'],
			blocks: [],
			expandable: true,
		});
	});
});

describe('presentStickerUpdate', () => {
	const context = fakeContext({stickers: {[STICKER_ID]: 'Wave'}});

	it('renames with no rows when only the name changed', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [{key: 'name', old_value: 'Wave', new_value: 'Hello'}],
				},
				context,
			),
		).toEqual(summaryOnly('Hampus renamed the sticker Wave to Hello.'));
	});

	it('adds a description', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [{key: 'description', old_value: null, new_value: 'hello there'}],
				},
				context,
			),
		).toEqual({
			summary: 'Hampus updated the sticker Wave.',
			rows: ['+ Set the description to hello there.'],
			blocks: [],
			expandable: true,
		});
	});

	it('changes a description', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [{key: 'description', old_value: 'hi', new_value: 'hello there'}],
				},
				context,
			),
		).toEqual({
			summary: 'Hampus updated the sticker Wave.',
			rows: ['~ Changed the description from hi to hello there.'],
			blocks: [],
			expandable: true,
		});
	});

	it('removes a description, treating blank as removed', () => {
		for (const newValue of [null, '', '   ']) {
			expect(
				present(
					{
						action_type: AuditLogActionType.STICKER_UPDATE,
						target_id: STICKER_ID,
						changes: [{key: 'description', old_value: 'hello there', new_value: newValue}],
					},
					context,
				),
			).toEqual({
				summary: 'Hampus updated the sticker Wave.',
				rows: ['- Removed the description hello there.'],
				blocks: [],
				expandable: true,
			});
		}
	});

	it('lists the name before the description and names the sticker from the new name', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [
						{key: 'description', old_value: 'hi', new_value: 'hello there'},
						{key: 'name', old_value: 'Wave', new_value: 'Hello'},
					],
				},
				context,
			),
		).toEqual({
			summary: 'Hampus updated the sticker Hello.',
			rows: ['~ Changed the name from Wave to Hello.', '~ Changed the description from hi to hello there.'],
			blocks: [],
			expandable: true,
		});
	});

	it('names the sticker from the sticker store when the name did not change', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [{key: 'description', old_value: 'hi', new_value: 'hello there'}],
				},
				fakeContext({stickers: {[STICKER_ID]: 'Stored name'}}),
			),
		).toEqual({
			summary: 'Hampus updated the sticker Stored name.',
			rows: ['~ Changed the description from hi to hello there.'],
			blocks: [],
			expandable: true,
		});
	});

	it('uses the unnamed summary when the sticker name is unknown', () => {
		expect(
			present({
				action_type: AuditLogActionType.STICKER_UPDATE,
				target_id: STICKER_ID,
				changes: [{key: 'description', old_value: null, new_value: 'hello there'}],
			}),
		).toEqual({
			summary: 'Hampus updated a sticker.',
			rows: ['+ Set the description to hello there.'],
			blocks: [],
			expandable: true,
		});
	});

	it('shows the summary alone for a legacy no-op or unchanged values', () => {
		expect(present({action_type: AuditLogActionType.STICKER_UPDATE, target_id: STICKER_ID})).toEqual(
			summaryOnly('Hampus updated a sticker.'),
		);
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [
						{key: 'description', old_value: '', new_value: null},
						{key: 'tags', old_value: ['a'], new_value: ['b']},
						{key: 'animated', old_value: false, new_value: true},
					],
				},
				context,
			),
		).toEqual(summaryOnly('Hampus updated the sticker Wave.'));
	});

	it('omits the name row when either name is unreadable', () => {
		expect(
			present(
				{
					action_type: AuditLogActionType.STICKER_UPDATE,
					target_id: STICKER_ID,
					changes: [
						{key: 'name', old_value: 5, new_value: 'Hello'},
						{key: 'description', old_value: 'hi', new_value: 'hello there'},
					],
				},
				context,
			),
		).toEqual({
			summary: 'Hampus updated the sticker Hello.',
			rows: ['~ Changed the description from hi to hello there.'],
			blocks: [],
			expandable: true,
		});
	});
});

describe('presentStickerDelete', () => {
	it('shows no rows when the actor uploaded the sticker', () => {
		expect(
			present({
				action_type: AuditLogActionType.STICKER_DELETE,
				target_id: STICKER_ID,
				changes: stickerSnapshot('old_value', 'Wave', 'hello there'),
			}),
		).toEqual(summaryOnly('Hampus deleted the sticker Wave.'));
	});

	it('names the uploader when someone else uploaded the sticker', () => {
		expect(
			present({
				action_type: AuditLogActionType.STICKER_DELETE,
				target_id: STICKER_ID,
				changes: stickerSnapshot('old_value', 'Wave', null, OTHER_USER_ID),
			}),
		).toEqual({
			summary: 'Hampus deleted the sticker Wave.',
			rows: ['~ The sticker was uploaded by ender.'],
			blocks: [],
			expandable: true,
		});
	});

	it('uses the unnamed summary when the name is missing', () => {
		expect(present({action_type: AuditLogActionType.STICKER_DELETE, target_id: STICKER_ID})).toEqual(
			summaryOnly('Hampus deleted a sticker.'),
		);
	});
});

describe('presentMessageDelete, presentMessagePin and presentMessageUnpin', () => {
	const cases = [
		{
			actionType: AuditLogActionType.MESSAGE_DELETE,
			inChannel: `Hampus deleted a message in #${CHANNEL_ID}.`,
			withoutChannel: 'Hampus deleted a message.',
		},
		{
			actionType: AuditLogActionType.MESSAGE_PIN,
			inChannel: `Hampus pinned a message in #${CHANNEL_ID}.`,
			withoutChannel: 'Hampus pinned a message.',
		},
		{
			actionType: AuditLogActionType.MESSAGE_UNPIN,
			inChannel: `Hampus unpinned a message in #${CHANNEL_ID}.`,
			withoutChannel: 'Hampus unpinned a message.',
		},
	];

	for (const {actionType, inChannel, withoutChannel} of cases) {
		describe(AuditLogActionType[actionType], () => {
			it('names the channel', () => {
				const fixture = {
					action_type: actionType,
					target_id: MESSAGE_ID,
					options: {channel_id: CHANNEL_ID, message_id: MESSAGE_ID},
				};
				expect(present(fixture)).toEqual(summaryOnly(inChannel));
				expect(presentAuditLogEntry(makeEntry(fixture), fakeContext()).summary.values.channel).toEqual({
					kind: 'channel',
					id: CHANNEL_ID,
					recordedName: null,
					fallback: 'channel',
				});
			});

			it('leaves the channel out when it is missing or not a snowflake', () => {
				for (const options of [undefined, {}, {channel_id: null}, {channel_id: 1400}, {channel_id: 'general'}]) {
					expect(present({action_type: actionType, target_id: MESSAGE_ID, options})).toEqual(
						summaryOnly(withoutChannel),
					);
				}
			});

			it('is expandable only for the Reason block', () => {
				expect(
					present({
						action_type: actionType,
						target_id: MESSAGE_ID,
						reason: 'Off topic',
						options: {channel_id: CHANNEL_ID},
					}),
				).toEqual({summary: inChannel, rows: [], blocks: [{kind: 'reason', text: 'Off topic'}], expandable: true});
			});
		});
	}
});

describe('presentMessageBulkDelete', () => {
	it('counts one message in a channel', () => {
		expect(
			present({action_type: AuditLogActionType.MESSAGE_BULK_DELETE, options: {channel_id: CHANNEL_ID, count: 1}}),
		).toEqual(summaryOnly(`Hampus deleted 1 message in #${CHANNEL_ID}.`));
	});

	it('counts five messages in a channel, from a REST number or a dispatch string', () => {
		for (const count of [5, '5']) {
			expect(
				present({action_type: AuditLogActionType.MESSAGE_BULK_DELETE, options: {channel_id: CHANNEL_ID, count}}),
			).toEqual(summaryOnly(`Hampus deleted 5 messages in #${CHANNEL_ID}.`));
		}
	});

	it('says some messages when the count is missing or not a positive whole number', () => {
		for (const count of [undefined, null, 0, -3, 2.5, 'many', '']) {
			expect(
				present({action_type: AuditLogActionType.MESSAGE_BULK_DELETE, options: {channel_id: CHANNEL_ID, count}}),
			).toEqual(summaryOnly(`Hampus deleted some messages in #${CHANNEL_ID}.`));
		}
	});

	it('leaves the channel out when it is missing', () => {
		expect(present({action_type: AuditLogActionType.MESSAGE_BULK_DELETE, options: {count: 5}})).toEqual(
			summaryOnly('Hampus deleted 5 messages.'),
		);
		expect(present({action_type: AuditLogActionType.MESSAGE_BULK_DELETE, options: {count: 1}})).toEqual(
			summaryOnly('Hampus deleted 1 message.'),
		);
		expect(present({action_type: AuditLogActionType.MESSAGE_BULK_DELETE})).toEqual(
			summaryOnly('Hampus deleted some messages.'),
		);
	});

	it('passes the count through as a number for the plural', () => {
		const presentation = presentAuditLogEntry(
			makeEntry({action_type: AuditLogActionType.MESSAGE_BULK_DELETE, options: {channel_id: CHANNEL_ID, count: '12'}}),
			fakeContext(),
		);
		expect(presentation.summary.values.count).toBe(12);
	});

	it('is expandable only for the Reason block', () => {
		expect(
			present({
				action_type: AuditLogActionType.MESSAGE_BULK_DELETE,
				reason: 'Spam wave',
				options: {channel_id: CHANNEL_ID, count: 40},
			}),
		).toEqual({
			summary: `Hampus deleted 40 messages in #${CHANNEL_ID}.`,
			rows: [],
			blocks: [{kind: 'reason', text: 'Spam wave'}],
			expandable: true,
		});
	});
});

describe('junk input', () => {
	it('never throws and renders only summaries for every expression and message action', () => {
		const junkFixtures: Array<Omit<EntryFixture, 'action_type'>> = [
			{user_id: null, target_id: null},
			{
				target_id: 'not-a-snowflake',
				options: {channel_id: {}, count: [], message_id: 5},
				changes: [
					{key: 'name', old_value: {}, new_value: []},
					{key: 'description', old_value: 12, new_value: false},
					{key: 'creator_id', old_value: 1400, new_value: null},
				],
			},
		];
		for (const actionType of EXPRESSION_MESSAGE_ACTION_TYPES) {
			for (const fixture of junkFixtures) {
				const result = present({...fixture, action_type: actionType});
				expect(result.rows).toEqual([]);
				expect(result.blocks).toEqual([]);
				expect(result.expandable).toBe(false);
			}
		}
	});
});
