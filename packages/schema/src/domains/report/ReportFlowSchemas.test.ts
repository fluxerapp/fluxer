// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ReportFlowMessageSubmissionRequest,
	ReportFlowQuery,
	ReportFlowResponse,
	ReportFlowUserSubmissionRequest,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {describe, expect, it} from 'vitest';

const userFlowResponse = {
	target_type: 'user',
	surface: 'in_app',
	revision_hash: '9c41d2a07e5b18f3',
	locale: 'en-US',
	start_screen_id: 'profile_intro',
	guidelines_url: 'https://fluxer.app/guidelines',
	screens: [
		{
			id: 'profile_intro',
			title: 'Report profile',
			subtitle: "Tell us what's wrong with this profile. They won't be told who reported them.",
			urgent: false,
			options: [
				{
					id: 'learn_more',
					label: 'Read the Fluxer Community Guidelines',
					outcome: {
						type: 'link',
						screen_id: null,
						reason: null,
						notice_id: null,
						url: 'https://fluxer.app/guidelines',
					},
				},
			],
			options_heading: 'Learn more',
			checklist: null,
			next_screen_id: 'profile_parts',
		},
		{
			id: 'profile_parts',
			title: 'Which parts of their profile are a problem?',
			subtitle: 'Pick as many as you need.',
			urgent: false,
			options: [],
			options_heading: null,
			checklist: {
				items: [
					{id: 'photo', label: 'Pictures', description: 'What they use as avatar and banner'},
					{id: 'name', label: 'Names', description: 'Username, display name or community nickname'},
					{id: 'descriptors', label: 'Profile text', description: 'Bio, custom status or pronouns'},
				],
				min_checked: 1,
				outcome: {type: 'screen', screen_id: 'root_user', reason: null, notice_id: null, url: null},
			},
			next_screen_id: null,
		},
	],
	notices: [
		{
			id: 'need_more_info',
			title: "We can't act on this yet",
			body: "To review this, we need a message where they say how old they are or make it clear they're under Fluxer's minimum age. Report that message instead.",
		},
	],
};

const messageSubmission = {
	channel_id: '1427410010101010101',
	message_id: '1427410099999999999',
	revision_hash: '6b1f0c9e2a7d4413',
	locale: 'de',
	steps: [
		{screen_id: 'root_message', option_id: 'abuse'},
		{screen_id: 'abuse', option_id: 'sexual'},
		{screen_id: 'sexual', option_id: 'minor_sexual'},
		{screen_id: 'minor_sexual', option_id: 'csam'},
	],
};

const userSubmission = {
	user_id: '1427410010101010102',
	guild_id: '1427410010101010103',
	revision_hash: '9c41d2a07e5b18f3',
	locale: 'en-US',
	steps: [
		{screen_id: 'profile_intro'},
		{screen_id: 'profile_parts', item_ids: ['photo']},
		{screen_id: 'root_user', option_id: 'abuse'},
		{screen_id: 'profile_abuse', option_id: 'harassment'},
	],
};

describe('ReportFlowResponse', () => {
	it('parses the user flow example', () => {
		expect(ReportFlowResponse.parse(userFlowResponse)).toEqual(userFlowResponse);
	});

	it('accepts reasons and locales it does not know', () => {
		const response = {
			...userFlowResponse,
			locale: 'tlh-Latn',
			screens: [
				{
					...userFlowResponse.screens[0],
					options: [
						{
							id: 'future_option',
							label: 'A future option',
							outcome: {type: 'submit', screen_id: null, reason: 'reason_added_later', notice_id: null, url: null},
						},
					],
				},
			],
		};
		expect(ReportFlowResponse.safeParse(response).success).toBe(true);
	});

	it('rejects an unknown outcome type', () => {
		const response = {
			...userFlowResponse,
			screens: [
				{
					...userFlowResponse.screens[0],
					options: [
						{
							id: 'x',
							label: 'X',
							outcome: {type: 'redirect', screen_id: null, reason: null, notice_id: null, url: null},
						},
					],
				},
			],
		};
		expect(ReportFlowResponse.safeParse(response).success).toBe(false);
	});
});

describe('ReportFlowQuery', () => {
	it('defaults the surface to in_app', () => {
		expect(ReportFlowQuery.parse({})).toEqual({surface: 'in_app'});
	});

	it('accepts any locale tag up to 35 characters', () => {
		expect(ReportFlowQuery.parse({surface: 'dsa', locale: 'xx-YY'})).toEqual({surface: 'dsa', locale: 'xx-YY'});
		expect(ReportFlowQuery.safeParse({locale: 'a'.repeat(36)}).success).toBe(false);
	});

	it('rejects an unknown surface', () => {
		expect(ReportFlowQuery.safeParse({surface: 'nope'}).success).toBe(false);
	});
});

describe('ReportFlowMessageSubmissionRequest', () => {
	it('parses the message submission example', () => {
		const parsed = ReportFlowMessageSubmissionRequest.parse(messageSubmission);
		expect(parsed.steps).toEqual(messageSubmission.steps);
		expect(parsed.revision_hash).toBe('6b1f0c9e2a7d4413');
		expect(parsed.locale).toBe('de');
	});

	it('accepts a checklist step and an unknown locale string', () => {
		const body = {
			...messageSubmission,
			locale: 'zz-Unknown',
			steps: [
				{screen_id: 'root_message', option_id: 'private_info'},
				{screen_id: 'private_info', item_ids: ['email', 'phone']},
			],
		};
		expect(ReportFlowMessageSubmissionRequest.safeParse(body).success).toBe(true);
	});

	it('accepts 16 steps and rejects 17', () => {
		const step = {screen_id: 'root_message', option_id: 'abuse'};
		const sixteen = {...messageSubmission, steps: Array.from({length: 16}, () => step)};
		const seventeen = {...messageSubmission, steps: Array.from({length: 17}, () => step)};
		expect(ReportFlowMessageSubmissionRequest.safeParse(sixteen).success).toBe(true);
		expect(ReportFlowMessageSubmissionRequest.safeParse(seventeen).success).toBe(false);
	});

	it('rejects an empty walk', () => {
		expect(ReportFlowMessageSubmissionRequest.safeParse({...messageSubmission, steps: []}).success).toBe(false);
	});

	it('accepts 32 item ids and rejects 33 or none', () => {
		const items = (count: number) => Array.from({length: count}, (_, index) => `item_${index}`);
		const body = (itemIds: Array<string>) => ({
			...messageSubmission,
			steps: [{screen_id: 'private_info', item_ids: itemIds}],
		});
		expect(ReportFlowMessageSubmissionRequest.safeParse(body(items(32))).success).toBe(true);
		expect(ReportFlowMessageSubmissionRequest.safeParse(body(items(33))).success).toBe(false);
		expect(ReportFlowMessageSubmissionRequest.safeParse(body([])).success).toBe(false);
	});

	it('accepts a 48-character id and rejects a 49-character id', () => {
		const body = (id: string) => ({...messageSubmission, steps: [{screen_id: id, option_id: 'abuse'}]});
		expect(ReportFlowMessageSubmissionRequest.safeParse(body('a'.repeat(48))).success).toBe(true);
		expect(ReportFlowMessageSubmissionRequest.safeParse(body('a'.repeat(49))).success).toBe(false);
		expect(
			ReportFlowMessageSubmissionRequest.safeParse({
				...messageSubmission,
				steps: [{screen_id: 'root_message', option_id: 'b'.repeat(49)}],
			}).success,
		).toBe(false);
		expect(
			ReportFlowMessageSubmissionRequest.safeParse({
				...messageSubmission,
				steps: [{screen_id: 'private_info', item_ids: ['c'.repeat(49)]}],
			}).success,
		).toBe(false);
	});

	it('requires the revision hash and the message ids', () => {
		const {revision_hash: _revisionHash, ...withoutHash} = messageSubmission;
		const {message_id: _messageId, ...withoutMessage} = messageSubmission;
		expect(ReportFlowMessageSubmissionRequest.safeParse(withoutHash).success).toBe(false);
		expect(ReportFlowMessageSubmissionRequest.safeParse(withoutMessage).success).toBe(false);
	});
});

describe('ReportFlowUserSubmissionRequest', () => {
	it('parses the user submission example with info steps', () => {
		const parsed = ReportFlowUserSubmissionRequest.parse(userSubmission);
		expect(parsed.steps).toEqual(userSubmission.steps);
		expect(parsed.locale).toBe('en-US');
	});

	it('makes the guild and locale optional', () => {
		const {guild_id: _guildId, locale: _locale, ...body} = userSubmission;
		expect(ReportFlowUserSubmissionRequest.safeParse(body).success).toBe(true);
	});
});
