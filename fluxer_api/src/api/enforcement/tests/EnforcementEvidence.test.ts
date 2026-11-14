// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {createStaff, decisionBody, getDecision, suspend} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Enforcement evidence', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		staff = await createStaff(harness);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function messageWithAttachment() {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const {json: message} = await sendMessageWithAttachments(
			harness,
			target.token,
			systemChannel.id,
			{content: 'look at this', attachments: [{id: 0, filename: 'evidence.png'}]},
			[{index: 0, filename: 'evidence.png', data: loadFixture('yeah.png')}],
		);
		return {target, evidence: [{channel_id: systemChannel.id, message_id: message.id}]};
	}

	test('attachments are stored as details without the files', async () => {
		const {target, evidence} = await messageWithAttachment();
		const decisionId = await suspend(harness, staff, target.userId, decisionBody({evidence}));
		const stored = await new EnforcementDecisionRepository().getEvidence(BigInt(decisionId));
		const [message] = stored!.messages!;
		expect(message!.content).toBe('look at this');
		expect(message!.attachments).toBeNull();
		expect(message!.missing_attachments).toEqual([
			expect.objectContaining({
				filename: 'evidence.png',
				content_type: 'image/png',
				content_hash: null,
				placeholder: null,
			}),
		]);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.evidence).toEqual([
			expect.objectContaining({
				content: 'look at this',
				attachments: [expect.objectContaining({filename: 'evidence.png', content_type: 'image/png'})],
				attachment_count: 1,
			}),
		]);
	});

	test('restricted evidence keeps only the message, channel, time and attachment count', async () => {
		const {target, evidence} = await messageWithAttachment();
		const decisionId = await suspend(
			harness,
			staff,
			target.userId,
			decisionBody({
				reason_code: DeletionReasons.CHILD_SAFETY_VIOLATION,
				statement: undefined,
				evidence,
				confirmations: ['evidence_preserved', 'reports_filed_or_not_required'],
			}),
			{duration_hours: 0},
		);
		const stored = await new EnforcementDecisionRepository().getEvidence(BigInt(decisionId));
		const [message] = stored!.messages!;
		expect(message).toMatchObject({
			message_id: BigInt(evidence[0]!.message_id),
			channel_id: BigInt(evidence[0]!.channel_id),
			content: null,
			author_id: null,
			author_username: '',
			author_avatar_hash: null,
			attachments: null,
			embeds: null,
			sticker_items: null,
		});
		expect(message!.missing_attachments).toHaveLength(1);
		expect(message!.missing_attachments![0]).toMatchObject({filename: '', content_type: '', size: 0n});
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.notice.facts).toEqual([
			'We found content or activity on your account that breaks our rules on protecting children.',
		]);
		expect(decision.notice.statement).toBeNull();
		expect(decision.notice_text).not.toContain('look at this');
		expect(decision.notice_text).not.toContain('you sent in');
	});

	test('evidence content is hidden without message lookup and report IDs without report view', async () => {
		const {target, evidence} = await messageWithAttachment();
		const decisionId = await suspend(harness, staff, target.userId, decisionBody({evidence}));
		const viewer = await createStaff(harness, [AdminACLs.ENFORCEMENT_VIEW]);
		const decision = await getDecision(harness, viewer, decisionId);
		expect(decision.evidence).toEqual([
			expect.objectContaining({content: null, attachments: null, attachment_count: 1}),
		]);
		expect(decision.report_ids).toEqual([]);
	});
});
