// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/AdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAttachmentID,
	createChannelID,
	createMessageID,
	createReportID,
	createUserID,
	type ReportID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import {
	IAR_SUBMISSION_COLUMNS,
	type IARMessageContextRow,
	type IARSubmissionRow,
} from '@app/api/database/types/ReportTypes';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {getReportSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {serializeReportProfileSnapshot} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import {ms} from 'itty-time';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const REPORTS_BUCKET = Config.s3.buckets.reports;
const AUDIT_REASON = 'Erasure request 77';
let sequence = 1_498_000_000_000_000_000n;

function nextId(): bigint {
	sequence += 1n;
	return sequence;
}

interface Evidence {
	channelId: bigint;
	attachmentId: bigint;
	filename: string;
}

interface SeededReport {
	reportId: ReportID;
	reporterId: bigint;
	channelId: bigint;
	messageId: bigint;
	attachmentKey: string;
	profileKey: string;
}

function evidenceKey(evidence: Evidence): string {
	return makeAttachmentCdnKey(createChannelID(evidence.channelId), evidence.attachmentId, evidence.filename);
}

function contextRow(evidence: Evidence, authorId: bigint): IARMessageContextRow {
	return {
		message_id: nextId(),
		channel_id: evidence.channelId,
		author_id: authorId,
		webhook_id: null,
		author_username: 'reported',
		author_discriminator: 1,
		author_avatar_hash: null,
		content: 'the reported message',
		timestamp: new Date(),
		edited_timestamp: null,
		type: 0,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: [
			{
				attachment_id: createAttachmentID(evidence.attachmentId),
				filename: evidence.filename,
				size: 1024n,
				title: null,
				description: null,
				width: 64,
				height: 64,
				content_type: 'image/png',
				content_hash: null,
				placeholder: null,
				flags: 0,
				duration: null,
				nsfw: false,
				waveform: null,
			},
		],
		embeds: null,
		sticker_items: null,
	};
}

describe('DELETE /admin/reports/:report_id', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let repository: ReportRepository;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			AdminACLs.REPORT_VIEW,
			AdminACLs.REPORT_DELETE,
		]);
		repository = new ReportRepository();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function seedMessageReport(
		evidence: Evidence,
		options: {legalHoldUntil?: Date | null} = {},
	): Promise<SeededReport> {
		const id = nextId();
		const reportId = createReportID(id);
		const reporterId = nextId();
		const reportedUserId = nextId();
		const messageId = nextId();
		const attachmentKey = evidenceKey(evidence);
		const profileKey = `reports/${reportId}/profile/user_avatar/abc${id}`;
		await harness.storageService.uploadObject({bucket: REPORTS_BUCKET, key: attachmentKey, body: new Uint8Array([1])});
		await harness.storageService.uploadObject({bucket: REPORTS_BUCKET, key: profileKey, body: new Uint8Array([2])});
		const uploadedAt = new Date(Date.now() - ms('1 hour'));
		harness.storageService.setObjectLastModified(REPORTS_BUCKET, attachmentKey, uploadedAt);
		harness.storageService.setObjectLastModified(REPORTS_BUCKET, profileKey, uploadedAt);
		const report = await repository.createReport({
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: id,
			reporter_id: reporterId,
			reported_at: new Date(Date.now() - ms('1 day')),
			status: 1,
			resolved_at: new Date(),
			report_type: 0,
			category: 'spam',
			reported_user_id: reportedUserId,
			reported_channel_id: evidence.channelId,
			reported_message_id: messageId,
			message_context: [contextRow(evidence, reportedUserId)],
			reported_profile_snapshot: serializeReportProfileSnapshot({
				captured_at: new Date().toISOString(),
				user: {
					id: reportedUserId.toString(),
					username: 'reported',
					discriminator: 1,
					global_name: null,
					bio: null,
					pronouns: null,
					avatar: {hash: `abc${id}`, key: profileKey},
					banner: null,
				},
				member: null,
				guild: null,
			}),
			legal_hold_until: options.legalHoldUntil ?? null,
			legal_hold_reason: options.legalHoldUntil ? 'Preservation request' : null,
		} as IARSubmissionRow);
		await getReportSearchService()!.indexReport(report);
		const reserved = await repository.reserveMessageReportByReporter({
			reporter_id: reporterId,
			channel_id: evidence.channelId,
			message_id: messageId,
			report_id: id,
			reported_at: report.reportedAt,
		});
		expect(reserved).toBe(true);
		return {reportId, reporterId, channelId: evidence.channelId, messageId, attachmentKey, profileKey};
	}

	function newEvidence(): Evidence {
		return {channelId: nextId(), attachmentId: nextId(), filename: 'evidence.png'};
	}

	async function indexedIds(): Promise<Array<string>> {
		const result = await getReportSearchService()!.searchReports('', {}, {limit: 1000});
		return result.hits.map((hit) => hit.id);
	}

	function reservationIsFree(seeded: SeededReport): Promise<boolean> {
		return repository.reserveMessageReportByReporter({
			reporter_id: createUserID(seeded.reporterId),
			channel_id: createChannelID(seeded.channelId),
			message_id: createMessageID(seeded.messageId),
			report_id: createReportID(nextId()),
			reported_at: new Date(),
		});
	}

	function deleteRequest(account: TestAccount, reportId: ReportID | string) {
		return createBuilder<{code?: string}>(harness, account.token)
			.delete(`/admin/reports/${reportId}`)
			.header('X-Audit-Log-Reason', AUDIT_REASON);
	}

	async function auditEntries(action: string): Promise<Array<AdminAuditLog>> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(10000);
		return logs.filter((log) => log.action === action);
	}

	test('deletes the record, message context, evidence, search entry and reservation, and records the reason', async () => {
		const seeded = await seedMessageReport(newEvidence());
		const report = await repository.getReport(seeded.reportId);
		expect(report?.messageContext).toHaveLength(1);
		expect(await indexedIds()).toContain(seeded.reportId.toString());

		await deleteRequest(admin, seeded.reportId).expect(HTTP_STATUS.NO_CONTENT).execute();

		expect(await repository.getReport(seeded.reportId)).toBeNull();
		expect(harness.storageService.hasObject(REPORTS_BUCKET, seeded.attachmentKey)).toBe(false);
		expect(harness.storageService.hasObject(REPORTS_BUCKET, seeded.profileKey)).toBe(false);
		expect(await indexedIds()).not.toContain(seeded.reportId.toString());
		expect(await reservationIsFree(seeded)).toBe(true);
		const [entry, ...rest] = await auditEntries('delete_report');
		expect(rest).toEqual([]);
		expect(entry?.adminUserId.toString()).toBe(admin.userId);
		expect(entry?.targetType).toBe('report');
		expect(entry?.targetId.toString()).toBe(seeded.reportId.toString());
		expect(entry?.auditLogReason).toBe(AUDIT_REASON);
		expect(Object.fromEntries(entry?.metadata ?? [])).toEqual({
			report_id: seeded.reportId.toString(),
			report_type: '0',
			status: '1',
			objects_deleted: '2',
			shared_objects_kept: '0',
		});
	});

	test('keeps an evidence copy another report still uses', async () => {
		const shared = newEvidence();
		const deleted = await seedMessageReport(shared);
		const other = await seedMessageReport(shared);

		await deleteRequest(admin, deleted.reportId).expect(HTTP_STATUS.NO_CONTENT).execute();

		expect(await repository.getReport(deleted.reportId)).toBeNull();
		expect(harness.storageService.hasObject(REPORTS_BUCKET, deleted.profileKey)).toBe(false);
		expect(harness.storageService.hasObject(REPORTS_BUCKET, evidenceKey(shared))).toBe(true);
		expect(await repository.getReport(other.reportId)).not.toBeNull();
		expect(harness.storageService.hasObject(REPORTS_BUCKET, other.profileKey)).toBe(true);
		expect(await indexedIds()).toContain(other.reportId.toString());
		expect(await reservationIsFree(other)).toBe(false);
		const [entry] = await auditEntries('delete_report');
		expect(Object.fromEntries(entry?.metadata ?? [])).toMatchObject({objects_deleted: '1', shared_objects_kept: '1'});
	});

	test('refuses a report under a legal hold and changes nothing', async () => {
		const seeded = await seedMessageReport(newEvidence(), {legalHoldUntil: new Date(Date.now() + ms('30 days'))});

		const response = await deleteRequest(admin, seeded.reportId).expect(HTTP_STATUS.CONFLICT).execute();

		expect(response.code).toBe(APIErrorCodes.REPORT_UNDER_LEGAL_HOLD);
		expect(await repository.getReport(seeded.reportId)).not.toBeNull();
		expect(harness.storageService.hasObject(REPORTS_BUCKET, seeded.attachmentKey)).toBe(true);
		expect(harness.storageService.hasObject(REPORTS_BUCKET, seeded.profileKey)).toBe(true);
		expect(await indexedIds()).toContain(seeded.reportId.toString());
		expect(await reservationIsFree(seeded)).toBe(false);
		expect(await auditEntries('delete_report')).toEqual([]);
	});

	test('deletes a report whose legal hold has ended', async () => {
		const seeded = await seedMessageReport(newEvidence(), {legalHoldUntil: new Date(Date.now() - ms('1 minute'))});

		await deleteRequest(admin, seeded.reportId).expect(HTTP_STATUS.NO_CONTENT).execute();

		expect(await repository.getReport(seeded.reportId)).toBeNull();
	});

	test('an unknown report gives UNKNOWN_REPORT and records nothing', async () => {
		await deleteRequest(admin, '1499999999999999999')
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_REPORT)
			.execute();

		expect(await auditEntries('delete_report')).toEqual([]);
	});

	test('needs REPORT_DELETE, which REPORT_RESOLVE does not grant', async () => {
		const seeded = await seedMessageReport(newEvidence());
		const resolver = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			AdminACLs.REPORT_VIEW,
			AdminACLs.REPORT_RESOLVE,
		]);

		await deleteRequest(resolver, seeded.reportId).expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL').execute();

		expect(await repository.getReport(seeded.reportId)).not.toBeNull();
		expect(harness.storageService.hasObject(REPORTS_BUCKET, seeded.attachmentKey)).toBe(true);
		expect(await auditEntries('delete_report')).toEqual([]);
	});
});
