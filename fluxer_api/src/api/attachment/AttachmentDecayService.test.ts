// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {type AttachmentDecayPayload, AttachmentDecayService} from '@app/api/attachment/AttachmentDecayService';
import type {AttachmentID} from '@app/api/BrandedTypes';
import {createAttachmentID, createChannelID, createMessageID} from '@app/api/BrandedTypes';
import type {InstanceAttachmentDecayEffectiveConfig} from '@app/api/instance/InstanceConfigRepository';
import type {AttachmentDecayRow} from '@app/api/types/AttachmentDecayTypes';
import {ms} from 'itty-time';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const NOW = new Date('2026-10-10T00:00:00.000Z');
const ATTACHMENT_ID = createAttachmentID(7001n);
const CONFIG: InstanceAttachmentDecayEffectiveConfig = {
	enabled: true,
	min_size_mb: 5,
	max_size_mb: 500,
	max_eligible_size_mb: 500,
	min_lifetime_days: 14,
	max_lifetime_days: 365,
	curve: 0.5,
	renew_threshold_days: 30,
	renew_window_days: 30,
};

function daysFromNow(days: number): Date {
	return new Date(NOW.getTime() + days * ms('1 day'));
}

function payload(uploadedAt: Date, sizeBytes = 1024n): AttachmentDecayPayload {
	return {
		attachmentId: ATTACHMENT_ID,
		channelId: createChannelID(7002n),
		messageId: createMessageID(7003n),
		filename: 'file.png',
		sizeBytes,
		uploadedAt,
	};
}

function createService(config: InstanceAttachmentDecayEffectiveConfig = CONFIG) {
	const rows = new Map<AttachmentID, AttachmentDecayRow>();
	const repo = {
		async fetchByIds(ids: Array<AttachmentID>) {
			return new Map(ids.flatMap((id) => (rows.has(id) ? [[id, rows.get(id)!] as const] : [])));
		},
		async upsert(record: AttachmentDecayRow) {
			rows.set(record.attachment_id, record);
		},
		async deleteRecords({attachment_id}: {attachment_id: AttachmentID}) {
			rows.delete(attachment_id);
		},
	} as unknown as AttachmentDecayRepository;
	return {rows, service: new AttachmentDecayService(repo, async () => config)};
}

async function fileExists(): Promise<boolean> {
	return true;
}

async function fileMissing(): Promise<boolean> {
	return false;
}

function seed(rows: Map<AttachmentID, AttachmentDecayRow>, uploadedAt: Date, expiresAt: Date): void {
	rows.set(ATTACHMENT_ID, {
		attachment_id: ATTACHMENT_ID,
		channel_id: createChannelID(7002n),
		message_id: createMessageID(7003n),
		filename: 'file.png',
		size_bytes: 1024n,
		uploaded_at: uploadedAt,
		expires_at: expiresAt,
		last_accessed_at: uploadedAt,
		cost: 1,
		lifetime_days: 1,
		status: null,
	});
}

describe('AttachmentDecayService.reconcile', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		vi.setSystemTime(NOW);
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('writes nothing while expiry is off', async () => {
		const {rows, service} = createService({...CONFIG, enabled: false});

		expect(await service.reconcile([payload(daysFromNow(-10))], fileExists)).toBe(0);
		expect(rows.size).toBe(0);
	});

	it('gives an untracked attachment the expiry its upload date implies', async () => {
		const {rows, service} = createService();

		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(1);
		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(265));
	});

	it('gives an untracked attachment that is already overdue the renew window', async () => {
		const {rows, service} = createService();

		await service.reconcile([payload(daysFromNow(-1000))], fileExists);

		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(30));
	});

	it('gives an attachment whose expiry passed unnoticed the renew window', async () => {
		const {rows, service} = createService();
		seed(rows, daysFromNow(-1000), daysFromNow(-200));

		await service.reconcile([payload(daysFromNow(-1000))], fileExists);

		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(30));
	});

	it('leaves an untracked attachment alone when its file is gone', async () => {
		const {rows, service} = createService();

		expect(await service.reconcile([payload(daysFromNow(-100))], fileMissing)).toBe(0);
		expect(rows.size).toBe(0);
	});

	it('keeps a passed expiry when the file is gone', async () => {
		const {rows, service} = createService();
		seed(rows, daysFromNow(-1000), daysFromNow(-200));

		expect(await service.reconcile([payload(daysFromNow(-1000))], fileMissing)).toBe(0);
		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(-200));
	});

	it('does not look for the file of an attachment that is still pending', async () => {
		const {rows, service} = createService({...CONFIG, max_lifetime_days: 730});
		seed(rows, daysFromNow(-100), daysFromNow(265));

		await service.reconcile([payload(daysFromNow(-100))], fileMissing);

		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(630));
	});

	it('extends a tracked attachment when the lifetime grows', async () => {
		const {rows, service} = createService({...CONFIG, max_lifetime_days: 730});
		seed(rows, daysFromNow(-100), daysFromNow(265));

		await service.reconcile([payload(daysFromNow(-100))], fileExists);

		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(630));
	});

	it('shortens a tracked attachment no further than the renew window', async () => {
		const {rows, service} = createService({...CONFIG, max_lifetime_days: 60});
		seed(rows, daysFromNow(-100), daysFromNow(265));

		await service.reconcile([payload(daysFromNow(-100))], fileExists);

		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(30));
	});

	it('keeps an expiry that already falls inside the renew window', async () => {
		const {rows, service} = createService({...CONFIG, max_lifetime_days: 60});
		seed(rows, daysFromNow(-100), daysFromNow(12));

		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(0);
		expect(rows.get(ATTACHMENT_ID)?.expires_at).toEqual(daysFromNow(12));
	});

	it('drops the record of an attachment that is no longer eligible', async () => {
		const {rows, service} = createService({...CONFIG, max_eligible_size_mb: 1});
		seed(rows, daysFromNow(-100), daysFromNow(265));

		expect(await service.reconcile([payload(daysFromNow(-100), 2n * 1024n * 1024n)], fileExists)).toBe(1);
		expect(rows.size).toBe(0);
	});

	it('settles after one pass', async () => {
		const {service} = createService({...CONFIG, max_lifetime_days: 60});

		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(1);
		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(0);
	});
});
