// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {type AttachmentDecayPayload, AttachmentDecayService} from '@app/api/attachment/AttachmentDecayService';
import {createAttachmentID, createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {resetCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {InstanceAttachmentDecayEffectiveConfig} from '@app/api/instance/InstanceConfigRepository';
import {getExpiryBucket} from '@app/api/utils/AttachmentDecay';
import {ms} from 'itty-time';
import {beforeEach, describe, expect, it} from 'vitest';

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

let NOW = new Date();

function daysFromNow(days: number): Date {
	return new Date(NOW.getTime() + days * ms('1 day'));
}

function expectAboutDaysFromRealNow(date: Date | undefined, days: number): void {
	expect(Math.abs(date!.getTime() - (Date.now() + days * ms('1 day')))).toBeLessThan(ms('1 minute'));
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
	const repo = new AttachmentDecayRepository();
	return {repo, service: new AttachmentDecayService(repo, async () => config)};
}

async function storedExpiry(repo: AttachmentDecayRepository): Promise<Date | undefined> {
	return (await repo.fetchByIds([ATTACHMENT_ID])).get(ATTACHMENT_ID)?.expires_at;
}

async function fileExists(): Promise<boolean> {
	return true;
}

async function fileMissing(): Promise<boolean> {
	return false;
}

async function seed(repo: AttachmentDecayRepository, uploadedAt: Date, expiresAt: Date): Promise<void> {
	await repo.upsert({
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
		expiry_bucket: getExpiryBucket(expiresAt),
	});
}

describe('AttachmentDecayService.reconcile', () => {
	beforeEach(() => {
		resetCassandraQueryExecutorForTesting();
		NOW = new Date();
	});

	it('writes nothing while expiry is off', async () => {
		const {repo, service} = createService({...CONFIG, enabled: false});

		expect(await service.reconcile([payload(daysFromNow(-10))], fileExists)).toBe(0);
		expect(await storedExpiry(repo)).toBeUndefined();
	});

	it('gives an untracked attachment the expiry its upload date implies', async () => {
		const {repo, service} = createService();

		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(1);
		expect(await storedExpiry(repo)).toEqual(daysFromNow(265));
	});

	it('gives an untracked attachment that is already overdue the renew window', async () => {
		const {repo, service} = createService();

		await service.reconcile([payload(daysFromNow(-1000))], fileExists);

		expectAboutDaysFromRealNow(await storedExpiry(repo), 30);
	});

	it('gives an attachment whose expiry passed unnoticed the renew window', async () => {
		const {repo, service} = createService();
		await seed(repo, daysFromNow(-1000), daysFromNow(-200));

		await service.reconcile([payload(daysFromNow(-1000))], fileExists);

		expectAboutDaysFromRealNow(await storedExpiry(repo), 30);
	});

	it('leaves an untracked attachment alone when its file is gone', async () => {
		const {repo, service} = createService();

		expect(await service.reconcile([payload(daysFromNow(-100))], fileMissing)).toBe(0);
		expect(await storedExpiry(repo)).toBeUndefined();
	});

	it('keeps a passed expiry when the file is gone', async () => {
		const {repo, service} = createService();
		await seed(repo, daysFromNow(-1000), daysFromNow(-200));

		expect(await service.reconcile([payload(daysFromNow(-1000))], fileMissing)).toBe(0);
		expect(await storedExpiry(repo)).toEqual(daysFromNow(-200));
	});

	it('does not look for the file of an attachment that is still pending', async () => {
		const {repo, service} = createService({...CONFIG, max_lifetime_days: 730});
		await seed(repo, daysFromNow(-100), daysFromNow(265));

		await service.reconcile([payload(daysFromNow(-100))], fileMissing);

		expect(await storedExpiry(repo)).toEqual(daysFromNow(630));
	});

	it('extends a tracked attachment when the lifetime grows', async () => {
		const {repo, service} = createService({...CONFIG, max_lifetime_days: 730});
		await seed(repo, daysFromNow(-100), daysFromNow(265));

		await service.reconcile([payload(daysFromNow(-100))], fileExists);

		expect(await storedExpiry(repo)).toEqual(daysFromNow(630));
	});

	it('shortens a tracked attachment no further than the renew window', async () => {
		const {repo, service} = createService({...CONFIG, max_lifetime_days: 60});
		await seed(repo, daysFromNow(-100), daysFromNow(265));

		await service.reconcile([payload(daysFromNow(-100))], fileExists);

		expectAboutDaysFromRealNow(await storedExpiry(repo), 30);
	});

	it('keeps an expiry that already falls inside the renew window', async () => {
		const {repo, service} = createService({...CONFIG, max_lifetime_days: 60});
		await seed(repo, daysFromNow(-100), daysFromNow(12));

		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(0);
		expect(await storedExpiry(repo)).toEqual(daysFromNow(12));
	});

	it('drops the record of an attachment that is no longer eligible', async () => {
		const {repo, service} = createService({...CONFIG, max_eligible_size_mb: 1});
		await seed(repo, daysFromNow(-100), daysFromNow(265));

		expect(await service.reconcile([payload(daysFromNow(-100), 2n * 1024n * 1024n)], fileExists)).toBe(1);
		expect(await storedExpiry(repo)).toBeUndefined();
	});

	it('settles after one pass', async () => {
		const {service} = createService({...CONFIG, max_lifetime_days: 60});

		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(1);
		expect(await service.reconcile([payload(daysFromNow(-100))], fileExists)).toBe(0);
	});
});
