// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getWorkerService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getEmailService, getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import notifyRegistrationApprovers from '@app/api/worker/tasks/NotifyRegistrationApprovers';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('notice to registration approvers', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let reviewer: TestAccount;
	let bystander: TestAccount;
	let worker: RecordingWorkerService;
	let previousWorker: ReturnType<typeof getWorkerService>;
	let helpers: ReturnType<typeof createTestWorkerHelpers>;
	const adminEndpoint = getConfig().endpoints.admin;
	const selfHosted = getConfig().instance.selfHosted;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		previousWorker = getWorkerService();
		worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
		helpers = createTestWorkerHelpers({addJob: worker.addJob.bind(worker)});
		owner = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		reviewer = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.REGISTRATION_REVIEW,
		]);
		bystander = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_LOOKUP,
		]);
		setWorkerDependenciesForTest({
			userRepository: getUserRepository(),
			kvClient: harness.kvProvider,
			instanceConfigRepository: getInstanceConfigRepository(),
			emailService: getEmailService(),
		});
		worker.jobs.length = 0;
		await clearTestEmails(harness);
		getConfig().endpoints.admin = 'https://admin.example.test///';
		getConfig().instance.selfHosted = true;
	});

	afterEach(() => {
		getConfig().instance.selfHosted = selfHosted;
		clearWorkerDependencies();
		getConfig().endpoints.admin = adminEndpoint;
		setInjectedWorkerService(previousWorker);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function notifiedApprovers(): Promise<Set<string>> {
		await notifyRegistrationApprovers({user_id: '125'}, helpers);
		const jobs = worker.byTask('sendSystemDm');
		expect(jobs).toHaveLength(1);
		const recipients = jobs[0]?.payload.user_ids;
		if (!Array.isArray(recipients) || !recipients.every((id) => typeof id === 'string')) {
			throw new Error('Expected registration notice recipient IDs');
		}
		return new Set(recipients);
	}

	function setAcls(target: TestAccount, acls: Array<string>) {
		return createBuilder(harness, owner.token).put(`/admin/users/${target.userId}/acls`).body({acls}).execute();
	}

	it('notifies an approver granted the ACL and drops one whose ACL was revoked', async () => {
		await setAcls(reviewer, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
		await setAcls(bystander, [AdminACLs.AUTHENTICATE, AdminACLs.REGISTRATION_REVIEW]);
		expect(await notifiedApprovers()).toEqual(new Set([owner.userId, bystander.userId]));
	});

	it('sends one system message to each approver and holds back repeats for a while', async () => {
		await notifyRegistrationApprovers({user_id: '123'}, helpers);
		const firstNotice = [
			{
				taskType: 'sendSystemDm',
				payload: {
					user_ids: [owner.userId, reviewer.userId],
					content:
						'New sign-ups are waiting for approval. Review pending registrations at https://admin.example.test/pending-registrations',
				},
				options: undefined,
			},
		];
		expect(worker.jobs).toEqual(firstNotice);
		await notifyRegistrationApprovers({user_id: '124'}, helpers);
		expect(worker.jobs).toEqual(firstNotice);
	});

	it('emails each approver instead of a hidden DM when the instance turned DMs off', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({direct_messages_disabled: true});
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {enabled: true}});
		await notifyRegistrationApprovers({user_id: '126'}, helpers);
		expect(worker.jobs).toEqual([]);
		const emails = await listTestEmails(harness);
		expect(emails.map(({to, type, metadata}) => ({to, type, metadata}))).toEqual([
			{
				to: owner.email,
				type: 'registration_pending',
				metadata: {review_url: 'https://admin.example.test/pending-registrations'},
			},
			{
				to: reviewer.email,
				type: 'registration_pending',
				metadata: {review_url: 'https://admin.example.test/pending-registrations'},
			},
		]);
	});

	it('sends nothing when DMs are off and the instance sends no email', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({direct_messages_disabled: true});
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {enabled: false}});
		await notifyRegistrationApprovers({user_id: '127'}, helpers);
		expect(worker.jobs).toEqual([]);
		expect(await listTestEmails(harness)).toEqual([]);
	});
});
