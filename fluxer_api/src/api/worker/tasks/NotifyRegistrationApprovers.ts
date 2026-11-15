// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {User} from '@app/api/models/User';
import {noticeEmailAddress, systemDmsHidden} from '@app/api/user/services/SystemDmMessage';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const REVIEW_ACLS = [AdminACLs.WILDCARD, AdminACLs.REGISTRATION_REVIEW];
const NOTICE_INTERVAL_SECONDS = 10 * 60;
const INELIGIBLE_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.DISABLED;
const PayloadSchema = z.object({user_id: z.string().regex(/^\d+$/)});

function canReviewRegistrations(user: User): boolean {
	if (user.isBot || (user.flags & INELIGIBLE_FLAGS) !== 0n) return false;
	if (user.acls.has(AdminACLs.WILDCARD)) return true;
	return user.acls.has(AdminACLs.AUTHENTICATE) && user.acls.has(AdminACLs.REGISTRATION_REVIEW);
}

function pendingRegistrationsUrl(): string {
	return `${Config.endpoints.admin.replace(/\/+$/, '')}/pending-registrations`;
}

function registrationApprovalNoticeContent(): string {
	return `New sign-ups are waiting for approval. Review pending registrations at ${pendingRegistrationsUrl()}`;
}

async function findReviewers(): Promise<Array<User>> {
	const {userRepository} = getWorkerDependencies();
	const ids = new Set((await Promise.all(REVIEW_ACLS.map((acl) => userRepository.listUserIdsWithAcl(acl)))).flat());
	const users = await userRepository.listUsers([...ids]);
	return users.filter(canReviewRegistrations);
}

async function emailReviewers(reviewers: Array<User>): Promise<void> {
	const {instanceConfigRepository, emailService} = getWorkerDependencies();
	for (const reviewer of reviewers) {
		const email = await noticeEmailAddress(reviewer, instanceConfigRepository);
		if (email) {
			await emailService.send('registration_pending', email, {reviewUrl: pendingRegistrationsUrl()}, reviewer.locale);
		}
	}
}

const notifyRegistrationApprovers: WorkerTaskHandler = async (payload, helpers) => {
	const {kvClient, instanceConfigRepository} = getWorkerDependencies();
	const {user_id: userId} = PayloadSchema.parse(payload);
	const reviewers = await findReviewers();
	const recipients: Array<User> = [];
	for (const user of reviewers) {
		if (await kvClient.setnx(`registration_approval_notice:${user.id}`, '1', NOTICE_INTERVAL_SECONDS)) {
			recipients.push(user);
		}
	}
	helpers.logger.info({userId, recipients: recipients.length}, 'Notifying registration approvers');
	if (recipients.length === 0) return;
	if (await systemDmsHidden(instanceConfigRepository)) {
		await emailReviewers(recipients);
		return;
	}
	await helpers.addJob('sendSystemDm', {
		content: registrationApprovalNoticeContent(),
		user_ids: recipients.map((user) => user.id.toString()),
	});
};

export default notifyRegistrationApprovers;
