// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {OperatorPassRemovalHistoryRow, OperatorPassRemovalRow} from '@app/api/database/types/OperatorPassTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import type {User} from '@app/api/models/User';
import {
	barOperatorPassAfterDispute,
	liftOperatorPassDisputeBar,
	type OperatorPassDisputeBarProgress,
	type OperatorPassDisputeServices,
} from '@app/api/operator_pass/OperatorPassDisputeBar';
import {OperatorPassRemovalService} from '@app/api/operator_pass/OperatorPassRemovalService';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {expect, onTestFinished, test, vi} from 'vitest';

const user = {id: createUserID(77n), email: 'buyer@example.com', locale: 'en-US', flags: 0n} as unknown as User;
const review = {
	dispute_id: 'dp_test_bar',
	decided_by: null,
	warning_deadline: new Date('2026-12-01T00:00:00Z'),
} as unknown as PaymentDisputeReviewRow;

function fixture() {
	let removal: OperatorPassRemovalRow | null = null;
	const history: Array<OperatorPassRemovalHistoryRow> = [];
	const auditPending = new Set<string>();
	const noticePending = new Set<string>();
	const repository = {
		findRemoval: vi.fn(async () => removal),
		saveRemoval: vi.fn(async (row: OperatorPassRemovalRow) => {
			removal = row;
		}),
		deleteRemoval: vi.fn(async () => {
			removal = null;
		}),
		listRemovalHistory: vi.fn(async () => history),
		saveRemovalHistory: vi.fn(async (row: OperatorPassRemovalHistoryRow) => {
			history.push(row);
		}),
		endRemovalHistory: vi.fn(async (_userId: unknown, removedAt: Date, endedAt: Date, reason: string) => {
			const entry = history.find((row) => row.removed_at.getTime() === removedAt.getTime());
			if (entry) Object.assign(entry, {ended_at: endedAt, end_reason: reason});
		}),
	};
	const wallService = {
		recordHidden: vi.fn(async () => {}),
		recordShownAgain: vi.fn(async () => {}),
		endBusinessListing: vi.fn(async () => {}),
	};
	const emailService = {send: vi.fn(async () => true)};
	const removalService = new OperatorPassRemovalService(
		repository as unknown as OperatorPassRepository,
		{findUnique: vi.fn(async () => user)} as never,
		{} as never,
		emailService as never,
		{} as never,
		{wallService, holdingService: {}} as never,
	);
	const syncHolder = vi
		.spyOn(removalService as unknown as {syncHolder: (id: unknown) => Promise<void>}, 'syncHolder')
		.mockResolvedValue();
	const services = {repository, removalService} as unknown as OperatorPassDisputeServices;
	const progress: OperatorPassDisputeBarProgress = {
		isAuditPending: async (disputeId) => auditPending.has(disputeId),
		markAuditPending: async (disputeId) => {
			auditPending.add(disputeId);
		},
		clearAuditPending: async (disputeId) => {
			auditPending.delete(disputeId);
		},
		isNoticePending: async (disputeId) => noticePending.has(disputeId),
		markNoticePending: async (disputeId) => {
			noticePending.add(disputeId);
		},
		clearNoticePending: async (disputeId) => {
			noticePending.delete(disputeId);
		},
	};
	return {
		repository,
		wallService,
		emailService,
		removalService,
		syncHolder,
		services,
		progress,
		history,
		auditPending,
		noticePending,
		current: () => removal,
	};
}

test.each([
	['saving the active removal', 'saveRemoval'],
	['hiding the wall entry', 'recordHidden'],
	['ending the business listing', 'endBusinessListing'],
	['syncing access', 'syncHolder'],
	['emailing the buyer', 'send'],
] as const)('a bar that failed after %s is finished on the next run', async (_step, step) => {
	const state = fixture();
	const targets = {
		saveRemoval: state.repository.saveRemoval,
		recordHidden: state.wallService.recordHidden,
		endBusinessListing: state.wallService.endBusinessListing,
		syncHolder: state.syncHolder,
		send: state.emailService.send,
	};
	(targets[step] as ReturnType<typeof vi.fn>).mockRejectedValueOnce(new Error(`${step} failed`));

	await expect(barOperatorPassAfterDispute(state.services, state.progress, user, review)).rejects.toThrow(
		`${step} failed`,
	);
	expect(state.history).toHaveLength(1);

	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(true);
	expect(state.current()).toMatchObject({removal_state: 'removed', removed_at: state.history[0]!.removed_at});
	expect(state.history).toHaveLength(1);
	expect(state.syncHolder).toHaveBeenCalled();
	expect(state.emailService.send).toHaveBeenLastCalledWith(
		'operator_pass_perks_removed',
		user.email,
		expect.objectContaining({
			rule: 'Section 9 of the Operator Pass terms',
			dispute: {id: review.dispute_id, deadline: review.warning_deadline},
		}),
		user.locale,
	);

	const sends = state.emailService.send.mock.calls.length;
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);
	expect(state.emailService.send.mock.calls.length).toBe(sends);
});

test('a bar lifted after an appeal is not put back by the next run', async () => {
	const state = fixture();
	await state.repository.saveRemovalHistory({
		user_id: user.id,
		removed_at: new Date('2026-12-02T00:00:00Z'),
		removed_until: null,
		rule: 'Section 9 of the Operator Pass terms',
		facts: 'Payment dispute dp_test_bar was found unjustified',
		automated: false,
		serious: true,
		notice_sent_at: null,
		admin_user_id: user.id,
		ended_at: new Date('2026-12-05T00:00:00Z'),
		end_reason: 'restored',
	});
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);
	expect(state.current()).toBeNull();
	expect(state.emailService.send).not.toHaveBeenCalled();
});

test('a bar whose email was not delivered sends the reasons again on the next run, without a second removal', async () => {
	const state = fixture();
	state.emailService.send.mockResolvedValueOnce(false);

	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(true);
	expect(state.emailService.send).toHaveBeenCalledTimes(1);
	expect(state.current()).toMatchObject({removal_state: 'removed'});

	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);
	expect(state.emailService.send).toHaveBeenCalledTimes(2);
	expect(state.history).toHaveLength(1);

	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);
	expect(state.emailService.send).toHaveBeenCalledTimes(2);
});

test('reversing one of two lost disputes keeps the bar on the other, and reversing both lifts it', async () => {
	const state = fixture();
	const second = {...review, dispute_id: 'dp_test_bar_second'} as PaymentDisputeReviewRow;
	vi.spyOn(state.removalService, 'restore').mockImplementation(async () => {
		await state.repository.deleteRemoval();
		return true;
	});

	vi.useFakeTimers({toFake: ['Date'], now: new Date('2026-12-03T00:00:00Z')});
	onTestFinished(() => {
		vi.useRealTimers();
	});
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(true);
	vi.setSystemTime(new Date('2026-12-04T00:00:00Z'));
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, second)).toBe(true);
	vi.setSystemTime(new Date('2026-12-20T00:00:00Z'));
	expect(state.current()?.facts).toMatch(/^Payment dispute dp_test_bar /);

	expect(await liftOperatorPassDisputeBar(state.services, state.progress, user, review.dispute_id, [second])).toBe(
		false,
	);
	expect(state.current()).toMatchObject({removal_state: 'removed', removed_until: null});
	expect(state.current()?.facts).toMatch(/^Payment dispute dp_test_bar_second /);
	expect(state.emailService.send).toHaveBeenLastCalledWith(
		'operator_pass_perks_removed',
		user.email,
		expect.objectContaining({dispute: {id: second.dispute_id, deadline: second.warning_deadline}}),
		user.locale,
	);
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, second)).toBe(false);

	expect(await liftOperatorPassDisputeBar(state.services, state.progress, user, second.dispute_id, [])).toBe(true);
	expect(state.current()).toBeNull();
});

test('a bar recorded behind another permanent removal takes over when that removal is restored', async () => {
	const state = fixture();
	vi.useFakeTimers({toFake: ['Date'], now: new Date('2026-12-03T00:00:00Z')});
	onTestFinished(() => {
		vi.useRealTimers();
	});
	await state.removalService.remove({
		user,
		adminUserId: user.id,
		rule: 'Section 3.1 of the Operator Pass terms',
		facts: 'Wall breach',
		automated: false,
		serious: true,
		durationDays: null,
		notifyUser: true,
	});
	vi.setSystemTime(new Date('2026-12-04T00:00:00Z'));
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(true);
	expect(state.current()?.facts).toBe('Wall breach');
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);

	vi.setSystemTime(new Date('2026-12-05T00:00:00Z'));
	expect(await state.removalService.restore(user.id)).toBe(true);
	expect(state.current()).toMatchObject({removal_state: 'removed', removed_until: null});
	expect(state.current()?.facts).toMatch(/^Payment dispute dp_test_bar /);
	expect(state.history.find((entry) => entry.facts === 'Wall breach')).toMatchObject({end_reason: 'restored'});

	const sends = state.emailService.send.mock.calls.length;
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);
	expect(state.emailService.send.mock.calls.length).toBe(sends);
});

test('losing the progress markers does not email or audit an applied bar again', async () => {
	const state = fixture();
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(true);
	state.auditPending.clear();
	state.noticePending.clear();
	const sends = state.emailService.send.mock.calls.length;
	expect(await barOperatorPassAfterDispute(state.services, state.progress, user, review)).toBe(false);
	expect(state.emailService.send.mock.calls.length).toBe(sends);
	expect(state.history).toHaveLength(1);
});
