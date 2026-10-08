// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {action, makeObservable, observableRef} from 'mobx';

const logger = new Logger('AccountScopedWork');

const ACCOUNT_SCOPED_WORK_IDLE_TIMEOUT_MS = 5000;

class AccountScopedWorkFenceTimeoutError extends Error {
	constructor(outstanding: number) {
		super(`Account scoped work fence timed out with ${outstanding} outstanding operation(s)`);
		this.name = 'AccountScopedWorkFenceTimeoutError';
	}
}

export const AccountScopedWorkTransitionReason = Object.freeze({
	LOGOUT: 'logout',
	ACCOUNT_SWITCH: 'account-switch',
} as const);

export type AccountScopedWorkTransitionReason =
	(typeof AccountScopedWorkTransitionReason)[keyof typeof AccountScopedWorkTransitionReason];

export type AccountScopedWorkCancellation = (reason: AccountScopedWorkTransitionReason) => void | Promise<void>;

export interface AccountScopedWorkTransitionHandler {
	suspend(reason: AccountScopedWorkTransitionReason): void | Promise<void>;
	resume(reason: AccountScopedWorkTransitionReason): void | Promise<void>;
	released?(reason: AccountScopedWorkTransitionReason): void | Promise<void>;
}

export interface AccountScopedWorkTicket {
	readonly signal: AbortSignal;
	readonly isStale: boolean;
	assertCurrent(): void;
	dispose(): void;
}

const accountScopedWorkSuspensionBrand: unique symbol = Symbol('AccountScopedWorkSuspension');

export interface AccountScopedWorkSuspension {
	readonly reason: AccountScopedWorkTransitionReason;
	readonly [accountScopedWorkSuspensionBrand]: true;
	assertActive(reason: AccountScopedWorkTransitionReason): void;
}

export interface AccountScopedWorkRegistry {
	readonly isSuspended: boolean;
	registerCancellation(cancellation: AccountScopedWorkCancellation): () => void;
	registerTransition(handler: AccountScopedWorkTransitionHandler): () => void;
	begin(): AccountScopedWorkTicket;
	beginWithinSuspension(suspension: AccountScopedWorkSuspension): AccountScopedWorkTicket;
	runSuspended<Result>(
		reason: AccountScopedWorkTransitionReason,
		operation: (suspension: AccountScopedWorkSuspension) => Promise<Result>,
	): Promise<Result>;
}

class AccountScopedWorkCancellationAlreadyRegisteredError extends Error {
	constructor() {
		super('Account scoped work cancellation already has a registration owner');
		this.name = 'AccountScopedWorkCancellationAlreadyRegisteredError';
	}
}

class AccountScopedWorkTransitionAlreadyRegisteredError extends Error {
	constructor() {
		super('Account scoped work transition already has a registration owner');
		this.name = 'AccountScopedWorkTransitionAlreadyRegisteredError';
	}
}

class AccountScopedWorkSuspensionConflictError extends Error {
	constructor(activeReason: AccountScopedWorkTransitionReason, requestedReason: AccountScopedWorkTransitionReason) {
		super(`Account scoped work is already suspended for ${activeReason}, cannot start ${requestedReason}`);
		this.name = 'AccountScopedWorkSuspensionConflictError';
	}
}

class AccountScopedWorkSuspensionInvariantError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'AccountScopedWorkSuspensionInvariantError';
	}
}

export function accountScopedWorkAbortError(): DOMException {
	return createAccountTransitionAbortError();
}

class AccountScopedWorkSuspensionLease implements AccountScopedWorkSuspension {
	readonly [accountScopedWorkSuspensionBrand] = true;

	constructor(
		readonly reason: AccountScopedWorkTransitionReason,
		private readonly owner: AccountScopedWorkOwner,
	) {}

	assertActive(reason: AccountScopedWorkTransitionReason): void {
		this.owner.assertActiveSuspension(this, reason);
	}
}

type AccountScopedWorkAdmission =
	| {readonly kind: 'open'}
	| {readonly kind: 'suspended'; readonly suspension: AccountScopedWorkSuspensionLease};

class AccountScopedWorkOwner implements AccountScopedWorkRegistry {
	private readonly cancellations = new Set<AccountScopedWorkCancellation>();
	private readonly transitionHandlers = new Set<AccountScopedWorkTransitionHandler>();
	private readonly outstanding = new Set<AccountScopedWorkTicket>();
	private readonly idleWaiters = new Set<() => void>();
	private generation = 0;
	private controller = new AbortController();
	private admission: AccountScopedWorkAdmission = {kind: 'open'};

	constructor() {
		makeObservable<AccountScopedWorkOwner, 'admission' | 'setAdmission'>(this, {
			admission: observableRef,
			setAdmission: action,
		});
	}

	private setAdmission(admission: AccountScopedWorkAdmission): void {
		this.admission = admission;
	}

	get isSuspended(): boolean {
		return this.admission.kind === 'suspended';
	}

	registerCancellation(cancellation: AccountScopedWorkCancellation): () => void {
		if (this.cancellations.has(cancellation)) {
			throw new AccountScopedWorkCancellationAlreadyRegisteredError();
		}
		this.cancellations.add(cancellation);
		let registered = true;
		return () => {
			if (!registered) {
				return;
			}
			registered = false;
			this.cancellations.delete(cancellation);
		};
	}

	registerTransition(handler: AccountScopedWorkTransitionHandler): () => void {
		if (this.transitionHandlers.has(handler)) {
			throw new AccountScopedWorkTransitionAlreadyRegisteredError();
		}
		this.transitionHandlers.add(handler);
		let registered = true;
		return () => {
			if (!registered) {
				return;
			}
			registered = false;
			this.transitionHandlers.delete(handler);
		};
	}

	begin(): AccountScopedWorkTicket {
		if (this.admission.kind === 'suspended') {
			throw accountScopedWorkAbortError();
		}
		return this.createTicket();
	}

	beginWithinSuspension(suspension: AccountScopedWorkSuspension): AccountScopedWorkTicket {
		this.assertActiveSuspension(suspension, suspension.reason);
		return this.createTicket();
	}

	private createTicket(): AccountScopedWorkTicket {
		const generation = this.generation;
		const signal = this.controller.signal;
		const isStale = (): boolean => generation !== this.generation;
		let disposed = false;
		const ticket: AccountScopedWorkTicket = {
			signal,
			get isStale(): boolean {
				return isStale();
			},
			assertCurrent: (): void => {
				if (isStale()) {
					throw accountScopedWorkAbortError();
				}
			},
			dispose: (): void => {
				if (disposed) {
					return;
				}
				disposed = true;
				this.outstanding.delete(ticket);
				if (this.outstanding.size === 0) {
					this.releaseIdleWaiters();
				}
			},
		};
		this.outstanding.add(ticket);
		return ticket;
	}

	async runSuspended<Result>(
		reason: AccountScopedWorkTransitionReason,
		operation: (suspension: AccountScopedWorkSuspension) => Promise<Result>,
	): Promise<Result> {
		this.assertAdmissionOpen(reason);
		const suspension = new AccountScopedWorkSuspensionLease(reason, this);
		this.setAdmission({kind: 'suspended', suspension});
		this.generation += 1;
		this.controller.abort(accountScopedWorkAbortError());
		this.controller = new AbortController();
		try {
			await this.settleAll(
				[
					...[...this.cancellations].map((cancellation) => () => cancellation(reason)),
					...[...this.transitionHandlers].map((handler) => () => handler.suspend(reason)),
				],
				`Account scoped work suspension failed during ${reason}`,
			);
			await this.awaitIdle();
			return await operation(suspension);
		} finally {
			await this.settleAll(
				[...this.transitionHandlers].map((handler) => () => handler.resume(reason)),
				`Account scoped work resumption failed after ${reason}`,
			);
			this.releaseSuspension(suspension);
			await this.notifyAdmissionReleased(reason);
		}
	}

	assertActiveSuspension(suspension: AccountScopedWorkSuspension, reason: AccountScopedWorkTransitionReason): void {
		if (this.admission.kind !== 'suspended' || this.admission.suspension !== suspension) {
			throw new AccountScopedWorkSuspensionInvariantError('Account scoped work suspension is no longer active');
		}
		if (suspension.reason !== reason) {
			throw new AccountScopedWorkSuspensionInvariantError(
				`Account scoped work suspension reason is ${suspension.reason}, not ${reason}`,
			);
		}
	}

	awaitIdle(): Promise<void> {
		if (this.outstanding.size === 0) {
			return Promise.resolve();
		}
		return new Promise<void>((resolve, reject) => {
			let settled = false;
			const finish = (error?: Error): void => {
				if (settled) {
					return;
				}
				settled = true;
				clearTimeout(timer);
				this.idleWaiters.delete(finish);
				if (error === undefined) {
					resolve();
				} else {
					reject(error);
				}
			};
			const timer = setTimeout(() => {
				const error = new AccountScopedWorkFenceTimeoutError(this.outstanding.size);
				logger.warn(error.message, ACCOUNT_SCOPED_WORK_IDLE_TIMEOUT_MS);
				finish(error);
			}, ACCOUNT_SCOPED_WORK_IDLE_TIMEOUT_MS);
			this.idleWaiters.add(finish);
		});
	}

	private releaseIdleWaiters(): void {
		for (const waiter of [...this.idleWaiters]) {
			waiter();
		}
	}

	private assertAdmissionOpen(reason: AccountScopedWorkTransitionReason): void {
		if (this.admission.kind === 'suspended') {
			throw new AccountScopedWorkSuspensionConflictError(this.admission.suspension.reason, reason);
		}
	}

	private releaseSuspension(suspension: AccountScopedWorkSuspensionLease): void {
		this.assertActiveSuspension(suspension, suspension.reason);
		this.generation += 1;
		this.controller.abort(accountScopedWorkAbortError());
		this.setAdmission({kind: 'open'});
		this.controller = new AbortController();
	}

	private async settleAll(actions: Array<() => void | Promise<void>>, message: string): Promise<void> {
		const results = await Promise.allSettled(
			actions.map((run) => {
				try {
					return Promise.resolve(run());
				} catch (error) {
					return Promise.reject(error);
				}
			}),
		);
		for (const result of results) {
			if (result.status === 'rejected') {
				logger.warn(message, result.reason);
			}
		}
	}

	private async notifyAdmissionReleased(reason: AccountScopedWorkTransitionReason): Promise<void> {
		await this.settleAll(
			[...this.transitionHandlers]
				.filter((handler) => handler.released !== undefined)
				.map((handler) => () => handler.released?.(reason)),
			`Account scoped work release notification failed after ${reason}`,
		);
	}
}

const owner = new AccountScopedWorkOwner();

export const AccountScopedWork: AccountScopedWorkRegistry = owner;

export function awaitAccountScopedWorkIdle(): Promise<void> {
	return owner.awaitIdle();
}
