// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';
import {instanceRequest, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Headers} from '@fluxer/constants/src/Headers';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {computed, makeAutoObservable, observable, runInAction} from 'mobx';

const logger = new Logger('AccountAccess');

const ACCOUNT_ACCESS_POLL_INTERVAL_MS = 5_000;
const ACCOUNT_ACCESS_UNAVAILABLE_BACKOFF_MAX_MS = 300_000;
const ACCOUNT_ACCESS_REQUEST_TIMEOUT_MS = 10_000;

export const AccountAccessPhase = Object.freeze({
	UNKNOWN: 'unknown',
	CHECKING: 'checking',
	ACTION_REQUIRED: 'action_required',
	GATEWAY_STARTING: 'gateway_starting',
	ALLOWED: 'allowed',
	UNAVAILABLE: 'unavailable',
} as const);

export type AccountAccessPhase = (typeof AccountAccessPhase)[keyof typeof AccountAccessPhase];

export const AccountAccessDecision = Object.freeze({
	ALLOWED: 'allowed',
	ACTION_REQUIRED: 'action_required',
	UNAVAILABLE: 'unavailable',
	INVALID: 'invalid',
} as const);

export type AccountAccessDecision = (typeof AccountAccessDecision)[keyof typeof AccountAccessDecision];

const GATEWAY_READY_CLEARS_PHASES: ReadonlySet<AccountAccessPhase> = new Set([
	AccountAccessPhase.ALLOWED,
	AccountAccessPhase.GATEWAY_STARTING,
	AccountAccessPhase.UNAVAILABLE,
]);

export class AccountInstanceUnavailableError extends Error {
	constructor(cause: Error) {
		super(`Account instance is unavailable: ${cause.message}`, {cause});
		this.name = 'AccountInstanceUnavailableError';
	}
}

class AccountAccessUserMismatchError extends Error {
	constructor(accountKey: string) {
		super(`Account access response user did not match account ${accountKey}`);
		this.name = 'AccountAccessUserMismatchError';
	}
}

class AccountAccessUnknownFailureError extends Error {
	constructor(error: unknown) {
		super(String(error));
		this.name = 'AccountAccessUnknownFailureError';
	}
}

class AccountAccessUnrecordedUnavailabilityError extends Error {
	constructor(accountKey: string) {
		super(`Account instance for ${accountKey} is unavailable and recorded no cause`);
		this.name = 'AccountAccessUnrecordedUnavailabilityError';
	}
}

interface AccountAccessRecord {
	consecutiveFailures: number;
	nextPollAt: number;
	phase: AccountAccessPhase;
	user: UserPrivate | null;
	wasActionRequired: boolean;
	error: Error | null;
}

interface AccountAccessRequest {
	readonly controller: AbortController;
	readonly promise: Promise<AccountAccessDecision>;
	readonly token: string;
}

export interface AccountAccessCheckOptions {
	readonly force?: boolean;
	readonly resetFailures?: boolean;
}

function userRequiresAction(user: UserPrivate): boolean {
	const requiredActions = user.required_actions;
	return Array.isArray(requiredActions) && requiredActions.length > 0;
}

function actionRequiredNextPollAt(actionRequired: boolean): number {
	return actionRequired ? Date.now() + ACCOUNT_ACCESS_POLL_INTERVAL_MS : 0;
}

function checkedAccountPhase(actionRequired: boolean, wasActionRequired: boolean): AccountAccessPhase {
	if (actionRequired) {
		return AccountAccessPhase.ACTION_REQUIRED;
	}
	if (wasActionRequired) {
		return AccountAccessPhase.GATEWAY_STARTING;
	}
	return AccountAccessPhase.ALLOWED;
}

function checkedAccountDecision(actionRequired: boolean): AccountAccessDecision {
	return actionRequired ? AccountAccessDecision.ACTION_REQUIRED : AccountAccessDecision.ALLOWED;
}

function accountAllowsAccessCheck(account: Account | null): account is Account {
	if (account === null) {
		return false;
	}
	if (account.token.length === 0) {
		return false;
	}
	return account.isValid;
}

function normalizeAccountAccessError(error: unknown): Error {
	return error instanceof Error ? error : new AccountAccessUnknownFailureError(error);
}

function recheckConsecutiveFailures(options: AccountAccessCheckOptions, existing: AccountAccessRecord | null): number {
	if (options.force === true && options.resetFailures !== false) {
		return 0;
	}
	return existing?.consecutiveFailures ?? 0;
}

function currentAccountKey(): string | null {
	const account = SessionManager.currentAccount;
	return account === null ? null : getAccountKey(account);
}

function unavailablePollDelay(consecutiveFailures: number): number {
	return Math.min(
		ACCOUNT_ACCESS_POLL_INTERVAL_MS * 2 ** consecutiveFailures,
		ACCOUNT_ACCESS_UNAVAILABLE_BACKOFF_MAX_MS,
	);
}

export class AccountAccess {
	private readonly records = observable.map<string, AccountAccessRecord>();
	private readonly checkedTokens = new Map<string, string>();
	private readonly requests = new Map<string, AccountAccessRequest>();
	revision = 0;

	constructor() {
		makeAutoObservable<AccountAccess, 'records' | 'checkedTokens' | 'requests' | 'setRecord'>(
			this,
			{
				records: false,
				checkedTokens: false,
				requests: false,
				currentPhase: computed,
				setRecord: false,
			},
			{autoBind: true},
		);
	}

	get currentPhase(): AccountAccessPhase {
		const accountKey = currentAccountKey();
		return accountKey === null ? AccountAccessPhase.UNKNOWN : this.getPhase(accountKey);
	}

	getPhase(accountKey: string): AccountAccessPhase {
		const record = this.recordFor(accountKey);
		return record === null ? AccountAccessPhase.UNKNOWN : record.phase;
	}

	getUnavailabilityCause(accountKey: string): Error {
		const record = this.recordFor(accountKey);
		if (record === null || record.error === null) {
			return new AccountAccessUnrecordedUnavailabilityError(accountKey);
		}
		return record.error;
	}

	getCheckedUser(accountKey: string): UserPrivate | null {
		const account = this.accountFor(accountKey);
		const record = this.recordFor(accountKey);
		if (account === null || record === null || this.tokenCheckedFor(accountKey) !== account.token) {
			return null;
		}
		if (
			record.phase !== AccountAccessPhase.ALLOWED &&
			record.phase !== AccountAccessPhase.ACTION_REQUIRED &&
			record.phase !== AccountAccessPhase.GATEWAY_STARTING
		) {
			return null;
		}
		return record.user;
	}

	getRetryEligibleAt(accountKey: string): number {
		const record = this.recordFor(accountKey);
		return record === null ? 0 : record.nextPollAt;
	}

	async ensureAccountChecked(
		accountKey: string,
		options: AccountAccessCheckOptions = {},
	): Promise<AccountAccessDecision> {
		const account = this.accountFor(accountKey);
		if (!accountAllowsAccessCheck(account)) {
			this.forgetAccount(accountKey);
			return AccountAccessDecision.INVALID;
		}
		const existingToken = this.tokenCheckedFor(accountKey);
		if (existingToken !== null && existingToken !== account.token) {
			this.clearRecord(accountKey);
		}
		const existingRequest = this.requestFor(accountKey);
		if (existingRequest !== null && existingRequest.token === account.token) {
			return await existingRequest.promise;
		}
		if (existingRequest !== null) {
			existingRequest.controller.abort();
			this.requests.delete(accountKey);
		}
		const existing = this.recordFor(accountKey);
		if (options.force !== true && existingToken === account.token && existing !== null) {
			switch (existing.phase) {
				case AccountAccessPhase.ALLOWED:
				case AccountAccessPhase.GATEWAY_STARTING:
					return AccountAccessDecision.ALLOWED;
				case AccountAccessPhase.ACTION_REQUIRED:
					return AccountAccessDecision.ACTION_REQUIRED;
				case AccountAccessPhase.UNAVAILABLE:
					if (Date.now() < existing.nextPollAt) {
						return AccountAccessDecision.UNAVAILABLE;
					}
					break;
				case AccountAccessPhase.UNKNOWN:
				case AccountAccessPhase.CHECKING:
					break;
			}
		}
		if (
			existing === null ||
			(existing.phase !== AccountAccessPhase.ACTION_REQUIRED && existing.phase !== AccountAccessPhase.GATEWAY_STARTING)
		) {
			this.setRecord(accountKey, {
				consecutiveFailures: recheckConsecutiveFailures(options, existing),
				nextPollAt: 0,
				phase: AccountAccessPhase.CHECKING,
				user: existing?.user ?? null,
				wasActionRequired: existing?.wasActionRequired ?? false,
				error: null,
			});
		}
		const controller = new AbortController();
		const token = account.token;
		const promise = this.fetchAccountAccess(accountKey, token, controller.signal).finally(() => {
			const currentRequest = this.requestFor(accountKey);
			if (currentRequest !== null && currentRequest.controller === controller) {
				this.requests.delete(accountKey);
			}
		});
		this.requests.set(accountKey, {controller, promise, token});
		return await promise;
	}

	markGatewayUnavailable(accountKey: string, error: Error): void {
		const account = this.accountFor(accountKey);
		const existing = this.recordFor(accountKey);
		if (!accountAllowsAccessCheck(account)) {
			return;
		}
		if (existing !== null && existing.phase === AccountAccessPhase.ACTION_REQUIRED) {
			return;
		}
		this.checkedTokens.set(accountKey, account.token);
		this.recordUnavailable(accountKey, error);
	}

	markGatewayReady(accountKey: string | null): void {
		if (accountKey === null || accountKey.length === 0) {
			return;
		}
		const existing = this.recordFor(accountKey);
		if (existing === null || !GATEWAY_READY_CLEARS_PHASES.has(existing.phase)) {
			return;
		}
		if (existing.user === null) {
			this.clearRecord(accountKey);
			return;
		}
		this.setRecord(accountKey, {
			...existing,
			consecutiveFailures: 0,
			nextPollAt: 0,
			phase: AccountAccessPhase.ALLOWED,
			wasActionRequired: false,
			error: null,
		});
	}

	forgetAccount(accountKey: string): void {
		const request = this.requestFor(accountKey);
		if (request !== null) {
			request.controller.abort();
		}
		this.requests.delete(accountKey);
		this.clearRecord(accountKey);
	}

	private clearRecord(accountKey: string): void {
		this.records.delete(accountKey);
		this.checkedTokens.delete(accountKey);
		this.revision += 1;
	}

	private recordUnavailable(accountKey: string, error: Error): void {
		const existing = this.recordFor(accountKey);
		const consecutiveFailures = (existing?.consecutiveFailures ?? 0) + 1;
		this.setRecord(accountKey, {
			consecutiveFailures,
			nextPollAt: Date.now() + unavailablePollDelay(consecutiveFailures),
			phase: AccountAccessPhase.UNAVAILABLE,
			user: existing?.user ?? null,
			wasActionRequired: existing?.wasActionRequired ?? false,
			error,
		});
	}

	private accountFor(accountKey: string): Account | null {
		return SessionManager.accounts.find((account) => getAccountKey(account) === accountKey) ?? null;
	}

	private async fetchAccountAccess(
		accountKey: string,
		token: string,
		signal: AbortSignal,
	): Promise<AccountAccessDecision> {
		try {
			const account = this.accountFor(accountKey);
			if (account === null) {
				return AccountAccessDecision.INVALID;
			}
			if (account.instance == null) {
				throw new AccountInstanceUnavailableError(new Error(`Stored account ${accountKey} has no instance runtime`));
			}
			const response = await instanceRequest<UserPrivate>({
				method: 'GET',
				path: Endpoints.USER_ME,
				target: instanceTargetFromSnapshot(account.instance),
				auth: 'none',
				headers: {[Headers.AUTHORIZATION]: token},
				retries: 0,
				signal,
				timeoutMs: ACCOUNT_ACCESS_REQUEST_TIMEOUT_MS,
			});
			const user = response.body;
			if (user.id !== account.userId) {
				throw new AccountAccessUserMismatchError(accountKey);
			}
			const currentAccount = this.accountFor(accountKey);
			if (currentAccount === null || currentAccount.token !== token) {
				return AccountAccessDecision.UNAVAILABLE;
			}
			const existing = this.recordFor(accountKey);
			const actionRequired = userRequiresAction(user);
			const wasActionRequired = actionRequired || (existing?.wasActionRequired ?? false);
			runInAction(() => {
				this.checkedTokens.set(accountKey, token);
				this.setRecord(accountKey, {
					consecutiveFailures: 0,
					nextPollAt: actionRequiredNextPollAt(actionRequired),
					phase: checkedAccountPhase(actionRequired, wasActionRequired),
					user,
					wasActionRequired,
					error: null,
				});
			});
			return checkedAccountDecision(actionRequired);
		} catch (error) {
			if (signal.aborted) {
				return AccountAccessDecision.UNAVAILABLE;
			}
			if (
				error instanceof AccountAccessUserMismatchError ||
				(error instanceof HttpError && error.status === HttpStatus.UNAUTHORIZED)
			) {
				this.invalidateAccount(accountKey, token);
				return AccountAccessDecision.INVALID;
			}
			const accessError = normalizeAccountAccessError(error);
			logger.warn('Account access check failed', accessError);
			this.recordUnavailable(accountKey, accessError);
			return AccountAccessDecision.UNAVAILABLE;
		}
	}

	private invalidateAccount(accountKey: string, token: string): void {
		const account = this.accountFor(accountKey);
		runInAction(() => {
			if (account !== null) {
				SessionManager.markAccountInvalid(accountKey, token);
			}
			this.forgetAccount(accountKey);
		});
	}

	private recordFor(accountKey: string): AccountAccessRecord | null {
		return this.records.get(accountKey) ?? null;
	}

	private requestFor(accountKey: string): AccountAccessRequest | null {
		return this.requests.get(accountKey) ?? null;
	}

	private tokenCheckedFor(accountKey: string): string | null {
		return this.checkedTokens.get(accountKey) ?? null;
	}

	private setRecord(accountKey: string, record: AccountAccessRecord): void {
		this.records.set(accountKey, record);
		this.revision += 1;
	}
}

export default new AccountAccess();
