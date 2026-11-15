// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {redeemAccountHandoff} from '@app/features/auth/commands/AccountEntryCommands';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {readyBrowserAuthRuntime} from '@app/features/auth/flow/AuthRuntimeTargetBoundary';
import {
	clearCapturedAccountHandoff,
	endAccountHandoffArrival,
	isAccountHandoffArrival,
	readCapturedAccountHandoff,
} from '@app/features/auth/state/AccountHandoffCapture';
import type {UserData} from '@app/features/auth/state/AccountStorageContract';
import Accounts from '@app/features/auth/state/Accounts';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {openGuildWhenAvailable} from '@app/features/guild/commands/GuildVisitCommands';
import {getDefaultLandingPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode, failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {formatUserTag} from '@app/features/user/utils/UserTagUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';

const logger = new Logger('AccountHandoffRedeem');

const RUNTIME_TIMEOUT_MS = 10_000;

const TERMINAL_HANDOFF_CODES = new Set<string>([
	APIErrorCodes.UNKNOWN_ACCOUNT_HANDOFF,
	APIErrorCodes.INVALID_ACCOUNT_HANDOFF_NONCE,
]);

export type AccountHandoffOutcome =
	| {readonly kind: 'signed_in'}
	| {readonly kind: 'retry'; readonly error: unknown}
	| {readonly kind: 'expired'}
	| {readonly kind: 'failed'; readonly error: unknown}
	| {
			readonly kind: 'choose';
			readonly payload: LoginSuccessPayload;
			readonly entry: GuildEntryResult | null;
			readonly current: string;
	  };

export function accountLabel(user: UserData | undefined, fallback: string): string {
	return user == null || user.username === '' ? fallback : formatUserTag(user);
}

function isRetryable(error: unknown): boolean {
	if (failureCode(error) === APIErrorCodes.CONTENT_BLOCKED) return true;
	return (
		failureValidationErrors(error)?.some(
			(fault) =>
				fault.path === 'global_name' ||
				(fault.path === 'consent' && fault.code === ValidationErrorCodes.MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY),
		) === true
	);
}

export function finishAccountHandoffArrival(): void {
	const arrived = isAccountHandoffArrival(window.location.pathname);
	endAccountHandoffArrival();
	if (arrived) RouterUtils.replaceWith(getDefaultLandingPath());
}

export async function redeemCapturedAccountHandoff(
	globalName: string,
	snapshot: RuntimeConfigSnapshot,
): Promise<AccountHandoffOutcome> {
	const handoff = readCapturedAccountHandoff();
	if (handoff === null) return {kind: 'expired'};
	try {
		const {payload, entry} = await redeemAccountHandoff({
			handoffCode: handoff.handoffCode,
			nonce: handoff.nonce,
			consent: true,
			globalName,
			snapshot,
		});
		clearCapturedAccountHandoff();
		const current = Accounts.currentAccount;
		if (current !== null) {
			return {kind: 'choose', payload, entry, current: accountLabel(current.userData, current.userId)};
		}
		await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot: snapshot}, {redirectPath: Routes.ME});
		finishAccountHandoffArrival();
		if (entry !== null) void openGuildWhenAvailable(entry.guild_id);
		return {kind: 'signed_in'};
	} catch (error) {
		if (isRetryable(error)) return {kind: 'retry', error};
		clearCapturedAccountHandoff();
		const code = failureCode(error);
		if (code !== undefined && TERMINAL_HANDOFF_CODES.has(code)) {
			logger.warn('Account handoff could not be redeemed', error);
			return {kind: 'expired'};
		}
		return {kind: 'failed', error};
	}
}

async function redeemOnArrival(): Promise<AccountHandoffOutcome> {
	const name = readCapturedAccountHandoff()?.name ?? '';
	let snapshot: RuntimeConfigSnapshot;
	try {
		snapshot = await readyBrowserAuthRuntime(AbortSignal.timeout(RUNTIME_TIMEOUT_MS));
	} catch (error) {
		logger.warn('The instance for the account handoff could not be resolved', error);
		return {kind: 'failed', error};
	}
	return redeemCapturedAccountHandoff(name, snapshot);
}

let arrival: Promise<AccountHandoffOutcome> | null = null;

export function redeemAccountHandoffOnArrival(): Promise<AccountHandoffOutcome> {
	arrival ??= redeemOnArrival();
	return arrival;
}
