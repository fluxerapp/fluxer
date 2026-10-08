// SPDX-License-Identifier: AGPL-3.0-or-later

import {validateResetPasswordToken} from '@app/features/auth/commands/AuthenticationCommands';
import type {AuthRequestTarget} from '@app/features/auth/state/AuthRequestTarget';

export type ResetPasswordTokenStatus = 'valid' | 'invalid';

interface SettledResetPasswordTokenCheck {
	readonly token: string;
	readonly instanceKey: string;
	readonly status: ResetPasswordTokenStatus;
}

let settled: SettledResetPasswordTokenCheck | null = null;

export async function checkResetPasswordToken(
	token: string,
	target: AuthRequestTarget,
): Promise<ResetPasswordTokenStatus> {
	try {
		return (await validateResetPasswordToken(token, target)) ? 'valid' : 'invalid';
	} catch {
		return 'invalid';
	}
}

export async function warmResetPasswordTokenCheck(token: string, target: AuthRequestTarget): Promise<void> {
	const status = await checkResetPasswordToken(token, target);
	settled = {token, instanceKey: target.http.instanceKey, status};
}

export function peekSettledResetPasswordTokenCheck(
	token: string,
	target: AuthRequestTarget,
): ResetPasswordTokenStatus | null {
	if (settled === null || settled.token !== token || settled.instanceKey !== target.http.instanceKey) {
		return null;
	}
	return settled.status;
}

export function takeSettledResetPasswordTokenCheck(
	token: string,
	target: AuthRequestTarget,
): ResetPasswordTokenStatus | null {
	const status = peekSettledResetPasswordTokenCheck(token, target);
	settled = null;
	return status;
}
