// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';

export function resolveForgotPasswordRedirect(runtimeSnapshot: RuntimeConfigSnapshot): string | null {
	if (usesUsernameSignIn(runtimeSnapshot.features)) {
		return Routes.RECOVER_ACCOUNT;
	}
	return runtimeSnapshot.features.emails_enabled ? null : Routes.LOGIN;
}

export function resolveMissingResetTokenRedirect(runtimeSnapshot: RuntimeConfigSnapshot): string {
	return resolveForgotPasswordRedirect(runtimeSnapshot) ?? Routes.FORGOT_PASSWORD;
}
