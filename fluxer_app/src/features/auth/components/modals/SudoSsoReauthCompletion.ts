// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import SudoPrompt from '@app/features/auth/state/SudoPrompt';

export async function finishSsoReauthInThisWindow(
	payload: LoginSuccessPayload,
	currentUserId: string | null,
): Promise<boolean> {
	if (currentUserId == null || payload.userId !== currentUserId) {
		return false;
	}
	await AuthenticationCommands.completeLogin(
		{...payload, runtimeSnapshot: RuntimeConfig.getSnapshot()},
		{redirectPath: null},
	);
	SudoPrompt.submit({});
	return true;
}
