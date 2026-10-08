// SPDX-License-Identifier: AGPL-3.0-or-later

import i18nGlobal from '@app/app/I18n';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as RecoveryKitCommands from '@app/features/user/commands/RecoveryKitCommands';
import RecoveryKitStatus from '@app/features/user/state/RecoveryKitStatus';
import Users from '@app/features/user/state/Users';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const logger = new Logger('RecoveryKitReminderGate');

const PROMPT_DELAY_MS = 5000;

const PROMPT_TITLE_DESCRIPTOR = msg({
	message: 'Save a recovery kit',
	comment: 'Title of the one-time prompt asking a signed-in user without a recovery kit to create one.',
});
const PROMPT_BODY_DESCRIPTOR = msg({
	message:
		'Your account has no email address. If you forget your password, a recovery kit is the only way back in. It only takes a minute.',
	comment: 'Body of the one-time prompt asking a signed-in user without a recovery kit to create one.',
});
const PROMPT_CREATE_DESCRIPTOR = msg({
	message: 'Create recovery kit',
	comment: 'Primary button in the one-time recovery kit prompt.',
});
const PROMPT_LATER_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Secondary button that closes the one-time recovery kit prompt.',
});

function openRecoveryKitPrompt(): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18nGlobal._(PROMPT_TITLE_DESCRIPTOR)}
				description={i18nGlobal._(PROMPT_BODY_DESCRIPTOR)}
				primaryText={i18nGlobal._(PROMPT_CREATE_DESCRIPTOR)}
				secondaryText={i18nGlobal._(PROMPT_LATER_DESCRIPTOR)}
				onPrimary={async () => {
					try {
						await RecoveryKitCommands.createAndShowRecoveryKit();
					} catch (error) {
						FormUtils.pushApiErrorModal(i18nGlobal, error);
					}
				}}
				data-flx="user.recovery-kit-reminder-gate.confirm-modal"
			/>
		)),
	);
}

export const RecoveryKitReminderGate = observer(() => {
	const user = Users.currentUser;
	const userId = user?.id ?? null;
	const eligible = RuntimeConfig.usesUsernameSignIn && Boolean(user?.isClaimed());
	useEffect(() => {
		if (!eligible || !userId || RecoveryKitStatus.hasShownPrompt(userId)) return;
		const timer = window.setTimeout(async () => {
			try {
				const status = await RecoveryKitCommands.fetchRecoveryKitStatus();
				if (status.has_recovery_kit || Users.currentUserId !== userId || RecoveryKitStatus.hasShownPrompt(userId)) {
					return;
				}
				RecoveryKitStatus.markPromptShown(userId);
				openRecoveryKitPrompt();
			} catch (error) {
				logger.warn('Failed to check recovery kit status', error);
			}
		}, PROMPT_DELAY_MS);
		return () => window.clearTimeout(timer);
	}, [eligible, userId]);
	return null;
});
