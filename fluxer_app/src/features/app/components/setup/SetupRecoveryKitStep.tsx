// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import {Note, StepHeader} from '@app/features/app/components/setup/SetupWizardSteps';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as RecoveryKitCommands from '@app/features/user/commands/RecoveryKitCommands';
import {confirmNewRecoveryKit} from '@app/features/user/components/modals/tabs/account_security_tab/RecoveryKitSettings';
import Users from '@app/features/user/state/Users';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import {makeAutoObservable} from 'mobx';
import {observer} from 'mobx-react-lite';
import {type RefObject, useCallback} from 'react';

const logger = new Logger('SelfHostedSetupWizardGate');

const RECOVERY_KIT_TITLE_DESCRIPTOR = msg({
	message: 'Save your recovery kit',
	comment: 'Setup wizard title asking the administrator to save a recovery kit.',
});
const RECOVERY_KIT_BODY_DESCRIPTOR = msg({
	message:
		"This instance doesn't use email, and you're the only administrator. If you forget your password, your recovery kit is the only way back into your account.",
	comment: 'Setup wizard body explaining why the administrator needs a recovery kit.',
});
const RECOVERY_KIT_CREATE_DESCRIPTOR = msg({
	message: 'Create recovery kit',
	comment: 'Button that creates a recovery kit in the setup wizard.',
});
const RECOVERY_KIT_RECREATE_DESCRIPTOR = msg({
	message: 'Create a new kit',
	comment: 'Button that replaces the recovery kit in the setup wizard.',
});
const RECOVERY_KIT_HELP_DESCRIPTOR = msg({
	message: 'Keep a copy somewhere other than this computer, like a password manager.',
	comment: 'Advice on where to keep the recovery kit, in the setup wizard.',
});
const RECOVERY_KIT_SAVED_DESCRIPTOR = msg({
	message: 'You saved a recovery kit.',
	comment: 'Status in the setup wizard once the administrator has a recovery kit.',
});

export class SetupRecoveryKitDraft {
	creation: 'idle' | 'pending' = 'idle';
	stored = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	beginCreation(): void {
		this.creation = 'pending';
		this.stored = false;
	}

	acknowledge(): void {
		this.stored = true;
	}

	finishCreation(): void {
		this.creation = 'idle';
	}
}

export const RecoveryKitStep = observer(
	({
		draft,
		currentUserId,
		hasRecoveryKit,
		adminPasswordRef,
	}: {
		draft: SetupRecoveryKitDraft;
		currentUserId: string | null;
		hasRecoveryKit: boolean;
		adminPasswordRef: RefObject<string | null>;
	}) => {
		const {i18n} = useLingui();
		const createRecoveryKit = useCallback(async () => {
			draft.beginCreation();
			const onDone = () => draft.acknowledge();
			try {
				const password = adminPasswordRef.current;
				if (password && currentUserId) {
					await RecoveryKitCommands.createRecoveryKitWithPasswordAndOpen({
						userId: currentUserId,
						password,
						username: Users.getCurrentUser()?.username,
						onDone,
					});
				} else {
					await RecoveryKitCommands.createAndShowRecoveryKit(onDone);
				}
			} catch (error) {
				logger.error('Failed to create the administrator recovery kit', error);
				FormUtils.pushApiErrorModal(i18n, error);
			} finally {
				draft.finishCreation();
			}
		}, [currentUserId, i18n, draft, adminPasswordRef]);

		const handleCreateRecoveryKit = useCallback(() => {
			if (!hasRecoveryKit) {
				void createRecoveryKit();
				return;
			}
			confirmNewRecoveryKit(i18n, createRecoveryKit);
		}, [hasRecoveryKit, i18n, createRecoveryKit]);

		const {stored, creation} = draft;
		return (
			<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.recovery-kit-step">
				<StepHeader
					title={i18n._(RECOVERY_KIT_TITLE_DESCRIPTOR)}
					body={i18n._(RECOVERY_KIT_BODY_DESCRIPTOR)}
					data-flx="app.setup.setup-wizard-steps.recovery-kit-step.step-header"
				/>
				{stored && (
					<p className={styles.success} role="status" data-flx="app.self-hosted-setup-wizard-gate.recovery-kit-saved">
						<CheckCircleIcon
							size={20}
							weight="fill"
							aria-hidden="true"
							data-flx="app.setup.setup-wizard-steps.recovery-kit-step.check-circle-icon"
						/>
						{i18n._(RECOVERY_KIT_SAVED_DESCRIPTOR)}
					</p>
				)}
				<div className={styles.actionRow} data-flx="app.self-hosted-setup-wizard-gate.recovery-kit-actions">
					<Button
						variant={stored ? 'secondary' : 'primary'}
						submitting={creation === 'pending'}
						onClick={handleCreateRecoveryKit}
						data-flx="app.self-hosted-setup-wizard-gate.recovery-kit-create"
					>
						{i18n._(stored ? RECOVERY_KIT_RECREATE_DESCRIPTOR : RECOVERY_KIT_CREATE_DESCRIPTOR)}
					</Button>
				</div>
				<Note data-flx="app.setup.setup-wizard-steps.recovery-kit-step.note">
					{i18n._(RECOVERY_KIT_HELP_DESCRIPTOR)}
				</Note>
			</section>
		);
	},
);
