// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {promptForAccountTier} from '@app/features/auth/utils/AccountTierPrompt';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';

const REPORT_ACCOUNT_SETUP_CODES: ReadonlySet<string> = new Set([
	APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED,
	APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED,
	APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS,
]);

export function needsReportAccountSetup(error: unknown): boolean {
	const code = failureCode(error);
	return code !== undefined && REPORT_ACCOUNT_SETUP_CODES.has(code);
}

export function openReportAccountSetup(error: unknown): void {
	if (promptForAccountTier(error)) return;
	if (failureCode(error) === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS) {
		openClaimAccountModal();
		return;
	}
	ModalCommands.push(
		modal(
			() => (
				<UserSettingsModal
					initialTab="account_security"
					data-flx="moderation.report-verification-gate.open-report-account-setup.user-settings-modal"
				/>
			),
			'user-settings',
		),
	);
}
