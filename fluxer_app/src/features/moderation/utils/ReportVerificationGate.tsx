// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';

export function canSubmitReport(): boolean {
	const user = Users.currentUser;
	if (!user) return false;
	if (!user.isClaimed()) return false;
	if (!user.verified) return false;
	return true;
}

export function showReportRestrictionDialog(): void {
	const user = Users.currentUser;
	if (!user) return;
	if (!user.isClaimed()) {
		openClaimAccountModal({force: true});
		return;
	}
	if (!user.verified) {
		ModalCommands.push(
			modal(
				() => (
					<UserSettingsModal
						initialTab="account_security"
						data-flx="moderation.report-verification-gate.show-report-restriction-dialog.user-settings-modal"
					/>
				),
				'user-settings',
			),
		);
	}
}
