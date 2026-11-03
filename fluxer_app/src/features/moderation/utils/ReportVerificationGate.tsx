// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {promptForAccountGate} from '@app/features/auth/utils/AccountTierPrompt';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {type AccountGateResult, canPerform} from '@app/features/user/utils/AccountGates';
import {AccountTiers, TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';

export function reportGate(): AccountGateResult {
	return canPerform(Users.currentUser, TierGatedActions.REPORT);
}

export function showReportRestrictionDialog(): boolean {
	const gate = reportGate();
	if (gate.allowed) return false;
	if (promptForAccountGate(gate)) return true;
	if (gate.tier === AccountTiers.UNCLAIMED) {
		openClaimAccountModal();
		return true;
	}
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
	return true;
}
