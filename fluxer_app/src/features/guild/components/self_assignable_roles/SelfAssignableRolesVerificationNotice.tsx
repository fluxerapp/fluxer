// SPDX-License-Identifier: AGPL-3.0-or-later

import {CountdownTimer} from '@app/features/channel/components/barriers/BarrierComponents';
import {VerificationFailureReason, type VerificationStatus} from '@app/features/guild/state/GuildVerification';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {AccountGateButton} from '@app/features/user/utils/AccountGatePresentation';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const CANT_PICK_ROLES_YET_DESCRIPTOR = msg({
	message: "You can't pick roles yet",
	comment:
		"Title of the notice on the Roles page when the member doesn't meet the community's verification level yet. The roles are shown but can't be picked.",
});
const ACCOUNT_TOO_NEW_DESCRIPTOR = msg({
	message: 'Your account is too new to pick roles in this community.',
	comment:
		'Notice on the Roles page when the community requires accounts to be a few minutes old. A countdown next to it shows when the member can pick roles.',
});
const NOT_MEMBER_LONG_ENOUGH_DESCRIPTOR = msg({
	message: "You haven't been a member of this community long enough to pick roles.",
	comment:
		'Notice on the Roles page when the community requires members to have joined a few minutes ago. A countdown next to it shows when the member can pick roles.',
});
const FINISH_ACCOUNT_DESCRIPTOR = msg({
	message: 'Finish setting up your account to pick roles in this community.',
	comment:
		'Notice on the Roles page when the community requires a finished account. A button next to it starts the missing step, such as adding or confirming an email.',
});

interface SelfAssignableRolesVerificationNoticeProps {
	readonly status: VerificationStatus;
}

export const SelfAssignableRolesVerificationNotice: React.FC<SelfAssignableRolesVerificationNoticeProps> = observer(
	function SelfAssignableRolesVerificationNotice({status}) {
		const {i18n} = useLingui();
		if (status.canAccess) return null;
		const timeRemaining = status.verificationEndsAt ? Math.max(0, status.verificationEndsAt - Date.now()) : 0;
		let description: string;
		let actions: React.ReactNode = null;
		switch (status.reason) {
			case VerificationFailureReason.ACCOUNT_TOO_NEW:
				description = i18n._(ACCOUNT_TOO_NEW_DESCRIPTOR);
				break;
			case VerificationFailureReason.NOT_MEMBER_LONG_ENOUGH:
				description = i18n._(NOT_MEMBER_LONG_ENOUGH_DESCRIPTOR);
				break;
			default:
				description = i18n._(FINISH_ACCOUNT_DESCRIPTOR);
				if (status.accountGate)
					actions = (
						<AccountGateButton
							gate={status.accountGate}
							data-flx="guild.self-assignable-roles.self-assignable-roles-verification-notice.account-gate-action"
						/>
					);
		}
		if (!actions && timeRemaining > 0)
			actions = (
				<CountdownTimer
					initialTime={timeRemaining}
					data-flx="guild.self-assignable-roles.self-assignable-roles-verification-notice.countdown-timer"
				/>
			);
		return (
			<WarningAlert
				title={i18n._(CANT_PICK_ROLES_YET_DESCRIPTOR)}
				actions={actions}
				data-flx="guild.self-assignable-roles.self-assignable-roles-verification-notice.warning-alert"
			>
				{description}
			</WarningAlert>
		);
	},
);
