// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import Users from '@app/features/user/state/Users';
import {
	getUnclaimedAccountPresentation,
	UnclaimedAccountSurfaces,
} from '@app/features/user/utils/AccountGatePresentation';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SIGN_OUT_DESCRIPTOR = msg({
	message: 'Sign out?',
	comment: 'Question prompt in the settings dialog logout modal. Keep the tone plain and specific.',
});
const SIGN_OUT_2_DESCRIPTOR = msg({
	message: 'Sign out',
	comment: 'Short label in the settings dialog logout modal. Keep the tone plain and specific.',
});
const STAY_SIGNED_IN_DESCRIPTOR = msg({
	message: 'Stay signed in',
	comment: 'Short label in the settings dialog logout modal.',
});
const SIGN_OUT_ANYWAY_DESCRIPTOR = msg({
	message: 'Sign out anyway',
	comment: 'Button in the sign out dialog for an account that has no way to sign back in.',
});
export const LogoutModal = observer(() => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	if (user?.isClaimed() === false && user.dateOfBirthRequired) {
		return (
			<ConfirmModal
				title={i18n._(SIGN_OUT_DESCRIPTOR)}
				description={
					<Trans comment="Sign out dialog text for an unclaimed account that has no way to sign back in after signing out.">
						This account is unclaimed, so you can't sign back in to it.
					</Trans>
				}
				primaryText={i18n._(SIGN_OUT_ANYWAY_DESCRIPTOR)}
				primaryVariant="danger"
				secondaryText={i18n._(STAY_SIGNED_IN_DESCRIPTOR)}
				onPrimary={() => AuthenticationCommands.logout()}
				data-flx="app.logout-modal.unclaimed-confirm-modal--date-of-birth"
			/>
		);
	}
	if (user?.isClaimed() === false) {
		const presentation = getUnclaimedAccountPresentation(
			i18n,
			UnclaimedAccountSurfaces.SIGN_OUT,
			user.verificationNeed,
		);
		return (
			<ConfirmModal
				title={i18n._(SIGN_OUT_DESCRIPTOR)}
				description={presentation.message}
				primaryText={i18n._(SIGN_OUT_ANYWAY_DESCRIPTOR)}
				primaryVariant="danger"
				secondaryText={presentation.actionLabel}
				onSecondary={() => openClaimAccountModal()}
				onPrimary={() => AuthenticationCommands.logout()}
				data-flx="app.logout-modal.unclaimed-confirm-modal"
			/>
		);
	}
	return (
		<ConfirmModal
			title={i18n._(SIGN_OUT_DESCRIPTOR)}
			description={<Trans>You can sign back in at any time.</Trans>}
			primaryText={i18n._(SIGN_OUT_2_DESCRIPTOR)}
			secondaryText={i18n._(STAY_SIGNED_IN_DESCRIPTOR)}
			onPrimary={() => AuthenticationCommands.logout()}
			data-flx="app.logout-modal.confirm-modal"
		/>
	);
});
