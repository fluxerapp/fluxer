// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {CLAIM_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const UnclaimedAccountAlert = observer(() => {
	const {i18n} = useLingui();
	return (
		<WarningAlert
			title={<Trans>Unclaimed account</Trans>}
			actions={
				<Button
					small={true}
					onClick={() => openClaimAccountModal({force: true})}
					data-flx="app.unclaimed-account-alert.button.open-claim-account-modal"
				>
					{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
				</Button>
			}
			data-flx="app.unclaimed-account-alert.warning-alert"
		>
			{RuntimeConfig.usesUsernameSignIn ? (
				<Trans>
					Your account is not yet claimed. Without a username and password, you won't be able to sign in from other
					devices and you could lose access to your account. Claim your account now to secure it.
				</Trans>
			) : (
				<Trans>
					Your account is not yet claimed. Without an email and password, you won't be able to sign in from other
					devices and you could lose access to your account. Claim your account now to secure it.
				</Trans>
			)}
		</WarningAlert>
	);
});
