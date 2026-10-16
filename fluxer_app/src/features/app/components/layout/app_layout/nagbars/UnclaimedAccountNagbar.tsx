// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {CLAIM_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const CLAIM_ACCOUNT_NAGBAR_MESSAGE_DESCRIPTOR = msg({
	message: 'Hey {displayName}, claim your account to prevent losing access.',
	comment: 'Nagbar warning shown to unclaimed-account users. displayName is the current user display name.',
});
export const UnclaimedAccountNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	if (!user) {
		return null;
	}
	const handleClaimAccount = () => {
		openClaimAccountModal({force: true});
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.ALERT].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.ALERT].textColor}
			data-flx="app.app-layout.nagbars.unclaimed-account-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(CLAIM_ACCOUNT_NAGBAR_MESSAGE_DESCRIPTOR, {
					displayName: NicknameUtils.getDisplayName(user),
				})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleClaimAccount}
						data-flx="app.app-layout.nagbars.unclaimed-account-nagbar.nagbar-button.claim-account"
					>
						{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.unclaimed-account-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
