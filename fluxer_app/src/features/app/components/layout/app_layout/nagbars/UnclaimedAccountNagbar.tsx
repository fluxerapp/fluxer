// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import NagbarState from '@app/features/ui/state/Nagbar';
import {snoozeUnclaimedNagbar} from '@app/features/ui/state/UnclaimedNagbarSnooze';
import Users from '@app/features/user/state/Users';
import {
	getUnclaimedAccountPresentation,
	UnclaimedAccountSurfaces,
} from '@app/features/user/utils/AccountGatePresentation';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const UnclaimedAccountNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const presentation = getUnclaimedAccountPresentation(
		i18n,
		UnclaimedAccountSurfaces.NAGBAR,
		Users.currentUser?.verificationNeed,
	);
	const handleDismiss = () => {
		const userId = Users.currentUser?.id;
		if (userId) snoozeUnclaimedNagbar(userId);
		NagbarState.dismissUnclaimedAccountForSession();
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.NEUTRAL].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.NEUTRAL].textColor}
			dismissible={true}
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.unclaimed-account-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={presentation.message}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={() => openClaimAccountModal()}
						data-flx="app.app-layout.nagbars.unclaimed-account-nagbar.nagbar-button.claim-account"
					>
						{presentation.actionLabel}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.unclaimed-account-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
