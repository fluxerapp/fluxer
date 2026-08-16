// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {OPEN_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const EMAIL_VERIFICATION_NAGBAR_MESSAGE_DESCRIPTOR = msg({
	message: 'Hey {displayName}, please verify your email address.',
	comment:
		'Nagbar warning shown when the current user still needs to verify email. displayName is the user display name.',
});
export const EmailVerificationNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	if (!user) {
		return null;
	}
	const openUserSettings = () => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="account_security"
					data-flx="app.app-layout.nagbars.email-verification-nagbar.open-user-settings.user-settings-modal"
				/>
			)),
		);
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.ALERT].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.ALERT].textColor}
			data-flx="app.app-layout.nagbars.email-verification-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(EMAIL_VERIFICATION_NAGBAR_MESSAGE_DESCRIPTOR, {
					displayName: NicknameUtils.getDisplayName(user),
				})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={openUserSettings}
						data-flx="app.app-layout.nagbars.email-verification-nagbar.nagbar-button.open-user-settings"
					>
						{i18n._(OPEN_SETTINGS_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.email-verification-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
