// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {PrivacySetupModal} from '@app/features/user/components/modals/PrivacySetupModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const PRIVACY_SETUP_MESSAGE_DESCRIPTOR = msg({
	message: 'Choose who can send you direct messages.',
	comment: 'Nagbar body asking the user to review who can send them direct messages.',
});
const REVIEW_PRIVACY_SETTINGS_DESCRIPTOR = msg({
	message: 'Review privacy settings',
	comment: 'Button label on the privacy setup nagbar. Opens the privacy setup modal.',
});
export const PrivacySetupNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<PrivacySetupModal data-flx="app.app-layout.nagbars.privacy-setup-nagbar.handle-open.privacy-setup-modal" />
			)),
		);
	}, []);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			data-flx="app.app-layout.nagbars.privacy-setup-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(PRIVACY_SETUP_MESSAGE_DESCRIPTOR)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpen}
						data-flx="app.app-layout.nagbars.privacy-setup-nagbar.nagbar-button.open"
					>
						{i18n._(REVIEW_PRIVACY_SETTINGS_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.privacy-setup-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
