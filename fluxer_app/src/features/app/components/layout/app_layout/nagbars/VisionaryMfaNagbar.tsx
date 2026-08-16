// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {ENABLE_TWO_FACTOR_AUTH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const VISIONARY_MFA_MESSAGE_DESCRIPTOR = msg({
	message: 'Protect your Visionary account by enabling two-factor authentication. Thanks for supporting {productName}!',
	comment:
		'Nagbar body shown to Visionary premium users who have not enabled two-factor authentication. {productName} is Fluxer.',
});
export const VisionaryMfaNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const handleEnableMfa = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="account_security"
					initialSubtab="security"
					data-flx="app.app-layout.nagbars.visionary-mfa-nagbar.handle-enable-mfa.user-settings-modal"
				/>
			)),
		);
	}, []);
	const handleDismiss = useCallback(() => {
		NagbarCommands.dismissNagbar('visionaryMfaDismissed');
	}, []);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.visionary-mfa-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(VISIONARY_MFA_MESSAGE_DESCRIPTOR, {productName: PRODUCT_NAME})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleEnableMfa}
						data-flx="app.app-layout.nagbars.visionary-mfa-nagbar.nagbar-button.enable-mfa"
					>
						{i18n._(ENABLE_TWO_FACTOR_AUTH_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.visionary-mfa-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
