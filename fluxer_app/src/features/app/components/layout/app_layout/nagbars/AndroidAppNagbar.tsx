// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {GOOGLE_PLAY_NAME, GOOGLE_PLAY_URL} from '@app/features/app/config/I18nDisplayConstants';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const ANDROID_APP_MESSAGE_DESCRIPTOR = msg({
	message: 'You can get the Fluxer app for Android on {googlePlay} and chat from your phone.',
	comment:
		'Nagbar body telling people that the Fluxer app for Android phones can be installed from Google Play. {googlePlay} is the store brand name Google Play and must stay as is. Keep Fluxer and Android untranslated. Keep the tone calm and plain.',
});
const ANDROID_APP_BUTTON_DESCRIPTOR = msg({
	message: 'Get it on {googlePlay}',
	comment:
		'Button in the Android app nagbar that opens the Fluxer listing on Google Play. {googlePlay} is the store brand name Google Play and must stay as is. Use the wording Google Play uses for its own "Get it on Google Play" badge in this language when one exists. Keep it short.',
});

export const AndroidAppNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const handleOpen = () => {
		NagbarCommands.dismissNagbar('androidAppDismissed');
		openExternalUrl(GOOGLE_PLAY_URL);
	};
	const handleDismiss = () => {
		NagbarCommands.dismissNagbar('androidAppDismissed');
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.android-app-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(ANDROID_APP_MESSAGE_DESCRIPTOR, {googlePlay: GOOGLE_PLAY_NAME})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpen}
						data-flx="app.app-layout.nagbars.android-app-nagbar.nagbar-button.open"
					>
						{i18n._(ANDROID_APP_BUTTON_DESCRIPTOR, {googlePlay: GOOGLE_PLAY_NAME})}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.android-app-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
