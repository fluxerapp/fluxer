// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {CUSTOM_CSS_IS_OFF_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const TURN_ON_DESCRIPTOR = msg({
	message: 'Turn on',
	comment: 'Nagbar button that applies quick CSS and saved themes again.',
});

export const CustomCssOffNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.NEUTRAL].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.NEUTRAL].textColor}
			data-flx="app.app-layout.nagbars.custom-css-off-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(CUSTOM_CSS_IS_OFF_DESCRIPTOR)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={() => ThemeSettingsState.setCustomCssOff(false)}
						data-flx="app.app-layout.nagbars.custom-css-off-nagbar.turn-on-button"
					>
						{i18n._(TURN_ON_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.custom-css-off-nagbar.content"
			/>
		</Nagbar>
	);
});
