// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const STREAMER_MODE_ENABLED_USERNAMES_TRUNCATED_DESCRIPTOR = msg({
	message: 'Streaming privacy is on. Names are shortened.',
	comment: 'Nagbar message shown while streaming privacy is active.',
});
const DISABLE_DESCRIPTOR = msg({
	message: 'Turn off',
	comment: 'Button label that turns off streaming privacy.',
});

export const StreamerModeNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.STREAMER].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.STREAMER].textColor}
			dismissible
			onDismiss={StreamerMode.dismissNagbar}
			data-flx="app.app-layout.nagbars.streamer-mode-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(STREAMER_MODE_ENABLED_USERNAMES_TRUNCATED_DESCRIPTOR)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={StreamerMode.disable}
						data-flx="app.app-layout.nagbars.streamer-mode-nagbar.disable-button"
					>
						{i18n._(DISABLE_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.streamer-mode-nagbar.content"
			/>
		</Nagbar>
	);
});
