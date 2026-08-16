// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import Config from '@app/features/app/config/Config';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import NagbarController from '@app/features/ui/state/Nagbar';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const BUILD_ENVIRONMENT_MESSAGE_DESCRIPTOR = msg({
	message: 'You are using a {releaseChannel} build of {productName}.',
	comment:
		'Session-dismissible nagbar identifying a non-stable official desktop app build. {releaseChannel} is development, staging, or canary. {productName} is the fixed official desktop product name, not the active instance name.',
});

export function BuildEnvironmentNagbar({isMobile}: {isMobile: boolean}) {
	const {i18n} = useLingui();
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.DEVELOPMENT].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.DEVELOPMENT].textColor}
			dismissible
			onDismiss={NagbarController.dismissBuildEnvironmentNagbar}
			data-flx="app.app-layout.nagbars.build-environment-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={NagbarController.dismissBuildEnvironmentNagbar}
				message={i18n._(BUILD_ENVIRONMENT_MESSAGE_DESCRIPTOR, {
					releaseChannel: Config.PUBLIC_RELEASE_CHANNEL,
					productName: PRODUCT_NAME,
				})}
				data-flx="app.app-layout.nagbars.build-environment-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
}
