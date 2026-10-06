// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {
	installDomainMovedApp,
	openDomainMovedBrowserMigration,
} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import type {DomainMigrationInstallKind} from '@app/features/app/domain_migration/DomainMigrationCore';
import DomainMovedNotice from '@app/features/app/domain_migration/DomainMovedNotice';
import {
	type DomainMovedStepsPlatform,
	showDomainMovedStepsModal,
} from '@app/features/app/domain_migration/DomainMovedStepsModal';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {isIOSMobileOrTabletUserAgent} from '@app/features/platform/notifications/NotificationAlertOptions';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

type DomainMovedPresentation = 'install' | 'browser' | 'apple' | 'generic';

const INSTALL_MESSAGE_DESCRIPTOR = msg({
	message: '{productName} has moved to {host}. Install the new app and you will already be signed in.',
	comment:
		'Banner in an installed desktop web app after the account moved to the new domain. productName is the app name. host is the new domain, for example fluxer.com.',
});
const BROWSER_MESSAGE_DESCRIPTOR = msg({
	message: '{productName} has moved to {host}. Open it in your browser to install the new app.',
	comment:
		'Banner in an installed Android web app. productName is the app name. host is the new domain, for example fluxer.com.',
});
const APPLE_MESSAGE_DESCRIPTOR = msg({
	message: '{productName} has moved to {host}. Add it to your Home Screen or Dock, then sign in with this app.',
	comment:
		'Banner in a web app installed on iPhone, iPad or Mac. productName is the app name. host is the new domain, for example fluxer.com. Home Screen and Dock are Apple names.',
});
const GENERIC_MESSAGE_DESCRIPTOR = msg({
	message: '{productName} has moved to {host}. Install it from your browser, then sign in with this app.',
	comment:
		'Banner in an installed web app on other browsers. productName is the app name. host is the new domain, for example fluxer.com.',
});
const INSTALL_NEW_APP_DESCRIPTOR = msg({
	message: 'Install the new app',
	comment: 'Button on the domain moved banner that installs the app from the new domain.',
});
const OPEN_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open {productName} in your browser',
	comment: 'Button on the domain moved banner that opens the new domain in the browser. productName is the app name.',
});
const SHOW_ME_HOW_DESCRIPTOR = msg({
	message: 'Show me how',
	comment: 'Button on the domain moved banner that opens the install steps.',
});
const LINK_NEW_DEVICE_DESCRIPTOR = msg({
	message: 'Link a new device',
	comment:
		'Button on the domain moved banner that opens the code entry used to sign in a new app. Must match the translation used in the "Sign in with your old {productName} app" instructions.',
});

const MESSAGE_DESCRIPTORS = {
	install: INSTALL_MESSAGE_DESCRIPTOR,
	browser: BROWSER_MESSAGE_DESCRIPTOR,
	apple: APPLE_MESSAGE_DESCRIPTOR,
	generic: GENERIC_MESSAGE_DESCRIPTOR,
} as const;

function presentationFor(installKind: DomainMigrationInstallKind): DomainMovedPresentation {
	switch (installKind) {
		case 'chromium-desktop':
			return 'install';
		case 'chromium-android':
			return 'browser';
		case 'webkit':
		case 'none':
			return 'apple';
		case 'firefox':
		case 'other':
			return 'generic';
	}
}

function stepsPlatform(presentation: DomainMovedPresentation): DomainMovedStepsPlatform {
	if (presentation !== 'apple') {
		return 'generic';
	}
	return isIOSMobileOrTabletUserAgent(navigator.userAgent, navigator.maxTouchPoints) ? 'ios' : 'mac';
}

export const DomainMovedNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const target = DomainMovedNotice.target;
	const presentation = presentationFor(DomainMovedNotice.installKind);
	const handleDismiss = useCallback(() => {
		DomainMovedNotice.dismiss(Date.now());
	}, []);
	const handleInstall = useCallback(() => {
		installDomainMovedApp(target, () => showDomainMovedStepsModal(target, 'install'));
	}, [target]);
	const handleOpenInBrowser = useCallback(() => {
		openDomainMovedBrowserMigration(target);
	}, [target]);
	const handleShowSteps = useCallback(() => {
		showDomainMovedStepsModal(target, stepsPlatform(presentation));
	}, [presentation, target]);
	const handleLinkDevice = useCallback(() => {
		RouterUtils.transitionTo(`${Routes.LOGIN}?handoff=1`);
	}, []);
	const tone = NAGBAR_TONES[NagbarToneKind.BRAND];
	const values = {productName: PRODUCT_NAME, host: DomainMovedNotice.targetHost};
	const linkDeviceButton = (
		<NagbarButton
			isMobile={isMobile}
			variant="inverted-outline"
			onClick={handleLinkDevice}
			data-flx="app.app-layout.nagbars.domain-moved-nagbar.nagbar-button.link-device"
		>
			{i18n._(LINK_NEW_DEVICE_DESCRIPTOR)}
		</NagbarButton>
	);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={tone.backgroundColor}
			textColor={tone.textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.domain-moved-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(MESSAGE_DESCRIPTORS[presentation], values)}
				actions={
					presentation === 'install' ? (
						<NagbarButton
							isMobile={isMobile}
							onClick={handleInstall}
							data-flx="app.app-layout.nagbars.domain-moved-nagbar.nagbar-button.install"
						>
							{i18n._(INSTALL_NEW_APP_DESCRIPTOR)}
						</NagbarButton>
					) : presentation === 'browser' ? (
						<>
							{linkDeviceButton}
							<NagbarButton
								isMobile={isMobile}
								onClick={handleOpenInBrowser}
								data-flx="app.app-layout.nagbars.domain-moved-nagbar.nagbar-button.open-in-browser"
							>
								{i18n._(OPEN_IN_BROWSER_DESCRIPTOR, values)}
							</NagbarButton>
						</>
					) : (
						<>
							{linkDeviceButton}
							<NagbarButton
								isMobile={isMobile}
								onClick={handleShowSteps}
								data-flx="app.app-layout.nagbars.domain-moved-nagbar.nagbar-button.show-steps"
							>
								{i18n._(SHOW_ME_HOW_DESCRIPTOR)}
							</NagbarButton>
						</>
					)
				}
				data-flx="app.app-layout.nagbars.domain-moved-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
