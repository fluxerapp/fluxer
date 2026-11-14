// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import UserSettings from '@app/features/user/state/UserSettings';
import {
	STRIP_TRACKING_PARAMETERS_FROM_URLS_DESCRIPTOR,
	TRUST_ALL_DESCRIPTOR,
	TRUST_ALL_EXTERNAL_LINKS_DESCRIPTOR as TRUST_ALL_EXTERNAL_LINKS_2_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const TRUST_ALL_EXTERNAL_LINKS_DESCRIPTOR = msg({
	message: 'Trust all external links?',
	comment: 'Confirmation prompt in the links tab.',
});

const STOP_TRUSTING_ALL_LINKS_DESCRIPTOR = msg({
	message: 'Stop trusting all links?',
	comment: 'Confirmation prompt in the links tab.',
});
const DISABLE_TRUST_ALL_DESCRIPTOR = msg({
	message: 'Disable trust all',
	comment: 'Button or menu action label in the links tab. Keep it concise.',
});

export const TrustAllExternalLinksControl: React.FC = observer(() => {
	const {i18n} = useLingui();
	const trustAll = TrustedDomain.trustAllDomains;
	const label = i18n._(TRUST_ALL_EXTERNAL_LINKS_2_DESCRIPTOR);
	const handleTrustAllChange = useCallback(
		(value: boolean) => {
			if (value) {
				ModalCommands.push(
					ModalCommands.modal(() => (
						<ConfirmModal
							title={i18n._(TRUST_ALL_EXTERNAL_LINKS_DESCRIPTOR)}
							description={
								<Trans>
									This will trust all external links and skip the warning for every domain. Your existing trusted
									domains will be replaced. This is less secure.
								</Trans>
							}
							primaryText={i18n._(TRUST_ALL_DESCRIPTOR)}
							primaryVariant="danger"
							onPrimary={async () => {
								await TrustedDomain.setTrustAllDomains(true);
							}}
							data-flx="user.chat-settings-tab.links-tab.handle-trust-all-change.confirm-modal"
						/>
					)),
				);
			} else {
				ModalCommands.push(
					ModalCommands.modal(() => (
						<ConfirmModal
							title={i18n._(STOP_TRUSTING_ALL_LINKS_DESCRIPTOR)}
							description={
								<Trans>
									External link warnings will be shown again. You will need to add trusted domains individually.
								</Trans>
							}
							primaryText={i18n._(DISABLE_TRUST_ALL_DESCRIPTOR)}
							onPrimary={async () => {
								await TrustedDomain.setTrustAllDomains(false);
							}}
							data-flx="user.chat-settings-tab.links-tab.handle-trust-all-change.confirm-modal--2"
						/>
					)),
				);
			}
		},
		[i18n],
	);
	return (
		<Switch
			ariaLabel={label}
			value={trustAll}
			onChange={handleTrustAllChange}
			compact
			data-flx="user.chat-settings-tab.links-tab.links-tab-content.switch.trust-all-change"
		/>
	);
});

export const StripTrackingParametersControl: React.FC = observer(() => {
	const {i18n} = useLingui();
	const sanitizeUrls = UserSettings.getSanitizeUrls();
	const label = i18n._(STRIP_TRACKING_PARAMETERS_FROM_URLS_DESCRIPTOR);
	const handleSanitizeUrlsChange = useCallback((value: boolean) => {
		void UserSettings.setSanitizeUrls(value);
	}, []);
	return (
		<Switch
			ariaLabel={label}
			value={sanitizeUrls}
			onChange={handleSanitizeUrlsChange}
			compact
			data-flx="user.chat-settings-tab.links-tab.links-tab-content.switch.sanitize-urls-change"
		/>
	);
});
