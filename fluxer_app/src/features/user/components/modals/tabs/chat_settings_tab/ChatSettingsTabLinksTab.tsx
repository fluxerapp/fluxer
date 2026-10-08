// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as TrustedDomainCommands from '@app/features/trusted_domain/commands/TrustedDomainCommands';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import UserSettings from '@app/features/user/state/UserSettings';
import {msg, plural} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const ALL_EXTERNAL_LINKS_ARE_TRUSTED_WARNINGS_WILL_NOT_DESCRIPTOR = msg({
	message: 'All external links are trusted. Warnings will not be shown.',
	comment: 'Warning text in the links tab. Keep the tone plain and specific.',
});
const WHEN_ENABLED_NO_EXTERNAL_LINK_WARNINGS_WILL_BE_DESCRIPTOR = msg({
	message: 'Skip external link warnings. Less secure.',
	comment: 'Warning text in the links tab. Keep the tone plain and specific.',
});
const TRUST_ALL_EXTERNAL_LINKS_DESCRIPTOR = msg({
	message: 'Trust all external links?',
	comment: 'Confirmation prompt in the links tab.',
});
const TRUST_ALL_DESCRIPTOR = msg({
	message: 'Trust all',
	comment: 'Short label in the links tab. Keep it concise.',
});
const STOP_TRUSTING_ALL_LINKS_DESCRIPTOR = msg({
	message: 'Stop trusting all links?',
	comment: 'Confirmation prompt in the links tab.',
});
const DISABLE_TRUST_ALL_DESCRIPTOR = msg({
	message: 'Disable trust all',
	comment: 'Button or menu action label in the links tab. Keep it concise.',
});
const TRUST_ALL_EXTERNAL_LINKS_2_DESCRIPTOR = msg({
	message: 'Trust all external links',
	comment: 'Label in the links tab.',
});
const STRIP_TRACKING_PARAMETERS_FROM_URLS_DESCRIPTOR = msg({
	message: 'Strip tracking parameters from URLs',
	comment: 'Label in the links tab.',
});
const AUTOMATICALLY_REMOVE_TRACKING_PARAMETERS_LIKE_UTM_SOURCE_FBCLID_DESCRIPTOR = msg({
	message:
		'Automatically remove tracking parameters (like utm_source, fbclid, gclid) from URLs in messages you send. Cleans the link before it reaches anyone else.',
	comment: 'Description text in the links tab. Keep the tone plain and specific.',
});
interface LinkSwitchControlProps {
	compact?: boolean;
}

export const TrustAllExternalLinksControl: React.FC<LinkSwitchControlProps> = observer(({compact = false}) => {
	const {i18n} = useLingui();
	const trustAll = TrustedDomain.trustAllDomains;
	const trustedCount = TrustedDomain.getTrustedDomainsCount();
	const label = i18n._(TRUST_ALL_EXTERNAL_LINKS_2_DESCRIPTOR);
	const description = useMemo(() => {
		if (trustAll) {
			return i18n._(ALL_EXTERNAL_LINKS_ARE_TRUSTED_WARNINGS_WILL_NOT_DESCRIPTOR);
		}
		if (trustedCount > 0) {
			return plural(
				{count: trustedCount},
				{
					one: 'You have # trusted domain. Add more by checking the box when visiting external links.',
					other: 'You have # trusted domains. Add more by checking the box when visiting external links.',
				},
			);
		}
		return i18n._(WHEN_ENABLED_NO_EXTERNAL_LINK_WARNINGS_WILL_BE_DESCRIPTOR);
	}, [trustAll, trustedCount, i18n.locale]);
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
								await TrustedDomainCommands.setTrustAllDomains(true);
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
								await TrustedDomainCommands.setTrustAllDomains(false);
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
			label={compact ? undefined : label}
			description={compact ? undefined : description}
			value={trustAll}
			onChange={handleTrustAllChange}
			compact={compact}
			data-flx="user.chat-settings-tab.links-tab.links-tab-content.switch.trust-all-change"
		/>
	);
});

export const StripTrackingParametersControl: React.FC<LinkSwitchControlProps> = observer(({compact = false}) => {
	const {i18n} = useLingui();
	const sanitizeUrls = UserSettings.getSanitizeUrls();
	const label = i18n._(STRIP_TRACKING_PARAMETERS_FROM_URLS_DESCRIPTOR);
	const handleSanitizeUrlsChange = useCallback((value: boolean) => {
		void UserSettings.setSanitizeUrls(value);
	}, []);
	return (
		<Switch
			ariaLabel={label}
			label={compact ? undefined : label}
			description={
				compact ? undefined : i18n._(AUTOMATICALLY_REMOVE_TRACKING_PARAMETERS_LIKE_UTM_SOURCE_FBCLID_DESCRIPTOR)
			}
			value={sanitizeUrls}
			onChange={handleSanitizeUrlsChange}
			compact={compact}
			data-flx="user.chat-settings-tab.links-tab.links-tab-content.switch.sanitize-urls-change"
		/>
	);
});
