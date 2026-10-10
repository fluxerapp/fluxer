// SPDX-License-Identifier: AGPL-3.0-or-later

import {StreamerModeGate} from '@app/features/streamer_mode/components/StreamerModeGate';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {AccountSecuritySections} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecuritySections';
import styles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {
	getAccountSectionForLegacySection,
	getAccountSectionForNestedTab,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import type {UserSettingsTabType} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import Users from '@app/features/user/state/Users';
import WebAuthnCredentials from '@app/features/user/state/WebAuthnCredentials';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

interface AccountSecurityInlineTabProps {
	settingsTabType?: UserSettingsTabType;
	initialSubtab?: string;
}

export const AccountSecurityInlineTab = observer(({settingsTabType, initialSubtab}: AccountSecurityInlineTabProps) => {
	const user = Users.currentUser;
	const [showMaskedEmail, setShowMaskedEmail] = useState(false);
	if (!user) return null;
	if (StreamerMode.shouldHidePersonalInformation) {
		return (
			<div className={styles.container} data-flx="user.account-security-tab.inline.streamer-mode-gate.container">
				<StreamerModeGate data-flx="user.account-security-tab.account-security-tab-inline.account-security-inline-tab.streamer-mode-gate" />
			</div>
		);
	}
	const isClaimed = user.isClaimed();
	const passkeys = isClaimed ? WebAuthnCredentials.credentials : [];
	const targetSection =
		getAccountSectionForNestedTab(settingsTabType) ?? getAccountSectionForLegacySection(initialSubtab);
	return (
		<div className={styles.container} data-flx="user.account-security-tab.inline.account-security-inline-tab.container">
			<AccountSecuritySections
				user={user}
				isClaimed={isClaimed}
				passkeys={passkeys}
				showMaskedEmail={showMaskedEmail}
				setShowMaskedEmail={setShowMaskedEmail}
				targetSection={targetSection}
				data-flx="user.account-security-tab.inline.account-security-sections"
			/>
		</div>
	);
});
