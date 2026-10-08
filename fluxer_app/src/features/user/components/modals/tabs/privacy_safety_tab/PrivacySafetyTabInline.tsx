// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import Accounts from '@app/features/auth/state/Accounts';
import {ActiveNowTabContent as ActiveNowTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/ActiveNowTab';
import {CommunicationTabContent as CommunicationTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/CommunicationTab';
import {ConnectionsTabContent as ConnectionsTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/ConnectionsTab';
import {DataDeletionTabContent as DataDeletionTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/DataDeletionTab';
import {DataExportTabContent as DataExportTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/DataExportTab';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/PrivacySafetyTabInline.module.css';
import {ProfilePrivacyTabContent as ProfilePrivacyTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/ProfilePrivacyTab';
import {SensitiveContentTabContent as SensitiveContentTab} from '@app/features/user/components/modals/tabs/privacy_safety_tab/SensitiveContentTab';
import {useUserSettingsMutationController} from '@app/features/user/hooks/useUserSettingsMutationController';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FRIENDS_AND_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Friends & direct messages',
	comment: 'Privacy dashboard section label for who can send friend requests and direct messages.',
});
const PROFILE_PRIVACY_DESCRIPTOR = msg({
	message: 'Profile privacy',
	comment: 'Privacy section label for who can see the user profile details.',
});
const COMMUNICATION_DESCRIPTOR = msg({
	message: 'Communication',
	comment: 'Short label in the privacy dashboard. Keep it concise.',
});
const ACTIVITY_SHARING_DESCRIPTOR = msg({
	message: 'Activity sharing',
	comment: 'Privacy section label for voice activity sharing.',
});
const SENSITIVE_CONTENT_DESCRIPTOR = msg({
	message: 'Sensitive content',
	comment: 'Short label in the privacy dashboard. Keep it concise.',
});
const DATA_EXPORT_DESCRIPTOR = msg({
	message: 'Data export',
	comment: 'Short label in the privacy dashboard. Keep it concise.',
});
const DATA_DELETION_DESCRIPTOR = msg({
	message: 'Data deletion',
	comment: 'Short label in the privacy dashboard. Keep it concise.',
});
export const PrivacyDashboardContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const accountKey = Accounts.currentAccountKey;
	const mutationController = useUserSettingsMutationController();
	return (
		<div
			key={accountKey}
			className={styles.container}
			data-flx="user.privacy-safety-tab.privacy-dashboard-content.container"
		>
			<SettingsSection
				id="profile-privacy"
				tabType="privacy_safety"
				title={i18n._(PROFILE_PRIVACY_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.profile-privacy"
			>
				<ProfilePrivacyTab data-flx="user.privacy-safety-tab.privacy-dashboard-content.profile-privacy-tab" />
			</SettingsSection>
			<SettingsSection
				id="connections"
				tabType="privacy_safety"
				title={i18n._(FRIENDS_AND_DIRECT_MESSAGES_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.connections"
			>
				<ConnectionsTab data-flx="user.privacy-safety-tab.privacy-dashboard-content.connections-tab" />
			</SettingsSection>
			<SettingsSection
				id="communication"
				tabType="privacy_safety"
				title={i18n._(COMMUNICATION_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.communication"
			>
				<CommunicationTab
					mutationController={mutationController}
					data-flx="user.privacy-safety-tab.privacy-dashboard-content.communication-tab"
				/>
			</SettingsSection>
			<SettingsSection
				id="active-now"
				tabType="privacy_safety"
				title={i18n._(ACTIVITY_SHARING_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.active-now"
			>
				<ActiveNowTab data-flx="user.privacy-safety-tab.privacy-dashboard-content.active-now-tab" />
			</SettingsSection>
			<SettingsSection
				id="sensitive-content"
				tabType="privacy_safety"
				title={i18n._(SENSITIVE_CONTENT_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.sensitive-content"
			>
				<SensitiveContentTab data-flx="user.privacy-safety-tab.privacy-dashboard-content.sensitive-content-tab" />
			</SettingsSection>
			<SettingsSection
				id="data-export"
				tabType="privacy_safety"
				title={i18n._(DATA_EXPORT_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.data-export"
			>
				<DataExportTab data-flx="user.privacy-safety-tab.privacy-dashboard-content.data-export-tab" />
			</SettingsSection>
			<SettingsSection
				id="data-deletion"
				tabType="privacy_safety"
				title={i18n._(DATA_DELETION_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.privacy-dashboard-content.data-deletion"
			>
				<DataDeletionTab data-flx="user.privacy-safety-tab.privacy-dashboard-content.data-deletion-tab" />
			</SettingsSection>
		</div>
	);
});
