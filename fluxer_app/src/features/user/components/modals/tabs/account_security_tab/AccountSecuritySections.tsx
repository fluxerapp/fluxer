// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import * as OAuth2AuthorizationCommands from '@app/features/auth/commands/OAuth2AuthorizationCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {AuthorizedAppsManagementModal} from '@app/features/user/components/modals/tabs/account_security_tab/AccountManagementModals';
import {AccountTabContent} from '@app/features/user/components/modals/tabs/account_security_tab/AccountTab';
import {DangerZoneTabContent} from '@app/features/user/components/modals/tabs/account_security_tab/DangerZoneTab';
import {SecurityTabContent} from '@app/features/user/components/modals/tabs/account_security_tab/SecurityTab';
import {BlockedUsersContent} from '@app/features/user/components/modals/tabs/BlockedUsersTab';
import {LinkedDevicesManagementModal} from '@app/features/user/components/modals/tabs/DevicesTab';
import type {AccountSettingsManagementSectionId} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import {
	BLOCKED_USERS_DESCRIPTOR,
	SECURITY_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import type {User} from '@app/features/user/models/User';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef, useState} from 'react';

const SIGN_IN_DETAILS_DESCRIPTOR = msg({
	message: 'Sign-in details',
	comment: 'Section title for email address and password controls in the account security tab. Keep it concise.',
});
const COULD_NOT_LOAD_AUTHORIZED_APPS_DESCRIPTOR = msg({
	message: "Couldn't load authorized apps",
	comment: 'Title of the error modal shown when opening the authorized apps management modal fails.',
});
const DANGER_ZONE_DESCRIPTOR = msg({
	message: 'Danger zone',
	comment: 'Short label in the account security tab. Keep it concise. Keep the tone plain and specific.',
});

interface AccountSecuritySectionsProps {
	user: User;
	isClaimed: boolean;
	passkeys: ReadonlyArray<WebAuthnCredential>;
	showMaskedEmail: boolean;
	setShowMaskedEmail: (show: boolean) => void;
	targetSection?: AccountSettingsManagementSectionId | null;
}

export const AccountSecuritySections: React.FC<AccountSecuritySectionsProps> = observer(
	({user, isClaimed, passkeys, showMaskedEmail, setShowMaskedEmail, targetSection}) => {
		const {i18n} = useLingui();
		const [authorizedAppsSubmitting, setAuthorizedAppsSubmitting] = useState(false);
		const authorizedAppsSubmittingRef = useRef(false);
		const openAuthorizedAppsModal = useCallback(() => {
			if (authorizedAppsSubmittingRef.current) return;
			authorizedAppsSubmittingRef.current = true;
			setAuthorizedAppsSubmitting(true);
			void (async () => {
				try {
					const authorizations = await OAuth2AuthorizationCommands.listAuthorizations();
					ModalCommands.push(
						modal(() => (
							<AuthorizedAppsManagementModal
								authorizations={authorizations}
								data-flx="user.account-security-sections.authorized-apps-modal"
							/>
						)),
					);
				} catch (error) {
					FormUtils.pushApiErrorModal(i18n, error, i18n._(COULD_NOT_LOAD_AUTHORIZED_APPS_DESCRIPTOR));
				} finally {
					authorizedAppsSubmittingRef.current = false;
					setAuthorizedAppsSubmitting(false);
				}
			})();
		}, [i18n]);
		const openLinkedDevicesModal = () => {
			ModalCommands.push(
				modal(() => <LinkedDevicesManagementModal data-flx="user.account-security-sections.linked-devices-modal" />),
			);
		};
		return (
			<>
				{isClaimed && (
					<SettingsSection
						id="account"
						title={i18n._(SIGN_IN_DETAILS_DESCRIPTOR)}
						data-flx="user.account-security-sections.account"
					>
						<AccountTabContent
							user={user}
							isClaimed={isClaimed}
							showMaskedEmail={showMaskedEmail}
							setShowMaskedEmail={setShowMaskedEmail}
							data-flx="user.account-security-sections.account-tab-content"
						/>
					</SettingsSection>
				)}
				{isClaimed && (
					<SettingsSection
						id="security"
						title={i18n._(SECURITY_DESCRIPTOR)}
						defaultExpanded={targetSection === 'security'}
						data-flx="user.account-security-sections.security"
					>
						<SecurityTabContent
							user={user}
							isClaimed={isClaimed}
							passkeys={passkeys}
							authorizedAppsSubmitting={authorizedAppsSubmitting}
							onManageAuthorizedApps={openAuthorizedAppsModal}
							onManageLinkedDevices={openLinkedDevicesModal}
							data-flx="user.account-security-sections.security-tab-content"
						/>
					</SettingsSection>
				)}
				<SettingsSection
					id="danger_zone"
					title={i18n._(DANGER_ZONE_DESCRIPTOR)}
					data-flx="user.account-security-sections.danger-zone"
				>
					<DangerZoneTabContent
						user={user}
						isClaimed={isClaimed}
						data-flx="user.account-security-sections.danger-zone-tab-content"
					/>
				</SettingsSection>
				{isClaimed && (
					<SettingsSection
						id="blocked_users"
						title={i18n._(BLOCKED_USERS_DESCRIPTOR)}
						defaultExpanded={targetSection === 'blocked_users'}
						data-flx="user.account-security-sections.blocked-users"
					>
						<BlockedUsersContent data-flx="user.account-security-sections.blocked-users-content" />
					</SettingsSection>
				)}
			</>
		);
	},
);
