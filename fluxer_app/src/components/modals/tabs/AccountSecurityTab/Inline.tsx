/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {UserAuthenticatorTypes, UserFlags} from '~/Constants';
import {SettingsSection} from '~/components/modals/shared/SettingsSection';
import UserStore from '~/stores/UserStore';
import {AccountTabContent as AccountTab} from './AccountTab';
import {DangerZoneTabContent as DangerZoneTab} from './DangerZoneTab';
import styles from './Inline.module.css';
import {SecurityTabContent as SecurityTab} from './SecurityTab';

export const AccountSecurityInlineTab = observer(() => {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const [showMaskedEmail, setShowMaskedEmail] = useState(false);
	const [passkeys, setPasskeys] = useState<Array<UserActionCreators.WebAuthnCredential>>([]);
	const [loadingPasskeys, setLoadingPasskeys] = useState(false);
	const [enablingSmsMfa, setEnablingSmsMfa] = useState(false);
	const [disablingSmsMfa, setDisablingSmsMfa] = useState(false);

	const loadPasskeys = useCallback(async () => {
		setLoadingPasskeys(true);
		try {
			const credentials = await UserActionCreators.listWebAuthnCredentials();
			setPasskeys(credentials);
		} catch (error) {
			console.error('Failed to load passkeys', error);
		} finally {
			setLoadingPasskeys(false);
		}
	}, []);

	useEffect(() => {
		loadPasskeys();
	}, [loadPasskeys]);

	if (!user) return null;

	const hasSmsMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.SMS) ?? false;
	const hasTotpMfa = user.authenticatorTypes?.includes(UserAuthenticatorTypes.TOTP) ?? false;
	const isSmsMfaDisabledForUser =
		(user.flags & UserFlags.STAFF) !== 0 ||
		(user.flags & UserFlags.CTP_MEMBER) !== 0 ||
		(user.flags & UserFlags.PARTNER) !== 0;

	const isClaimed = user.isClaimed();

	return (
		<div className={styles.container}>
			<SettingsSection id="account" title={t`Account`}>
				<AccountTab
					user={user}
					isClaimed={isClaimed}
					showMaskedEmail={showMaskedEmail}
					setShowMaskedEmail={setShowMaskedEmail}
				/>
			</SettingsSection>
			<SettingsSection id="security" title={t`Security`}>
				<SecurityTab
					user={user}
					isClaimed={isClaimed}
					hasSmsMfa={hasSmsMfa}
					hasTotpMfa={hasTotpMfa}
					isSmsMfaDisabledForUser={isSmsMfaDisabledForUser}
					passkeys={passkeys}
					loadingPasskeys={loadingPasskeys}
					enablingSmsMfa={enablingSmsMfa}
					disablingSmsMfa={disablingSmsMfa}
					loadPasskeys={loadPasskeys}
					setEnablingSmsMfa={setEnablingSmsMfa}
					setDisablingSmsMfa={setDisablingSmsMfa}
				/>
			</SettingsSection>
			<SettingsSection id="danger_zone" title={t`Danger Zone`}>
				<DangerZoneTab user={user} isClaimed={isClaimed} />
			</SettingsSection>
		</div>
	);
});
