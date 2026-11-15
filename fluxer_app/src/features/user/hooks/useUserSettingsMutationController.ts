// SPDX-License-Identifier: AGPL-3.0-or-later

import Accounts from '@app/features/auth/state/Accounts';
import {resolvePortalHost, usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {
	type UserSettingsMutation,
	type UserSettingsMutationController,
	UserSettingsMutationPresentationOwner,
} from '@app/features/user/UserSettingsMutationPresentation';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useLayoutEffect, useMemo, useRef} from 'react';

class InactiveUserSettingsMutationControllerError extends Error {
	public constructor() {
		super('User settings mutation requires the current account and portal owner');
		this.name = 'InactiveUserSettingsMutationControllerError';
	}
}

export function useUserSettingsMutationController(): UserSettingsMutationController {
	const {i18n} = useLingui();
	const accountKey = Accounts.currentAccountKey;
	const portalHost = resolvePortalHost(usePortalHost());
	const presentationOwnerRef = useRef<UserSettingsMutationPresentationOwner | null>(null);
	useLayoutEffect(() => {
		if (accountKey == null) {
			return;
		}
		const presentationOwner = new UserSettingsMutationPresentationOwner({accountKey, i18n, portalHost});
		presentationOwnerRef.current = presentationOwner;
		return () => {
			if (presentationOwnerRef.current === presentationOwner) {
				presentationOwnerRef.current = null;
			}
			presentationOwner.dispose();
		};
	}, [accountKey, i18n, portalHost]);
	const requirePresentationOwner = useCallback((): UserSettingsMutationPresentationOwner => {
		const presentationOwner = presentationOwnerRef.current;
		if (accountKey == null || presentationOwner == null || Accounts.currentAccountKey !== accountKey) {
			throw new InactiveUserSettingsMutationControllerError();
		}
		return presentationOwner;
	}, [accountKey]);
	const settle = useCallback(
		(mutation: UserSettingsMutation): void => {
			requirePresentationOwner().settle(mutation);
		},
		[requirePresentationOwner],
	);
	return useMemo<UserSettingsMutationController>(() => ({settle}), [settle]);
}
