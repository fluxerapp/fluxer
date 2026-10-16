// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accounts from '@app/features/auth/state/Accounts';
import Authentication from '@app/features/auth/state/Authentication';
import Location from '@app/features/ui/state/Location';
import {useEffect} from 'react';

export function useLastLocationPersistence(pathname: string): void {
	const isAuthenticated = Authentication.isAuthenticated;
	const currentAccountKey = Accounts.currentAccountKey;
	const isViewLive = Accounts.isViewLive;
	useEffect(() => {
		if (!isAuthenticated || currentAccountKey === null || !isViewLive) {
			return;
		}
		if (!Routes.isChannelRoute(pathname) && !Routes.isSpecialPage(pathname)) {
			return;
		}
		Location.saveLocation(pathname);
		globalThis.window?.electron?.reportLastRoute?.(pathname);
	}, [currentAccountKey, isAuthenticated, isViewLive, pathname]);
}
