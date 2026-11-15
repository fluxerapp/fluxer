// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {AuthLayout} from '@app/features/app/components/layout/AuthLayout';
import {AppSkeleton} from '@app/features/app/components/skeleton/AppSkeleton';
import type {AccountHandoffOutcome} from '@app/features/auth/flow/AccountHandoffRedeem';
import {createAuthRoutePage} from '@app/features/auth/flow/AuthLoadableRoutePage';
import {isAgeBlocked} from '@app/features/auth/state/AccountEntryStorage';
import {readCapturedAccountHandoff} from '@app/features/auth/state/AccountHandoffCapture';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useEffect, useState} from 'react';

const StartPage = createAuthRoutePage('StartPage', () => import('@app/features/auth/components/pages/StartPage'));

const PENDING = 'pending';

type ArrivalState = Exclude<AccountHandoffOutcome, {readonly kind: 'signed_in'}> | null | typeof PENDING;

function redeemsOnArrival(): boolean {
	const handoff = readCapturedAccountHandoff();
	return handoff !== null && handoff.name !== '' && !isAgeBlocked() && !isDesktop();
}

export function AccountHandoffArrival() {
	const [arrival, setArrival] = useState<ArrivalState>(() => (redeemsOnArrival() ? PENDING : null));
	useEffect(() => {
		void StartPage.preload();
		if (arrival !== PENDING) return;
		let active = true;
		void loadLazyModule(() => import('@app/features/auth/flow/AccountHandoffRedeem'))
			.then(({redeemAccountHandoffOnArrival}) => redeemAccountHandoffOnArrival())
			.catch((error: unknown): AccountHandoffOutcome => ({kind: 'failed', error}))
			.then((outcome) => {
				if (active && outcome.kind !== 'signed_in') setArrival(outcome);
			});
		return () => {
			active = false;
		};
	}, [arrival]);
	if (arrival === PENDING) {
		return <AppSkeleton effectivePathname={Routes.ME} data-flx="auth.account-handoff-arrival.app-skeleton" />;
	}
	return (
		<AuthLayout data-flx="auth.account-handoff-arrival.auth-layout">
			<StartPage arrival={arrival} data-flx="auth.account-handoff-arrival.start-page" />
		</AuthLayout>
	);
}
