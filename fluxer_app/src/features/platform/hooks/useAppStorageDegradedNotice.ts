// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAppStorageDegradedReason, subscribeAppStorageDegraded} from '@app/features/platform/state/PersistentStorage';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useEffect} from 'react';

const STORAGE_UNAVAILABLE_DESCRIPTOR = msg({
	message: "This browser can't save Fluxer's data. You'll need to sign in again after you close it.",
	comment: 'Notice shown when the browser refuses to store app data and the app keeps it in memory for this tab.',
});

const STORAGE_UNAVAILABLE_TIMEOUT_MS = 15_000;

let noticeShown = false;

export function useAppStorageDegradedNotice(): void {
	const {i18n} = useLingui();
	useEffect(() => {
		const show = (): void => {
			if (noticeShown) {
				return;
			}
			noticeShown = true;
			ToastCommands.createToast({
				type: 'info',
				children: i18n._(STORAGE_UNAVAILABLE_DESCRIPTOR),
				timeout: STORAGE_UNAVAILABLE_TIMEOUT_MS,
			});
		};
		if (getAppStorageDegradedReason() !== null) {
			show();
		}
		return subscribeAppStorageDegraded(show);
	}, [i18n]);
}
