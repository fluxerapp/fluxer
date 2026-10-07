// SPDX-License-Identifier: AGPL-3.0-or-later

import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';
import {useEffect, useRef} from 'react';

let globalCleanupInProgress = false;

interface BottomSheetHistoryState {
	bottomSheet?: unknown;
}

function getBottomSheetHistoryId(state: unknown): string | null {
	if (!state || typeof state !== 'object') {
		return null;
	}
	const bottomSheet = (state as BottomSheetHistoryState).bottomSheet;
	return typeof bottomSheet === 'string' ? bottomSheet : null;
}

export const useBottomSheetBackHandler = (isOpen: boolean, onClose: () => void, disableHistoryManagement = false) => {
	const historyEntryPushedRef = useRef(false);
	const closedViaBackButtonRef = useRef(false);
	const onCloseRef = useRef(onClose);
	useEffect(() => {
		onCloseRef.current = onClose;
	}, [onClose]);
	useEffect(() => {
		if (!isOpen) {
			historyEntryPushedRef.current = false;
			closedViaBackButtonRef.current = false;
			return;
		}
		if (disableHistoryManagement) {
			return;
		}
		if (historyEntryPushedRef.current) {
			return;
		}
		const historyStateId = `bottom-sheet-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`;
		const history = RouterUtils.getHistory();
		if (!history) {
			return;
		}
		const currentUrl = resolveDocumentURLFromRoot(window.location.pathname + window.location.search);
		history.push(currentUrl, {
			bottomSheet: historyStateId,
		});
		historyEntryPushedRef.current = true;
		const handlePopState = (event: PopStateEvent) => {
			if (globalCleanupInProgress) {
				return;
			}
			if (!historyEntryPushedRef.current) {
				return;
			}
			const bottomSheetHistoryId = getBottomSheetHistoryId(event.state);
			if (bottomSheetHistoryId === historyStateId) {
				return;
			}
			closedViaBackButtonRef.current = true;
			historyEntryPushedRef.current = false;
			onCloseRef.current();
		};
		window.addEventListener('popstate', handlePopState);
		return () => {
			if (disableHistoryManagement) {
				return;
			}
			const history = RouterUtils.getHistory();
			if (!history) {
				window.removeEventListener('popstate', handlePopState);
				return;
			}
			const location = history.getLocation();
			const activeBottomSheetHistoryId = getBottomSheetHistoryId(location.state);
			const isActiveSheetHistoryEntry = activeBottomSheetHistoryId === historyStateId;
			const isStillOnSheetUrl =
				location.url.pathname === currentUrl.pathname && location.url.search === currentUrl.search;
			if (
				historyEntryPushedRef.current &&
				!closedViaBackButtonRef.current &&
				isActiveSheetHistoryEntry &&
				isStillOnSheetUrl &&
				!AccountScopedWork.isSuspended
			) {
				historyEntryPushedRef.current = false;
				globalCleanupInProgress = true;
				window.removeEventListener('popstate', handlePopState);
				history.back();
				setTimeout(() => {
					globalCleanupInProgress = false;
				}, 100);
			} else if (!closedViaBackButtonRef.current && isActiveSheetHistoryEntry) {
				historyEntryPushedRef.current = false;
				window.removeEventListener('popstate', handlePopState);
				history.replace(location.url, {});
			} else {
				window.removeEventListener('popstate', handlePopState);
			}
		};
	}, [isOpen, disableHistoryManagement]);
};
