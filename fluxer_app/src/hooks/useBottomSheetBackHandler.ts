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

import {useEffect, useRef} from 'react';
import * as RouterUtils from '~/utils/RouterUtils';

let globalCleanupInProgress = false;

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

		const historyStateId = `bottom-sheet-${Date.now()}`;
		const history = RouterUtils.getHistory();

		if (!history) {
			return;
		}

		const currentState = history.getLocation().state as {bottomSheet?: string} | null;
		const isReplacingSheet = currentState?.bottomSheet;

		const currentUrl = new URL(window.location.pathname + window.location.search, window.location.origin);

		if (isReplacingSheet) {
			history.replace(currentUrl, {
				bottomSheet: historyStateId,
			});
		} else {
			history.push(currentUrl, {
				bottomSheet: historyStateId,
			});
			historyEntryPushedRef.current = true;
		}

		const handlePopState = (event: PopStateEvent) => {
			if (globalCleanupInProgress) {
				return;
			}

			const state = event.state as {bottomSheet?: string};
			if (historyEntryPushedRef.current && state?.bottomSheet !== historyStateId) {
				if (state?.bottomSheet) {
					return;
				}
				closedViaBackButtonRef.current = true;
				historyEntryPushedRef.current = false;
				onCloseRef.current();
			}
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

			if (historyEntryPushedRef.current && !closedViaBackButtonRef.current) {
				historyEntryPushedRef.current = false;
				globalCleanupInProgress = true;
				window.removeEventListener('popstate', handlePopState);
				history.back();
				setTimeout(() => {
					globalCleanupInProgress = false;
				}, 100);
			} else if (!closedViaBackButtonRef.current) {
				window.removeEventListener('popstate', handlePopState);
				const cleanUrl = new URL(window.location.pathname + window.location.search, window.location.origin);
				history.replace(cleanUrl, {});
			} else {
				window.removeEventListener('popstate', handlePopState);
			}
		};
	}, [isOpen, disableHistoryManagement]);
};
