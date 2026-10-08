// SPDX-License-Identifier: AGPL-3.0-or-later

import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import type React from 'react';
import {useMemo} from 'react';

export const MIDDLE_MOUSE_BUTTON = 1;

interface MiddleClickOpenHandlers {
	onMouseDown: (event: React.MouseEvent) => void;
	onAuxClick: (event: React.MouseEvent) => void;
}

export function useOpenInBrowserOnMiddleClick(url: string | null | undefined, enabled = true): MiddleClickOpenHandlers {
	return useMemo<MiddleClickOpenHandlers>(
		() => ({
			onMouseDown: (event) => {
				if (event.button !== MIDDLE_MOUSE_BUTTON || !enabled || !url) return;
			},
			onAuxClick: (event) => {
				if (event.button !== MIDDLE_MOUSE_BUTTON || !enabled || !url) return;
				event.preventDefault();
				event.stopPropagation();
				openExternalUrlWithWarning(url);
			},
		}),
		[url, enabled],
	);
}
