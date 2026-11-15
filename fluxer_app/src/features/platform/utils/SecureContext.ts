// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktop} from '@app/features/ui/utils/NativeUtils';

export function isInsecureBrowserContext(): boolean {
	return !isDesktop() && typeof window !== 'undefined' && window.isSecureContext === false;
}
