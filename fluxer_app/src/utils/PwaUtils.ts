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

import {isElectron} from '~/utils/NativeUtils';

export function isStandalonePwa(): boolean {
	const matchDisplayMode = window.matchMedia?.('(display-mode: standalone)').matches ?? false;
	const navigatorStandalone = (window.navigator as unknown as {standalone?: boolean})?.standalone === true;
	const androidReferrer = document.referrer.includes('android-app://');

	return matchDisplayMode || navigatorStandalone || androidReferrer;
}

export function isMobileOrTablet(): boolean {
	return /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
}

export function isPwaOnMobileOrTablet(): boolean {
	return isStandalonePwa() && isMobileOrTablet();
}

export function isInstalledPwa(): boolean {
	return isStandalonePwa() && !isElectron();
}
