// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getNotificationAlertOptions,
	isIOSMobileOrTabletUserAgent,
	isMobileOrTabletUserAgent,
	NOTIFICATION_VIBRATION_PATTERN,
} from '@app/features/platform/notifications/NotificationAlertOptions';
import {describe, expect, it} from 'vitest';

describe('NotificationAlertOptions', () => {
	it('uses vibration instead of silent notifications on mobile devices', () => {
		const options = getNotificationAlertOptions({mobileOrTablet: true, silentOnNonMobile: true});
		expect(options.silent).toBeUndefined();
		expect(options.vibrate).toEqual([...NOTIFICATION_VIBRATION_PATTERN]);
	});
	it('keeps explicitly silent notification behavior for non-mobile devices', () => {
		const options = getNotificationAlertOptions({mobileOrTablet: false, silentOnNonMobile: true});
		expect(options).toEqual({silent: true});
	});
	it('leaves background desktop push notifications to the platform default alert behavior', () => {
		const options = getNotificationAlertOptions({mobileOrTablet: false, silentOnNonMobile: false});
		expect(options).toEqual({});
	});
	it('detects mobile and tablet user agents used by PWA installs', () => {
		expect(isMobileOrTabletUserAgent('Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X)')).toBe(true);
		expect(isMobileOrTabletUserAgent('Mozilla/5.0 (Linux; Android 13; Pixel 7)')).toBe(true);
		expect(isMobileOrTabletUserAgent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15)', 5)).toBe(true);
		expect(isMobileOrTabletUserAgent('Mozilla/5.0 (Macintosh; Intel Mac OS X 14_7)')).toBe(false);
	});
	it('separates iOS mobile and tablet user agents from Android', () => {
		expect(isIOSMobileOrTabletUserAgent('Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X)')).toBe(true);
		expect(isIOSMobileOrTabletUserAgent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15)', 5)).toBe(true);
		expect(isIOSMobileOrTabletUserAgent('Mozilla/5.0 (Linux; Android 13; Pixel 7)')).toBe(false);
	});
});
