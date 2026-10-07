// SPDX-License-Identifier: AGPL-3.0-or-later

const NOTIFICATION_VIBRATION_PATTERN = [200, 100, 200] as const;

export interface NotificationAlertOptions {
	readonly silent?: true;
	readonly vibrate?: Array<number>;
}

export function isIOSMobileOrTabletUserAgent(userAgent: string, maxTouchPoints = 0): boolean {
	return /iPhone|iPad|iPod/i.test(userAgent) || (/Macintosh/i.test(userAgent) && maxTouchPoints > 1);
}

export function isMobileOrTabletUserAgent(userAgent: string, maxTouchPoints = 0): boolean {
	return /Android/i.test(userAgent) || isIOSMobileOrTabletUserAgent(userAgent, maxTouchPoints);
}

export function getNotificationAlertOptions({
	mobileOrTablet,
	silentOnNonMobile,
}: {
	mobileOrTablet: boolean;
	silentOnNonMobile: boolean;
}): NotificationAlertOptions {
	if (mobileOrTablet) {
		return {vibrate: [...NOTIFICATION_VIBRATION_PATTERN]};
	}
	if (silentOnNonMobile) {
		return {silent: true};
	}
	return {};
}
