// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {getNotificationIconURL} from '@app/features/notification/utils/NotificationIconURL';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import type {NotificationAlertOptions} from '@app/features/platform/notifications/NotificationAlertOptions';
import {getNotificationAlertOptions} from '@app/features/platform/notifications/NotificationAlertOptions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import Sound from '@app/features/ui/state/Sound';
import {getElectronAPI, hasUnavailableElectronNativeContext, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {isInstalledIOSPwa, isInstalledPwa, isMobileOrTablet} from '@app/features/ui/utils/PwaUtils';
import Users from '@app/features/user/state/Users';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const logger = new Logger('NotificationUtils');
const HUZZAH_DESKTOP_NOTIFICATIONS_ARE_ENABLED_DESCRIPTOR = msg({
	message: 'Huzzah! Desktop notifications are enabled',
	comment: 'Label in the notification utils helper.',
});
const HUZZAH_NOTIFICATIONS_ARE_ENABLED_DESCRIPTOR = msg({
	message: 'Huzzah! Notifications are enabled',
	comment: 'Label in the notification utils helper.',
});
const HUZZAH_BROWSER_NOTIFICATIONS_ARE_ENABLED_DESCRIPTOR = msg({
	message: 'Huzzah! Browser notifications are enabled',
	comment: 'Label in the notification utils helper.',
});
const ACCESS_GRANTED_DESCRIPTOR = msg({
	message: 'Access granted',
	comment: 'Short label in the notification utils helper. Keep it concise.',
});

type NotificationClickHandlerGlobal = typeof globalThis & {
	__fluxerNotificationClickHandlerInitialized?: boolean;
};

function hasInitializedDesktopNotificationClickHandler(): boolean {
	return Boolean((globalThis as NotificationClickHandlerGlobal).__fluxerNotificationClickHandlerInitialized);
}

function setDesktopNotificationClickHandlerInitialized(): void {
	(globalThis as NotificationClickHandlerGlobal).__fluxerNotificationClickHandlerInitialized = true;
}

export function ensureDesktopNotificationClickHandler(): void {
	if (hasInitializedDesktopNotificationClickHandler()) return;
	const electronApi = getElectronAPI();
	if (!electronApi) return;
	setDesktopNotificationClickHandlerInitialized();
	electronApi.onNotificationClick((_id: string, url?: string) => {
		if (url) {
			RouterUtils.transitionTo(url);
		}
	});
}

export function hasNotification(): boolean {
	if (hasUnavailableElectronNativeContext()) return false;
	if (isDesktop()) return true;
	return typeof Notification !== 'undefined';
}

export function isMacOSDesktopNotification(): boolean {
	return getElectronAPI()?.platform === 'darwin';
}

export async function isGranted(): Promise<boolean> {
	if (hasUnavailableElectronNativeContext()) return false;
	if (isDesktop()) return true;
	return typeof Notification !== 'undefined' && Notification.permission === 'granted';
}

async function shouldPlayNotificationSound(soundType: SoundType): Promise<boolean> {
	if (StreamerMode.shouldDisableNotifications) return false;
	if (isInstalledIOSPwa()) return false;
	if (!Sound.isSoundTypeEnabled(soundType)) return false;
	const electronApi = getElectronAPI();
	if (electronApi?.shouldPlayNotificationSound) {
		try {
			return await electronApi.shouldPlayNotificationSound();
		} catch (error) {
			logger.warn('Failed to query native notification sound state; allowing notification sound', {error});
		}
	}
	return true;
}

function playNotificationSound(soundType: SoundType): void {
	void shouldPlayNotificationSound(soundType)
		.then((allowed) => {
			if (allowed) {
				SoundCommands.playSound(soundType);
			}
		})
		.catch((error) => {
			logger.warn('Failed to decide notification sound policy; dropping notification sound', {error});
		});
}

export function playNotificationSoundIfEnabled(): void {
	playNotificationSound(SoundType.Message);
}

export function playDirectMessageNotificationSoundIfEnabled(): void {
	playNotificationSound(SoundType.DirectMessage);
}

export function playSameChannelNotificationSoundIfEnabled(): void {
	playNotificationSound(SoundType.SameChannelMessage);
}

type PermissionResult = 'granted' | 'denied' | 'unsupported';

const requestBrowserPermission = async (): Promise<PermissionResult> => {
	if (typeof Notification === 'undefined') {
		return 'unsupported';
	}
	try {
		const permission = await Notification.requestPermission();
		return permission === 'granted' ? 'granted' : 'denied';
	} catch {
		return 'denied';
	}
};

const getCurrentUserAvatar = (): string | null => {
	const currentUserId = Authentication.currentUserId;
	if (!currentUserId) return null;
	const currentUser = Users.getUser(currentUserId);
	if (!currentUser) return null;
	return getNotificationIconURL(currentUser);
};

async function handleDeniedPermission(i18n: I18n): Promise<void> {
	const NotificationCommands = await import('@app/features/ui/commands/NotificationCommands');
	NotificationCommands.permissionDenied(i18n);
}

const applyGrantedPermission = async (i18n: I18n): Promise<void> => {
	const NotificationCommands = await import('@app/features/ui/commands/NotificationCommands');
	NotificationCommands.permissionGranted();
	playNotificationSoundIfEnabled();
	const icon = getCurrentUserAvatar() ?? '';
	const isPwa = !isDesktop() && isInstalledPwa();
	const body = isDesktop()
		? HUZZAH_DESKTOP_NOTIFICATIONS_ARE_ENABLED_DESCRIPTOR
		: isPwa
			? HUZZAH_NOTIFICATIONS_ARE_ENABLED_DESCRIPTOR
			: HUZZAH_BROWSER_NOTIFICATIONS_ARE_ENABLED_DESCRIPTOR;
	void showNotification({
		title: i18n._(ACCESS_GRANTED_DESCRIPTOR),
		body: i18n._(body),
		icon,
		playSound: false,
	});
};

export function handlePermissionResult(i18n: I18n, permission: NotificationPermission): void {
	if (permission === 'granted') {
		void applyGrantedPermission(i18n);
	} else {
		void handleDeniedPermission(i18n);
	}
}

export async function requestPermission(i18n: I18n): Promise<void> {
	if (hasUnavailableElectronNativeContext()) {
		logger.error('Electron user agent has no preload API; refusing browser notification permission request', {
			userAgent: navigator.userAgent,
		});
		return;
	}
	if (isDesktop()) {
		await applyGrantedPermission(i18n);
		return;
	}
	const result = await requestBrowserPermission();
	if (result !== 'granted') {
		await handleDeniedPermission(i18n);
		return;
	}
	await applyGrantedPermission(i18n);
}

export interface NotificationResult {
	browserNotification: Notification | null;
	nativeNotificationId: string | null;
}

type WebNotificationOptions = NotificationOptions & NotificationAlertOptions;

const getWebNotificationAlertOptions = (): NotificationAlertOptions =>
	getNotificationAlertOptions({mobileOrTablet: isMobileOrTablet(), silentOnNonMobile: true});
const getServiceWorkerRegistration = async (): Promise<ServiceWorkerRegistration | null> => {
	if (typeof navigator.serviceWorker === 'undefined') {
		return null;
	}
	try {
		return (await navigator.serviceWorker.getRegistration()) ?? null;
	} catch {
		return null;
	}
};
const tryShowNotificationViaServiceWorker = async ({
	title,
	body,
	url,
	icon,
	targetUserId,
}: {
	title: string;
	body: string;
	url?: string;
	icon?: string;
	targetUserId?: string;
}): Promise<{
	shown: boolean;
	result: NotificationResult;
}> => {
	if (isDesktop() || hasUnavailableElectronNativeContext()) {
		logger.error('Blocked service-worker notification path in native desktop client', {
			hasElectronApi: Boolean(getElectronAPI()),
			userAgent: navigator.userAgent,
		});
		return {shown: false, result: {browserNotification: null, nativeNotificationId: null}};
	}
	const registration = await getServiceWorkerRegistration();
	if (!registration) {
		return {shown: false, result: {browserNotification: null, nativeNotificationId: null}};
	}
	const options: WebNotificationOptions = {body, ...getWebNotificationAlertOptions()};
	if (icon) {
		options.icon = icon;
	}
	if (url || targetUserId) {
		const data: Record<string, unknown> = {};
		if (url) data.url = url;
		if (targetUserId) data.target_user_id = targetUserId;
		options.data = data;
	}
	try {
		await registration.showNotification(title, options);
		return {shown: true, result: {browserNotification: null, nativeNotificationId: null}};
	} catch (error) {
		logger.warn('Service-worker notification show failed', {error});
		return {shown: false, result: {browserNotification: null, nativeNotificationId: null}};
	}
};
const tryShowNotificationViaWindowNotification = ({
	title,
	body,
	url,
	icon,
}: {
	title: string;
	body: string;
	url?: string;
	icon?: string;
}): NotificationResult => {
	const notificationOptions: WebNotificationOptions = icon
		? {body, icon, ...getWebNotificationAlertOptions()}
		: {body, ...getWebNotificationAlertOptions()};
	const notification = new Notification(title, notificationOptions);
	notification.addEventListener('click', (event) => {
		event.preventDefault();
		window.focus();
		if (url) {
			RouterUtils.transitionTo(url);
		}
		notification.close();
	});
	return {browserNotification: notification, nativeNotificationId: null};
};

export async function showNotification({
	id,
	title,
	subtitle,
	body,
	url,
	icon,
	playSound = true,
}: {
	id?: string;
	title: string;
	subtitle?: string;
	body: string;
	url?: string;
	icon?: string;
	playSound?: boolean;
}): Promise<NotificationResult> {
	try {
		if (StreamerMode.shouldDisableNotifications) {
			return {browserNotification: null, nativeNotificationId: null};
		}
		if (playSound) {
			playNotificationSoundIfEnabled();
		}
		const electronApi = getElectronAPI();
		if (electronApi) {
			try {
				try {
					if (typeof electronApi.flashFrame === 'function' && !document.hasFocus()) {
						electronApi.flashFrame(false);
					}
				} catch {}
				const result = await electronApi.showNotification({
					id,
					title,
					subtitle,
					body,
					icon: icon ?? '',
					url,
				});
				return {browserNotification: null, nativeNotificationId: result.id};
			} catch (error) {
				logger.error('Electron native notification show failed; refusing browser/Web Push fallback', {error});
				return {browserNotification: null, nativeNotificationId: null};
			}
		}
		if (hasUnavailableElectronNativeContext()) {
			logger.error('Electron user agent has no preload API; refusing browser/Web Push notification fallback', {
				userAgent: navigator.userAgent,
			});
			return {browserNotification: null, nativeNotificationId: null};
		}
		const targetUserId = Authentication.currentUserId ?? undefined;
		const swAttempt = await tryShowNotificationViaServiceWorker({title, body, url, icon, targetUserId});
		if (swAttempt.shown) {
			return swAttempt.result;
		}
		if (typeof Notification !== 'undefined' && Notification.permission === 'granted') {
			try {
				return tryShowNotificationViaWindowNotification({title, body, url, icon});
			} catch {
				const swFallback = await tryShowNotificationViaServiceWorker({title, body, url, icon, targetUserId});
				return swFallback.result;
			}
		}
		return swAttempt.result;
	} catch {
		return {browserNotification: null, nativeNotificationId: null};
	}
}

export function closeNativeNotification(id: string): void {
	const electronApi = getElectronAPI();
	if (electronApi) {
		electronApi.closeNotification(id);
	}
}

export function closeNativeNotifications(ids: Array<string>): void {
	if (ids.length === 0) return;
	const electronApi = getElectronAPI();
	if (electronApi) {
		electronApi.closeNotifications(ids);
	}
}
