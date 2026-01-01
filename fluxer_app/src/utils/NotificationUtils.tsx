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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as NotificationActionCreators from '~/actions/NotificationActionCreators';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import AuthenticationStore from '~/stores/AuthenticationStore';
import SoundStore from '~/stores/SoundStore';
import UserStore from '~/stores/UserStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {getElectronAPI, isDesktop} from '~/utils/NativeUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import {SoundType} from '~/utils/SoundUtils';

let notificationClickHandlerInitialized = false;

export const ensureDesktopNotificationClickHandler = (): void => {
	if (notificationClickHandlerInitialized) return;

	const electronApi = getElectronAPI();
	if (!electronApi) return;

	notificationClickHandlerInitialized = true;

	electronApi.onNotificationClick((_id: string, url?: string) => {
		if (url) {
			RouterUtils.transitionTo(url);
		}
	});
};

export const hasNotification = (): boolean => {
	if (isDesktop()) return true;
	return typeof Notification !== 'undefined';
};

export const isGranted = async (): Promise<boolean> => {
	if (isDesktop()) return true;
	return typeof Notification !== 'undefined' && Notification.permission === 'granted';
};

export const playNotificationSoundIfEnabled = (): void => {
	if (!SoundStore.isSoundTypeEnabled(SoundType.Message)) return;
	SoundActionCreators.playSound(SoundType.Message);
};

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
	const currentUserId = AuthenticationStore.currentUserId;
	if (!currentUserId) return null;

	const currentUser = UserStore.getUser(currentUserId);
	if (!currentUser) return null;

	return AvatarUtils.getUserAvatarURL(currentUser);
};

export const requestPermission = async (i18n: I18n): Promise<void> => {
	if (isDesktop()) {
		NotificationActionCreators.permissionGranted();
		playNotificationSoundIfEnabled();

		const icon = getCurrentUserAvatar() ?? '';
		void showNotification({
			title: i18n._(msg`Access granted`),
			body: i18n._(msg`Huzzah! Desktop notifications are enabled`),
			icon,
		});

		return;
	}

	const result = await requestBrowserPermission();
	if (result !== 'granted') {
		NotificationActionCreators.permissionDenied(i18n);
		return;
	}

	NotificationActionCreators.permissionGranted();
	playNotificationSoundIfEnabled();

	const icon = getCurrentUserAvatar() ?? '';
	void showNotification({
		title: i18n._(msg`Access granted`),
		body: i18n._(msg`Huzzah! Browser notifications are enabled`),
		icon,
	});
};

export interface NotificationResult {
	browserNotification: Notification | null;
	nativeNotificationId: string | null;
}

export const showNotification = async ({
	title,
	body,
	url,
	icon,
	playSound = true,
}: {
	title: string;
	body: string;
	url?: string;
	icon?: string;
	playSound?: boolean;
}): Promise<NotificationResult> => {
	if (playSound) {
		playNotificationSoundIfEnabled();
	}

	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			const result = await electronApi.showNotification({
				title,
				body,
				icon: icon ?? '',
				url,
			});
			return {browserNotification: null, nativeNotificationId: result.id};
		} catch {
			return {browserNotification: null, nativeNotificationId: null};
		}
	}

	if (typeof Notification !== 'undefined' && Notification.permission === 'granted') {
		const notificationOptions: NotificationOptions = icon ? {body, icon} : {body};
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
	}

	return {browserNotification: null, nativeNotificationId: null};
};

export const closeNativeNotification = (id: string): void => {
	const electronApi = getElectronAPI();
	if (electronApi) {
		electronApi.closeNotification(id);
	}
};

export const closeNativeNotifications = (ids: Array<string>): void => {
	if (ids.length === 0) return;

	const electronApi = getElectronAPI();
	if (electronApi) {
		electronApi.closeNotifications(ids);
	}
};
