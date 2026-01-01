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

import i18n from '~/i18n';
import CaptchaInterceptor from '~/lib/CaptchaInterceptor';
import ChannelDisplayNameStore from '~/stores/ChannelDisplayNameStore';
import KeybindStore from '~/stores/KeybindStore';
import NewDeviceMonitoringStore from '~/stores/NewDeviceMonitoringStore';
import NotificationStore from '~/stores/NotificationStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import MediaEngineFacade from '~/stores/voice/MediaEngineFacade';

export function setupI18nStores(): void {
	QuickSwitcherStore.setI18n(i18n);
	ChannelDisplayNameStore.setI18n(i18n);
	KeybindStore.setI18n(i18n);
	NewDeviceMonitoringStore.setI18n(i18n);
	NotificationStore.setI18n(i18n);
	MediaEngineFacade.setI18n(i18n);
	CaptchaInterceptor.setI18n(i18n);
}
