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

import NagbarStore, {type NagbarToggleKey} from '~/stores/NagbarStore';

export const dismissNagbar = (nagbarType: NagbarToggleKey): void => {
	NagbarStore.dismiss(nagbarType);
};

export const dismissInvitesDisabledNagbar = (guildId: string): void => {
	NagbarStore.dismissInvitesDisabled(guildId);
};

export const resetNagbar = (nagbarType: NagbarToggleKey): void => {
	NagbarStore.reset(nagbarType);
};

export const resetAllNagbars = (): void => {
	NagbarStore.resetAll();
};

export const setForceHideNagbar = (key: NagbarToggleKey, value: boolean): void => {
	NagbarStore.setFlag(key, value);
};

export const dismissPendingBulkDeletionNagbar = (scheduleKey: string): void => {
	NagbarStore.dismissPendingBulkDeletion(scheduleKey);
};

export const clearPendingBulkDeletionNagbarDismissal = (scheduleKey: string): void => {
	NagbarStore.clearPendingBulkDeletionDismissed(scheduleKey);
};
