// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {SidebarPreferencesSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

const DEFAULT_SHOW_COLLAPSED_UNREAD_DMS_BADGE = true;
const DEFAULT_SHOW_INCOMING_FRIEND_REQUEST_BADGE = true;

class SidebarPreferences {
	inlineDmsCollapsed = false;
	showCollapsedUnreadDmsBadge = DEFAULT_SHOW_COLLAPSED_UNREAD_DMS_BADGE;
	showIncomingFriendRequestBadge = DEFAULT_SHOW_INCOMING_FRIEND_REQUEST_BADGE;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () =>
			makeSyncedField(this, {
				field: 'sidebar',
				schema: SidebarPreferencesSchema,
				persist: ['inlineDmsCollapsed', 'showCollapsedUnreadDmsBadge', 'showIncomingFriendRequestBadge'],
				toMessage: (s) => ({
					inlineDmsCollapsed: s.inlineDmsCollapsed,
					showCollapsedUnreadDmsBadge:
						s.showCollapsedUnreadDmsBadge === DEFAULT_SHOW_COLLAPSED_UNREAD_DMS_BADGE
							? undefined
							: s.showCollapsedUnreadDmsBadge,
					showIncomingFriendRequestBadge:
						s.showIncomingFriendRequestBadge === DEFAULT_SHOW_INCOMING_FRIEND_REQUEST_BADGE
							? undefined
							: s.showIncomingFriendRequestBadge,
				}),
				applyMessage: (s, m) => {
					s.inlineDmsCollapsed = m.inlineDmsCollapsed;
					s.showCollapsedUnreadDmsBadge = m.showCollapsedUnreadDmsBadge ?? DEFAULT_SHOW_COLLAPSED_UNREAD_DMS_BADGE;
					s.showIncomingFriendRequestBadge =
						m.showIncomingFriendRequestBadge ?? DEFAULT_SHOW_INCOMING_FRIEND_REQUEST_BADGE;
				},
			}),
		);
	}

	toggleInlineDmsCollapsed(): void {
		this.inlineDmsCollapsed = !this.inlineDmsCollapsed;
	}

	setShowCollapsedUnreadDmsBadge(value: boolean): void {
		this.showCollapsedUnreadDmsBadge = value;
	}

	setShowIncomingFriendRequestBadge(value: boolean): void {
		this.showIncomingFriendRequestBadge = value;
	}
}

export default new SidebarPreferences();
