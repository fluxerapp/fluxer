// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import Notification from '@app/features/ui/state/Notification';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {updateDocumentTitleBadge} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import Favico from 'favico.js';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const logger = new Logger('AppBadge');
const UNREAD_INDICATOR = -1;

let favico: Favico | null = null;

const initFavico = (): Favico | null => {
	if (favico) return favico;
	try {
		favico = new Favico({animation: 'none'});
		return favico;
	} catch (e) {
		logger.warn('Failed to initialize Favico', e);
		return null;
	}
};
const setElectronBadge = (badge: number): void => {
	const electronApi = getElectronAPI();
	if (!electronApi?.setBadgeCount) return;
	const electronBadge = badge > 0 ? badge : 0;
	try {
		electronApi.setBadgeCount(electronBadge);
	} catch (e) {
		logger.warn('Failed to set Electron badge', e);
	}
};
const setFaviconBadge = (badge: number): void => {
	const fav = initFavico();
	if (!fav) return;
	try {
		if (badge === UNREAD_INDICATOR) {
			fav.badge('•');
		} else {
			fav.badge(badge);
		}
	} catch (e) {
		logger.warn('Failed to set favicon badge', e);
	}
};
const setPwaBadge = (badge: number): void => {
	if (!navigator.setAppBadge || !navigator.clearAppBadge) {
		return;
	}
	try {
		if (badge > 0) {
			void navigator.setAppBadge(badge);
		} else if (badge === UNREAD_INDICATOR) {
			void navigator.setAppBadge();
		} else {
			void navigator.clearAppBadge();
		}
	} catch (e) {
		logger.warn('Failed to set PWA badge', e);
	}
};
const setBadge = (badge: number): void => {
	setElectronBadge(badge);
	setFaviconBadge(badge);
	setPwaBadge(badge);
};
export const AppBadge: React.FC = observer(() => {
	const relationships = Relationships.getRelationships();
	const unreadMessageBadgeEnabled = Notification.unreadMessageBadgeEnabled;
	const mentionCount = GuildReadState.mentionCountAcrossGuilds();
	const hasUnread = GuildReadState.anyGuildUnread;
	const pendingCount = RuntimeConfig.directMessagesDisabled
		? 0
		: relationships.filter((relationship) => relationship.type === RelationshipTypes.INCOMING_REQUEST).length;
	const totalCount = mentionCount + pendingCount;
	let badge: number = 0;
	if (totalCount > 0) {
		badge = totalCount;
	} else if (hasUnread && unreadMessageBadgeEnabled) {
		badge = UNREAD_INDICATOR;
	}
	useEffect(() => {
		setBadge(badge);
	}, [badge]);
	useEffect(() => {
		updateDocumentTitleBadge(totalCount, hasUnread && unreadMessageBadgeEnabled);
	}, [totalCount, hasUnread, unreadMessageBadgeEnabled]);
	useEffect(() => {
		return () => {
			setBadge(0);
			updateDocumentTitleBadge(0, false);
		};
	}, []);
	return null;
});
