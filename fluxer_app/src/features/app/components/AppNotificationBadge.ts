// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {getDocumentFaviconUrl} from '@app/features/app/state/RuntimeDocumentBranding';
import DesktopBackgroundGateway from '@app/features/gateway/transport/DesktopBackgroundGateway';
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
let favicoLink: HTMLLinkElement | null = null;
let favicoLinkHref: string | null = null;
let favicoSource: string | null = null;

const findIconLink = (): HTMLLinkElement | null => {
	const links = document.head.querySelectorAll<HTMLLinkElement>('link');
	for (let index = links.length - 1; index >= 0; index--) {
		if (/(^|\s)icon(\s|$)/i.test(links[index].getAttribute('rel') ?? '')) {
			return links[index];
		}
	}
	return null;
};

const releaseFavico = (): void => {
	if (favicoLink !== null && favicoLinkHref !== null && favicoLink.getAttribute('href')?.startsWith('data:')) {
		favicoLink.setAttribute('href', favicoLinkHref);
	}
	favico = null;
	favicoLink = null;
	favicoLinkHref = null;
};

const initFavico = (): Favico | null => {
	if (favico) return favico;
	try {
		const link = findIconLink();
		favicoLink = link;
		favicoLinkHref = link?.getAttribute('href') ?? null;
		favico = new Favico(link === null ? {animation: 'none'} : {animation: 'none', element: link});
		return favico;
	} catch (e) {
		logger.warn('Failed to initialize Favico', e);
		releaseFavico();
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
const setFaviconBadge = (badge: number, source: string | null): void => {
	if (source !== favicoSource) {
		releaseFavico();
		favicoSource = source;
	}
	if (badge === 0 && favico === null) return;
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
const setBadge = (badge: number, faviconSource: string | null): void => {
	setElectronBadge(badge);
	setFaviconBadge(badge, faviconSource);
	setPwaBadge(badge);
};
export const AppBadge: React.FC = observer(() => {
	const relationships = Relationships.getRelationships();
	const unreadMessageBadgeEnabled = Notification.unreadMessageBadgeEnabled;
	const mentionCount = GuildReadState.mentionCountAcrossGuilds() + DesktopBackgroundGateway.totalMentionCount;
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
	const faviconSource = getDocumentFaviconUrl(RuntimeConfig.getSnapshotOrNull()?.appPublic ?? null);
	useEffect(() => {
		setBadge(badge, faviconSource);
	}, [badge, faviconSource]);
	useEffect(() => {
		updateDocumentTitleBadge(totalCount, hasUnread && unreadMessageBadgeEnabled);
	}, [totalCount, hasUnread, unreadMessageBadgeEnabled]);
	useEffect(() => {
		return () => {
			setBadge(0, favicoSource);
			updateDocumentTitleBadge(0, false);
		};
	}, []);
	return null;
});
