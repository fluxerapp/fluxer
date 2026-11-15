// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {computeAppBadge, UNREAD_INDICATOR} from '@app/features/app/components/AppBadgeCount';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {findActiveIconLink, getDocumentFavicons} from '@app/features/app/state/RuntimeDocumentBranding';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import Theme from '@app/features/theme/state/Theme';
import Notification from '@app/features/ui/state/Notification';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {updateDocumentTitleBadge} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import Favico from 'favico.js';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const logger = new Logger('AppBadge');

let favico: Favico | null = null;
let favicoLink: HTMLLinkElement | null = null;
let favicoLinkHref: string | null = null;
let favicoSource: string | null = null;

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
		const link = findActiveIconLink();
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
	try {
		if (electronApi?.setUnreadBadge) {
			electronApi.setUnreadBadge(badge);
		} else {
			electronApi?.setBadgeCount?.(Math.max(badge, 0));
		}
	} catch (e) {
		logger.warn('Failed to set Electron badge', e);
	}
};
const setFaviconBadge = (badge: number, source: string | null): void => {
	if (isDesktop()) {
		return;
	}
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
	if (isDesktop() || !navigator.setAppBadge || !navigator.clearAppBadge) {
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
function appBadgeMentionCount(): number {
	return GuildReadState.mentionCountAcrossGuilds(RuntimeConfig.directMessagesDisabled);
}

export const AppBadge: React.FC = observer(() => {
	const relationships = Relationships.getRelationships();
	const {badge, totalCount, showUnread} = computeAppBadge({
		foregroundMentionCount: appBadgeMentionCount(),
		otherAccountsMentionCount: AccountEngine.otherAccountsMentionCount,
		pendingFriendRequestCount: RuntimeConfig.friendsDisabled
			? 0
			: relationships.filter((relationship) => relationship.type === RelationshipTypes.INCOMING_REQUEST).length,
		foregroundHasUnread: GuildReadState.anyGuildUnread,
		otherAccountsHaveUnread: AccountEngine.otherAccountsHaveUnread,
		unreadMessageBadgeEnabled: Notification.unreadMessageBadgeEnabled,
	});
	const favicons = getDocumentFavicons(RuntimeConfig.getSnapshotOrNull()?.appPublic ?? null);
	const faviconSource = favicons.dark !== null && Theme.systemPrefersDark ? favicons.dark : favicons.light;
	useEffect(() => {
		setBadge(badge, faviconSource);
	}, [badge, faviconSource]);
	useEffect(() => {
		updateDocumentTitleBadge(totalCount, showUnread);
	}, [totalCount, showUnread]);
	useEffect(() => {
		return () => {
			setBadge(0, favicoSource);
			updateDocumentTitleBadge(0, false);
		};
	}, []);
	return null;
});
