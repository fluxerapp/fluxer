// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {GuildChannelRouter} from '@app/app/router/components/GuildChannelRouter';
import {requireAuthentication, whenAuthenticated} from '@app/app/router/policy/AuthRouteRedirectPolicy';
import {rootRoute} from '@app/app/router/routes/RootRoutes';
import {AppBadge} from '@app/features/app/components/AppNotificationBadge';
import {AppLayout} from '@app/features/app/components/layout/AppLayout';
import {DiscoveryLayout} from '@app/features/app/components/layout/DiscoveryLayout';
import {FavoritesLayout} from '@app/features/app/components/layout/FavoritesLayout';
import {GuildsLayout} from '@app/features/app/components/layout/GuildsLayout';
import {NotFoundPage} from '@app/features/app/components/pages/NotFoundPage';
import {
	AuthenticatedRuntimeInvariantError,
	readAuthenticatedRuntimeContext,
} from '@app/features/auth/state/AuthenticatedRuntime';
import {ChannelIndexPage} from '@app/features/channel/components/ChannelIndexPage';
import {ChannelLayout} from '@app/features/channel/components/ChannelLayout';
import {DMLayout} from '@app/features/channel/components/direct_message/DirectMessageLayout';
import Channels from '@app/features/channel/state/Channels';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {navigateToLinkedUserProfile} from '@app/features/navigation/utils/DeepLinkUtils';
import {getDirectMessagesFallbackPath} from '@app/features/navigation/utils/DefaultLandingUtils';
import {
	createDefaultLoadableComponent,
	createNamedLoadableComponent,
} from '@app/features/platform/components/loadable/LoadableComponent';
import {createRoute} from '@app/features/platform/components/router/RouterBuilder';
import {notFound} from '@app/features/platform/components/router/RouterErrors';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import {Redirect} from '@app/features/platform/components/router/RouterTypes';
import SessionManager from '@app/features/platform/state/AuthSession';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useEffect, useState} from 'react';

type AppRouteComponentProps = Record<string, unknown>;

function authenticatedRuntimeDisablesDirectMessages(): boolean {
	const runtimeContext = readAuthenticatedRuntimeContext();
	if (runtimeContext === null) {
		throw new AuthenticatedRuntimeInvariantError('An authenticated route requires an active account runtime');
	}
	return runtimeContext.runtime.community.direct_messages_disabled;
}

const YouPage = createNamedLoadableComponent<AppRouteComponentProps>({
	displayName: 'YouPage',
	load: async () => (await import('@app/features/app/components/pages/YouPage')).YouPage,
});
const MatureContentCheckCallbackPage = createDefaultLoadableComponent<AppRouteComponentProps>({
	displayName: 'MatureContentCheckCallbackPage',
	load: () => import('@app/features/auth/components/pages/MatureContentCheckCallbackPage'),
});
const GuildMembersPage = createNamedLoadableComponent<AppRouteComponentProps>({
	displayName: 'GuildMembersPage',
	load: async () => (await import('@app/features/channel/components/GuildMembersPage')).GuildMembersPage,
});
const BookmarksBottomSheet = createNamedLoadableComponent<AppRouteComponentProps>({
	displayName: 'BookmarksBottomSheet',
	load: async () => (await import('@app/features/channel/components/modals/BookmarksBottomSheet')).BookmarksBottomSheet,
});
const ConnectionCallbackPage = createDefaultLoadableComponent<AppRouteComponentProps>({
	displayName: 'ConnectionCallbackPage',
	load: () => import('@app/features/connection/components/pages/ConnectionCallbackPage'),
});
const NotificationsPage = createNamedLoadableComponent<AppRouteComponentProps>({
	displayName: 'NotificationsPage',
	load: async () => (await import('@app/features/notification/components/pages/NotificationsPage')).NotificationsPage,
});
const PlutoniumPageLayout = createNamedLoadableComponent<AppRouteComponentProps>({
	displayName: 'PlutoniumPageLayout',
	load: async () =>
		(await import('@app/features/premium/components/plutonium_page/PlutoniumPageLayout')).PlutoniumPageLayout,
});
const PremiumCallbackPage = createDefaultLoadableComponent<AppRouteComponentProps>({
	displayName: 'PremiumCallbackPage',
	load: () => import('@app/features/premium/components/pages/PremiumCallbackPage'),
});
const StatusChangeBottomSheet = createNamedLoadableComponent<AppRouteComponentProps>({
	displayName: 'StatusChangeBottomSheet',
	load: async () =>
		(await import('@app/features/user/components/modals/StatusChangeBottomSheet')).StatusChangeBottomSheet,
});

const ProtectedAppLayout = observer(({children}: {children: ReactNode}) => {
	if (readAuthenticatedRuntimeContext() === null && !SessionManager.isSwitching) {
		return null;
	}
	return (
		<>
			<AppBadge data-flx="app.router.app-routes.layout.app-badge" />
			<AppLayout data-flx="app.router.app-routes.layout.app-layout">{children}</AppLayout>
		</>
	);
});

const appLayoutRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'appLayout',
	onEnter: requireAuthentication,
	layout: ({children}) => (
		<ProtectedAppLayout data-flx="app.router.app-routes.layout.protected-app-layout">{children}</ProtectedAppLayout>
	),
});
const guildsLayoutRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'guildsLayout',
	layout: ({children}) => <GuildsLayout data-flx="app.router.app-routes.layout.guilds-layout">{children}</GuildsLayout>,
});
const notificationsRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'notifications',
	path: Routes.NOTIFICATIONS,
	preload: NotificationsPage.preload,
	component: () => {
		const [bookmarksSheetOpen, setBookmarksSheetOpen] = useState(false);
		const [bookmarksSheetLoaded, setBookmarksSheetLoaded] = useState(false);
		return (
			<>
				<NotificationsPage
					onBookmarksClick={() => {
						setBookmarksSheetLoaded(true);
						setBookmarksSheetOpen(true);
					}}
					data-flx="app.router.app-routes.notifications-page"
				/>
				{bookmarksSheetLoaded && (
					<BookmarksBottomSheet
						isOpen={bookmarksSheetOpen}
						onClose={() => setBookmarksSheetOpen(false)}
						data-flx="app.router.app-routes.bookmarks-bottom-sheet"
					/>
				)}
			</>
		);
	},
});
const youRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'you',
	path: Routes.YOU,
	preload: YouPage.preload,
	component: () => {
		const [statusSheetOpen, setStatusSheetOpen] = useState(false);
		const [statusSheetLoaded, setStatusSheetLoaded] = useState(false);
		return (
			<>
				<YouPage
					onAvatarClick={() => {
						setStatusSheetLoaded(true);
						setStatusSheetOpen(true);
					}}
					data-flx="app.router.app-routes.you-page"
				/>
				{statusSheetLoaded && (
					<StatusChangeBottomSheet
						isOpen={statusSheetOpen}
						onClose={() => setStatusSheetOpen(false)}
						data-flx="app.router.app-routes.status-change-bottom-sheet"
					/>
				)}
			</>
		);
	},
});
export const premiumCallbackRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'premiumCallback',
	path: Routes.PREMIUM_CALLBACK,
	onEnter: requireAuthentication,
	preload: PremiumCallbackPage.preload,
	component: () => <PremiumCallbackPage data-flx="app.router.app-routes.premium-callback-page" />,
});
export const matureContentCheckCallbackRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'matureContentCheckCallback',
	path: Routes.AGE_VERIFICATION_CALLBACK,
	preload: MatureContentCheckCallbackPage.preload,
	component: () => (
		<MatureContentCheckCallbackPage data-flx="app.router.app-routes.mature-content-check-callback-page" />
	),
});
export const connectionCallbackRoute = createRoute({
	getParentRoute: () => rootRoute,
	id: 'connectionCallback',
	path: Routes.CONNECTION_CALLBACK,
	preload: ConnectionCallbackPage.preload,
	component: () => <ConnectionCallbackPage data-flx="app.router.app-routes.connection-callback-page" />,
});
const bookmarksRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'bookmarks',
	path: Routes.BOOKMARKS,
	component: () => <DMLayout data-flx="app.router.app-routes.dm-layout" />,
});
const mentionsRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'mentions',
	path: Routes.MENTIONS,
	component: () => <DMLayout data-flx="app.router.app-routes.dm-layout--2" />,
});
const meRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'me',
	path: '/channels/@me',
	onEnter: whenAuthenticated(() => {
		if (authenticatedRuntimeDisablesDirectMessages()) {
			return new Redirect(getDirectMessagesFallbackPath());
		}
		return undefined;
	}),
	component: observer(() => {
		const isMobileLayout = MobileLayout.enabled;
		useEffect(() => {
			if (!isMobileLayout && SelectedChannel.selectedChannelIds.has(ME)) {
				SelectedChannel.clearGuildSelection(ME);
			}
		}, [isMobileLayout]);
		return <DMLayout data-flx="app.router.app-routes.dm-layout--3" />;
	}),
});
const discoverRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'discover',
	path: '/channels/@discover',
	component: () => <DiscoveryLayout data-flx="app.router.app-routes.discovery-layout" />,
});
const plutoniumRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'plutonium',
	path: Routes.PLUTONIUM,
	preload: PlutoniumPageLayout.preload,
	component: () => (
		<DMLayout data-flx="app.router.app-routes.dm-layout--plutonium">
			<PlutoniumPageLayout data-flx="app.router.app-routes.plutonium-page-layout" />
		</DMLayout>
	),
});
const legacyPlutoniumRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'legacyPlutonium',
	path: Routes.LEGACY_PLUTONIUM,
	onEnter: () => new Redirect(Routes.PLUTONIUM),
});
const userProfileRoute = createRoute({
	getParentRoute: () => appLayoutRoute,
	id: 'userProfile',
	path: Routes.USER_PROFILE,
	component: () => {
		const {userId} = useParams() as {userId: string};
		useEffect(() => {
			navigateToLinkedUserProfile(userId, {replace: true});
		}, [userId]);
		return null;
	},
});
const favoritesRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'favorites',
	path: '/channels/@favorites',
	layout: ({children}) => (
		<FavoritesLayout data-flx="app.router.app-routes.layout.favorites-layout">{children}</FavoritesLayout>
	),
});
const favoritesChannelRoute = createRoute({
	getParentRoute: () => favoritesRoute,
	id: 'favoritesChannel',
	path: '/channels/@favorites/:channelId',
	component: () => (
		<ChannelLayout data-flx="app.router.app-routes.channel-layout">
			<ChannelIndexPage data-flx="app.router.app-routes.channel-index-page" />
		</ChannelLayout>
	),
});
const channelsRoute = createRoute({
	getParentRoute: () => guildsLayoutRoute,
	id: 'channels',
	path: '/channels/:guildId',
	layout: ({children}) => {
		const params = useParams() as {guildId: string};
		const {guildId} = params;
		if (guildId === ME) {
			return <DMLayout data-flx="app.router.app-routes.layout.dm-layout">{children}</DMLayout>;
		}
		return guildId ? (
			<GuildChannelRouter guildId={guildId} data-flx="app.router.app-routes.layout.guild-channel-router">
				{children}
			</GuildChannelRouter>
		) : null;
	},
});
const membersRoute = createRoute({
	getParentRoute: () => channelsRoute,
	id: 'guildMembers',
	path: '/channels/:guildId/members',
	preload: GuildMembersPage.preload,
	component: () => {
		const {guildId} = useParams() as {guildId: string};
		return <GuildMembersPage guildId={guildId} data-flx="app.router.app-routes.guild-members-page" />;
	},
});
const channelRoute = createRoute({
	getParentRoute: () => channelsRoute,
	id: 'channel',
	path: '/channels/:guildId/:channelId',
	onEnter: whenAuthenticated((ctx) => {
		const {guildId, channelId} = ctx.params;
		if (guildId === ME && authenticatedRuntimeDisablesDirectMessages()) {
			return new Redirect(getDirectMessagesFallbackPath());
		}
		const channel = Channels.getChannel(channelId);
		if (channel && channel.type === ChannelTypes.GUILD_CATEGORY) {
			return new Redirect(Routes.guildChannel(guildId));
		}
		return undefined;
	}),
	component: () => (
		<ChannelLayout data-flx="app.router.app-routes.channel-layout--2">
			<ChannelIndexPage data-flx="app.router.app-routes.channel-index-page--2" />
		</ChannelLayout>
	),
});
const messageRoute = createRoute({
	getParentRoute: () => channelRoute,
	id: 'message',
	path: '/channels/:guildId/:channelId/:messageId',
	onEnter: whenAuthenticated((ctx) => {
		const {guildId, channelId} = ctx.params;
		if (guildId === ME && authenticatedRuntimeDisablesDirectMessages()) {
			return new Redirect(getDirectMessagesFallbackPath());
		}
		const channel = Channels.getChannel(channelId);
		if (channel && channel.type === ChannelTypes.GUILD_CATEGORY) {
			return new Redirect(Routes.guildChannel(guildId));
		}
		return undefined;
	}),
	component: () => (
		<ChannelLayout data-flx="app.router.app-routes.channel-layout--3">
			<ChannelIndexPage data-flx="app.router.app-routes.channel-index-page--3" />
		</ChannelLayout>
	),
});
const ThreadPanelRoutePage = observer(() => {
	const {guildId} = useParams() as {guildId?: string};
	if (GatewayConnection.isReady && !ThreadGuilds.isActive(guildId)) {
		return <NotFoundPage data-flx="app.router.app-routes.thread-panel-not-found-page" />;
	}
	return (
		<ChannelLayout data-flx="app.router.app-routes.channel-layout--4">
			<ChannelIndexPage data-flx="app.router.app-routes.channel-index-page--4" />
		</ChannelLayout>
	);
});
const threadPanelRoute = createRoute({
	getParentRoute: () => channelRoute,
	id: 'threadPanel',
	path: '/channels/:guildId/:channelId/threads/:threadId/:threadMessageId?',
	onEnter: (ctx) => {
		const {guildId, channelId} = ctx.params;
		if (guildId === ME || (GatewayConnection.isReady && !ThreadGuilds.isActive(guildId))) {
			return notFound();
		}
		const channel = Channels.getChannel(channelId);
		if (channel && channel.type === ChannelTypes.GUILD_CATEGORY) {
			return new Redirect(Routes.guildChannel(guildId));
		}
		return undefined;
	},
	component: () => <ThreadPanelRoutePage data-flx="app.router.app-routes.thread-panel-route-page" />,
});
export const appRouteTree = appLayoutRoute.addChildren([
	notificationsRoute,
	youRoute,
	userProfileRoute,
	guildsLayoutRoute.addChildren([
		bookmarksRoute,
		mentionsRoute,
		meRoute,
		discoverRoute,
		plutoniumRoute,
		legacyPlutoniumRoute,
		favoritesRoute.addChildren([favoritesChannelRoute]),
		channelsRoute.addChildren([membersRoute, channelRoute.addChildren([messageRoute, threadPanelRoute])]),
	]),
]);
