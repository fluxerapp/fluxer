// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConnectionNagbar} from '@app/features/app/components/layout/app_layout/nagbars/ConnectionNagbar';
import {OutlineFrame} from '@app/features/app/components/layout/OutlineFrame';
import styles from '@app/features/app/components/skeleton/AppSkeleton.module.css';
import {ChatSkeleton} from '@app/features/app/components/skeleton/ChatSkeleton';
import {DiscoveryPageSkeleton, DiscoverySidebarSkeleton} from '@app/features/app/components/skeleton/DiscoverySkeleton';
import {DMSidebarSkeleton} from '@app/features/app/components/skeleton/DMSidebarSkeleton';
import {FriendsSkeleton} from '@app/features/app/components/skeleton/FriendsSkeleton';
import {GuildRailSkeleton} from '@app/features/app/components/skeleton/GuildRailSkeleton';
import {GuildSidebarSkeleton} from '@app/features/app/components/skeleton/GuildSidebarSkeleton';
import {MobileBottomNavSkeleton} from '@app/features/app/components/skeleton/MobileBottomNavSkeleton';
import {
	type ChatSkeletonPresentation,
	resolveChatSkeletonPresentation,
} from '@app/features/app/components/skeleton/ResolveChatSkeleton';
import {
	reservesMobileBottomNavSpace,
	resolveSkeletonShell,
	type SkeletonContent,
	SkeletonSidebarVariant,
	showsMobileBottomNav,
	showsMobileGuildRail,
	showsMobileSidebar,
} from '@app/features/app/components/skeleton/ResolveSkeletonShell';
import {SimplePageSkeleton} from '@app/features/app/components/skeleton/SimplePageSkeleton';
import {
	getRememberedSkeletonNagbarLayout,
	getRememberedSkeletonVoicePresence,
	SKELETON_DEFAULT_NAGBAR_LAYOUT,
	SKELETON_DEFAULT_VOICE_PRESENCE,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {UserAreaSkeleton} from '@app/features/app/components/skeleton/UserAreaSkeleton';
import KeybindManager from '@app/features/app/keybindings/KeybindManager';
import {useCanFitMemberList} from '@app/features/member/hooks/useMemberListVisible';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import SidebarWidth from '@app/features/ui/state/SidebarWidth';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useLayoutEffect, useRef, useState} from 'react';

const SIDEBAR_WIDTH_VARIABLE = '--layout-sidebar-width';
const VOICE_CONNECTION_HEIGHT_VARIABLE = '--layout-voice-connection-height';
const GUILD_ID_PATH_INDEX = 2;

function resolveSkeletonGuildId(pathname: string): string | null {
	if (!Routes.isGuildChannelRoute(pathname)) {
		return null;
	}
	return pathname.split('/')[GUILD_ID_PATH_INDEX] ?? null;
}

function renderSidebar(sidebar: SkeletonSidebarVariant, guildId: string | null) {
	switch (sidebar) {
		case SkeletonSidebarVariant.DM:
			return <DMSidebarSkeleton data-flx="app.skeleton.app-skeleton.render-sidebar.dm-sidebar-skeleton" />;
		case SkeletonSidebarVariant.GUILD:
			return (
				<GuildSidebarSkeleton
					guildId={guildId}
					data-flx="app.skeleton.app-skeleton.render-sidebar.guild-sidebar-skeleton"
				/>
			);
		case SkeletonSidebarVariant.DISCOVERY:
			return (
				<DiscoverySidebarSkeleton data-flx="app.skeleton.app-skeleton.render-sidebar.discovery-sidebar-skeleton" />
			);
	}
}

function renderContent(content: SkeletonContent, chatPresentation: ChatSkeletonPresentation) {
	switch (content.kind) {
		case 'chat':
			return (
				<ChatSkeleton
					presentation={chatPresentation}
					data-flx="app.skeleton.app-skeleton.render-content.chat-skeleton"
				/>
			);
		case 'friends':
			return <FriendsSkeleton data-flx="app.skeleton.app-skeleton.render-content.friends-skeleton" />;
		case 'discovery':
			return <DiscoveryPageSkeleton data-flx="app.skeleton.app-skeleton.render-content.discovery-page-skeleton" />;
		case 'page':
			return (
				<SimplePageSkeleton
					route={content.route}
					data-flx="app.skeleton.app-skeleton.render-content.simple-page-skeleton"
				/>
			);
		case 'empty':
			return null;
	}
}

interface AppSkeletonProps {
	readonly isExiting?: boolean;
	readonly onTransitionEnd?: React.TransitionEventHandler<HTMLElement>;
	readonly effectivePathname?: string;
}

interface FrozenChatSkeletonPresentation {
	readonly pathname: string;
	readonly isMobile: boolean;
	readonly canFitMemberList: boolean;
	readonly presentation: ChatSkeletonPresentation;
}

export const AppSkeleton = observer(({isExiting = false, onTransitionEnd, effectivePathname}: AppSkeletonProps) => {
	const location = useLocation();
	const pathname = effectivePathname ?? location.pathname;
	const isFluxerSelected = pathname.startsWith(Routes.ME) || Routes.isSpecialPage(pathname);
	const isFavoritesSelected = pathname.startsWith(Routes.FAVORITES);
	const isDiscoverySelected = Routes.isDiscoverRoute(pathname);
	const isMobile = MobileLayout.enabled;
	const isVoiceCallFullscreenActive = VoiceCallFullscreen.isActive;
	const shell = resolveSkeletonShell(pathname);
	const guildId = resolveSkeletonGuildId(pathname);
	const [rememberedChrome] = useState(() =>
		Object.freeze({
			nagbar: getRememberedSkeletonNagbarLayout() ?? SKELETON_DEFAULT_NAGBAR_LAYOUT,
			voice: getRememberedSkeletonVoicePresence() ?? SKELETON_DEFAULT_VOICE_PRESENCE,
		}),
	);
	const voicePanelHeightPx = rememberedChrome.voice.panelHeightPx;
	const canFitMemberList = useCanFitMemberList();
	const frozenChatPresentationRef = useRef<FrozenChatSkeletonPresentation | null>(null);
	let frozenChatPresentation = frozenChatPresentationRef.current;
	if (
		frozenChatPresentation == null ||
		frozenChatPresentation.pathname !== pathname ||
		frozenChatPresentation.isMobile !== isMobile ||
		frozenChatPresentation.canFitMemberList !== canFitMemberList
	) {
		frozenChatPresentation = {
			pathname,
			isMobile,
			canFitMemberList,
			presentation: resolveChatSkeletonPresentation(pathname, !isMobile && canFitMemberList),
		};
		frozenChatPresentationRef.current = frozenChatPresentation;
	}
	const chatPresentation = frozenChatPresentation.presentation;
	useLayoutEffect(() => {
		if (isExiting) return;
		KeybindManager.suspend();
		return () => KeybindManager.resume();
	}, [isExiting]);
	const showMobileBottomNav = isMobile && showsMobileBottomNav(pathname);
	if (shell.chrome === 'bare') {
		return (
			<flx-app-skeleton
				className={flxElementClassName(styles.container, isExiting && styles.containerExiting)}
				aria-busy={isExiting ? undefined : 'true'}
				aria-hidden={isExiting || undefined}
				onTransitionEnd={onTransitionEnd}
				data-flx="app.skeleton.app-skeleton.container"
			>
				{renderContent(shell.content, chatPresentation)}
				{showMobileBottomNav && (
					<MobileBottomNavSkeleton
						voiceItemVisible={rememberedChrome.voice.connected}
						data-flx="app.skeleton.app-skeleton.mobile-bottom-nav-skeleton"
					/>
				)}
			</flx-app-skeleton>
		);
	}
	const showMobileSidebarInline = isMobile && showsMobileSidebar(pathname);
	const showRail = !isMobile || showsMobileGuildRail(pathname);
	const reserveMobileBottomNav = isMobile && reservesMobileBottomNavSpace(pathname);
	const showUserArea = !isMobile && !isVoiceCallFullscreenActive;
	const sidebarWidth = SidebarWidth.cssValue;
	const shellStyle: React.CSSProperties = {};
	if (!isMobile && sidebarWidth != null && sidebarWidth !== '') {
		Object.assign(shellStyle, {[SIDEBAR_WIDTH_VARIABLE]: sidebarWidth});
	}
	if (showUserArea) {
		Object.assign(shellStyle, {[VOICE_CONNECTION_HEIGHT_VARIABLE]: `${voicePanelHeightPx}px`});
	}
	return (
		<flx-app-skeleton
			className={flxElementClassName(
				styles.container,
				styles.containerGuilds,
				!showRail && styles.containerRailless,
				showUserArea && styles.containerReserveUserArea,
				reserveMobileBottomNav && styles.containerReserveMobileBottomNav,
				isExiting && styles.containerExiting,
			)}
			style={shellStyle}
			aria-busy={isExiting ? undefined : 'true'}
			aria-hidden={isExiting || undefined}
			onTransitionEnd={onTransitionEnd}
			data-flx="app.skeleton.app-skeleton.container--2"
		>
			{showRail && (
				<GuildRailSkeleton
					isFluxerSelected={isFluxerSelected}
					isFavoritesSelected={isFavoritesSelected}
					isDiscoverySelected={isDiscoverySelected}
					data-flx="app.skeleton.app-skeleton.guild-rail-skeleton"
				/>
			)}
			<flx-app-skeleton-content
				className={flxElementClassName(styles.content)}
				data-flx="app.skeleton.app-skeleton.content"
			>
				<OutlineFrame
					className={styles.outlineFrame}
					sidebarDivider={!isMobile}
					nagbar={
						<ConnectionNagbar
							isMobile={isMobile}
							rememberedRows={rememberedChrome.nagbar.rows}
							data-flx="app.skeleton.app-skeleton.connection-nagbar"
						/>
					}
					data-flx="app.skeleton.app-skeleton.outline-frame"
				>
					<flx-app-skeleton-main
						className={flxElementClassName(styles.main, isMobile && styles.mainSidebarless)}
						data-flx="app.skeleton.app-skeleton.main"
					>
						{!isMobile && renderSidebar(shell.sidebar, guildId)}
						<flx-app-skeleton-content-column
							className={flxElementClassName(styles.contentColumn)}
							data-flx="app.skeleton.app-skeleton.content-column"
						>
							{showMobileSidebarInline
								? renderSidebar(shell.sidebar, guildId)
								: renderContent(shell.content, chatPresentation)}
						</flx-app-skeleton-content-column>
					</flx-app-skeleton-main>
				</OutlineFrame>
			</flx-app-skeleton-content>
			{showUserArea && (
				<UserAreaSkeleton
					voicePanelHeightPx={voicePanelHeightPx}
					data-flx="app.skeleton.app-skeleton.user-area-skeleton"
				/>
			)}
			{showMobileBottomNav && (
				<MobileBottomNavSkeleton
					voiceItemVisible={rememberedChrome.voice.connected}
					data-flx="app.skeleton.app-skeleton.mobile-bottom-nav-skeleton--2"
				/>
			)}
		</flx-app-skeleton>
	);
});
