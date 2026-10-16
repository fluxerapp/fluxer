// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {SkeletonSimplePageRoute} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';

export const SkeletonSidebarVariant = Object.freeze({
	DM: 'dm',
	GUILD: 'guild',
	DISCOVERY: 'discovery',
} as const);

export type SkeletonSidebarVariant = (typeof SkeletonSidebarVariant)[keyof typeof SkeletonSidebarVariant];
export type SkeletonContent =
	| {kind: 'chat'}
	| {kind: 'friends'}
	| {kind: 'discovery'}
	| {kind: 'page'; route: SkeletonSimplePageRoute}
	| {kind: 'empty'};

export type SkeletonShell =
	| {chrome: 'guilds'; sidebar: SkeletonSidebarVariant; content: SkeletonContent}
	| {chrome: 'bare'; content: SkeletonContent};

const GUILD_MEMBERS_PATH_SUFFIX = '/members';
const USER_PROFILE_PATH_PREFIX = '/users/';
const GUILD_ROOT_PATH_SEGMENT_COUNT = 3;

function isGuildRootPath(pathname: string): boolean {
	return pathname.split('/').length === GUILD_ROOT_PATH_SEGMENT_COUNT;
}

function resolveDirectMessageContent(pathname: string): SkeletonContent {
	if (pathname === Routes.ME) {
		return {kind: 'friends'};
	}
	return {kind: 'chat'};
}

function resolveFavoritesContent(pathname: string): SkeletonContent {
	if (pathname === Routes.FAVORITES) {
		return {kind: 'empty'};
	}
	return {kind: 'chat'};
}

function resolveGuildContent(pathname: string): SkeletonContent {
	if (pathname.endsWith(GUILD_MEMBERS_PATH_SUFFIX)) {
		return {kind: 'page', route: SkeletonSimplePageRoute.GUILD_MEMBERS};
	}
	return {kind: 'chat'};
}

export function showsMobileGuildRail(pathname: string): boolean {
	if (pathname === Routes.ME || Routes.isDiscoverRoute(pathname)) {
		return true;
	}
	return Routes.isChannelRoute(pathname) && isGuildRootPath(pathname);
}

export function showsMobileSidebar(pathname: string): boolean {
	if (Routes.isDiscoverRoute(pathname)) {
		return false;
	}
	return showsMobileGuildRail(pathname);
}

export function showsMobileBottomNav(pathname: string): boolean {
	if (pathname === Routes.ME) return true;
	if (pathname === Routes.FAVORITES) return true;
	if (pathname === Routes.NOTIFICATIONS) return true;
	if (pathname === Routes.YOU) return true;
	return Routes.isGuildChannelRoute(pathname) && isGuildRootPath(pathname);
}

export function reservesMobileBottomNavSpace(pathname: string): boolean {
	return showsMobileBottomNav(pathname) || Routes.isDiscoverRoute(pathname);
}

export function resolveSkeletonShell(pathname: string): SkeletonShell {
	if (pathname === Routes.YOU || pathname.startsWith(USER_PROFILE_PATH_PREFIX)) {
		return {chrome: 'bare', content: {kind: 'empty'}};
	}
	if (pathname === Routes.NOTIFICATIONS) {
		return {chrome: 'bare', content: {kind: 'page', route: SkeletonSimplePageRoute.NOTIFICATIONS}};
	}
	if (Routes.isDiscoverRoute(pathname)) {
		return {chrome: 'guilds', sidebar: SkeletonSidebarVariant.DISCOVERY, content: {kind: 'discovery'}};
	}
	if (pathname === Routes.BOOKMARKS) {
		return {
			chrome: 'guilds',
			sidebar: SkeletonSidebarVariant.DM,
			content: {kind: 'page', route: SkeletonSimplePageRoute.BOOKMARKS},
		};
	}
	if (pathname === Routes.MENTIONS) {
		return {
			chrome: 'guilds',
			sidebar: SkeletonSidebarVariant.DM,
			content: {kind: 'page', route: SkeletonSimplePageRoute.MENTIONS},
		};
	}
	if (Routes.isDMRoute(pathname)) {
		return {
			chrome: 'guilds',
			sidebar: SkeletonSidebarVariant.DM,
			content: resolveDirectMessageContent(pathname),
		};
	}
	if (Routes.isFavoritesRoute(pathname)) {
		return {
			chrome: 'guilds',
			sidebar: SkeletonSidebarVariant.GUILD,
			content: resolveFavoritesContent(pathname),
		};
	}
	if (Routes.isGuildChannelRoute(pathname)) {
		return {
			chrome: 'guilds',
			sidebar: SkeletonSidebarVariant.GUILD,
			content: resolveGuildContent(pathname),
		};
	}
	return {chrome: 'guilds', sidebar: SkeletonSidebarVariant.DM, content: {kind: 'friends'}};
}
