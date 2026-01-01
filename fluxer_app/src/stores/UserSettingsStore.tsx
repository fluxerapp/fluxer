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

import {camelCase, isArray, isEqual, isPlainObject, mapKeys, mapValues, snakeCase} from 'lodash';
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';
import type {StatusType} from '~/Constants';
import {
	normalizeStatus,
	RenderSpoilers,
	StatusTypes,
	StickerAnimationOptions,
	ThemeTypes,
	TimeFormatTypes,
} from '~/Constants';
import {Endpoints} from '~/Endpoints';
import {loadLocaleCatalog} from '~/i18n';
import {type CustomStatus, normalizeCustomStatus} from '~/lib/customStatus';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import {persistTheme} from '~/lib/themePersistence';
import AccessibilityOverrideStore from '~/stores/AccessibilityOverrideStore';
import AccessibilityStore from '~/stores/AccessibilityStore';
import LocalPresenceStore from '~/stores/LocalPresenceStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';

interface GuildFolder {
	id: number | null;
	name: string | null;
	color: number | null;
	guildIds: Array<string>;
}

export interface UserSettings {
	flags: number;
	status: string;
	statusResetsAt: string | null;
	statusResetsTo: string | null;
	theme: string;
	timeFormat: number;
	guildPositions: Array<string>;
	locale: string;
	restrictedGuilds: Array<string>;
	defaultGuildsRestricted: boolean;
	inlineAttachmentMedia: boolean;
	inlineEmbedMedia: boolean;
	gifAutoPlay: boolean;
	renderEmbeds: boolean;
	renderReactions: boolean;
	animateEmoji: boolean;
	animateStickers: number;
	renderSpoilers: number;
	messageDisplayCompact: boolean;
	developerMode: boolean;
	friendSourceFlags: number;
	incomingCallFlags: number;
	groupDmAddPermissionFlags: number;
	guildFolders: Array<GuildFolder>;
	customStatus: CustomStatus | null;
	afkTimeout: number;
}

const logger = new Logger('UserSettingsStore');
const VALID_THEME_VALUES = new Set<string>(Object.values(ThemeTypes));
type ThemeValue = (typeof ThemeTypes)[keyof typeof ThemeTypes];

function normalizeTheme(theme: string | null | undefined): ThemeValue | null {
	if (theme && VALID_THEME_VALUES.has(theme)) {
		return theme as ThemeValue;
	}
	return null;
}

function convertKeysToCamelCaseInternal(value: unknown): unknown {
	if (isArray(value)) {
		return value.map(convertKeysToCamelCaseInternal);
	}

	if (isPlainObject(value)) {
		const record = value as Record<string, unknown>;
		return mapValues(
			mapKeys(record, (_, key) => camelCase(key)),
			convertKeysToCamelCaseInternal,
		);
	}

	return value;
}

function convertKeysToCamelCase<T>(obj: unknown): T {
	return convertKeysToCamelCaseInternal(obj) as T;
}

function convertKeysToSnakeCaseInternal(value: unknown): unknown {
	if (isArray(value)) {
		return value.map(convertKeysToSnakeCaseInternal);
	}

	if (isPlainObject(value)) {
		const record = value as Record<string, unknown>;
		return mapValues(
			mapKeys(record, (_, key) => snakeCase(key)),
			convertKeysToSnakeCaseInternal,
		);
	}

	return value;
}

function convertKeysToSnakeCase<T>(obj: unknown): T {
	return convertKeysToSnakeCaseInternal(obj) as T;
}

class UserSettingsStore {
	flags: number = 0;
	status: StatusType = StatusTypes.ONLINE;
	statusResetsAt: string | null = null;
	statusResetsTo: string | null = null;
	theme: string = ThemeTypes.SYSTEM;
	timeFormat: number = TimeFormatTypes.AUTO;
	guildPositions: Array<string> = [];
	locale: string = 'en-US';
	restrictedGuilds: Array<string> = [];
	defaultGuildsRestricted: boolean = false;
	inlineAttachmentMedia: boolean = true;
	inlineEmbedMedia: boolean = true;
	gifAutoPlay: boolean = true;
	renderEmbeds: boolean = true;
	renderReactions: boolean = true;
	animateEmoji: boolean = true;
	animateStickers: number = StickerAnimationOptions.ALWAYS_ANIMATE;
	renderSpoilers: number = RenderSpoilers.ON_CLICK;
	messageDisplayCompact: boolean = false;
	developerMode: boolean = false;
	friendSourceFlags: number = 0;
	incomingCallFlags: number = 0;
	groupDmAddPermissionFlags: number = 0;
	guildFolders: Array<GuildFolder> = [];
	customStatus: CustomStatus | null = null;
	afkTimeout: number = 600;
	systemDarkMode = false;
	mediaQuery: MediaQueryList | null = null;
	private hydrated = false;

	constructor() {
		makeAutoObservable(this, {mediaQuery: false}, {autoBind: true});
		this.initializeSystemThemeDetection();
	}

	private initializeSystemThemeDetection() {
		if (window.matchMedia) {
			this.mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
			this.systemDarkMode = this.mediaQuery.matches;
			this.mediaQuery.addEventListener('change', this.handleSystemThemeChange);
		}
	}

	private handleSystemThemeChange = (event: MediaQueryListEvent) => {
		this.systemDarkMode = event.matches;
	};

	destroy(): void {
		if (this.mediaQuery) {
			this.mediaQuery.removeEventListener('change', this.handleSystemThemeChange);
			this.mediaQuery = null;
		}
	}

	getFlags(): number {
		return this.flags;
	}

	getStatus(): StatusType {
		return this.status;
	}

	getStatusResetsAt(): string | null {
		return this.statusResetsAt;
	}

	getStatusResetsTo(): string | null {
		return this.statusResetsTo;
	}

	getTheme(): string {
		if (!AccessibilityStore.syncThemeAcrossDevices) {
			const localOverride = AccessibilityStore.localThemeOverride;
			if (localOverride !== null) {
				if (localOverride === 'system') {
					return this.systemDarkMode ? 'dark' : 'light';
				}
				return localOverride;
			}
		}
		if (this.theme === 'system') {
			return this.systemDarkMode ? 'dark' : 'light';
		}
		return this.theme;
	}

	getTimeFormat(): number {
		return this.timeFormat;
	}

	getGuildPositions(): ReadonlyArray<string> {
		return this.guildPositions;
	}

	getLocale(): string {
		return this.locale;
	}

	getRestrictedGuilds(): ReadonlyArray<string> {
		return this.restrictedGuilds;
	}

	getDefaultGuildsRestricted(): boolean {
		return this.defaultGuildsRestricted;
	}

	getInlineAttachmentMedia(): boolean {
		return this.inlineAttachmentMedia;
	}

	getInlineEmbedMedia(): boolean {
		return this.inlineEmbedMedia;
	}

	getGifAutoPlay(): boolean {
		if (MobileLayoutStore.isMobileLayout()) {
			if (!AccessibilityStore.mobileGifAutoPlayOverridden) {
				return false;
			}
			return AccessibilityStore.mobileGifAutoPlayValue;
		}
		return this.gifAutoPlay;
	}

	getRenderEmbeds(): boolean {
		return this.renderEmbeds;
	}

	getRenderReactions(): boolean {
		return this.renderReactions;
	}

	getAnimateEmoji(): boolean {
		if (MobileLayoutStore.isMobileLayout()) {
			if (AccessibilityStore.mobileAnimateEmojiOverridden) {
				return AccessibilityStore.mobileAnimateEmojiValue;
			}
		}
		return this.animateEmoji;
	}

	getAnimateStickers(): number {
		if (MobileLayoutStore.isMobileLayout()) {
			if (!AccessibilityStore.mobileStickerAnimationOverridden) {
				return StickerAnimationOptions.ANIMATE_ON_INTERACTION;
			}
			return AccessibilityStore.mobileStickerAnimationValue;
		}
		return this.animateStickers;
	}

	getRenderSpoilers(): number {
		return this.renderSpoilers;
	}

	getMessageDisplayCompact(): boolean {
		if (MobileLayoutStore.isMobileLayout()) {
			return false;
		}
		return this.messageDisplayCompact;
	}

	getFriendSourceFlags(): number {
		return this.friendSourceFlags;
	}

	getIncomingCallFlags(): number {
		return this.incomingCallFlags;
	}

	getGroupDmAddPermissionFlags(): number {
		return this.groupDmAddPermissionFlags;
	}

	getGuildFolders(): ReadonlyArray<GuildFolder> {
		return this.guildFolders;
	}

	getCustomStatus(): CustomStatus | null {
		return this.customStatus;
	}

	getAfkTimeout(): number {
		return this.afkTimeout;
	}

	getDeveloperMode(): boolean {
		return this.developerMode;
	}

	isHydrated(): boolean {
		return this.hydrated;
	}

	@action
	setStatus(status: StatusType): void {
		this.status = status;
		LocalPresenceStore.updatePresence();
	}

	handleConnectionOpen(userSettings: unknown): void {
		this.updateUserSettings(userSettings);
	}

	updateUserSettings(userSettings: unknown, options: {hydrate?: boolean} = {}): void {
		const {hydrate = true} = options;
		const previousStatus = this.status;
		const previousMessageDisplayCompact = this.messageDisplayCompact;
		const previousCustomStatus = this.customStatus;

		if (userSettings === null || userSettings === undefined) {
			return;
		}

		const camelCaseSettings = convertKeysToCamelCase<UserSettings>(userSettings);
		if (hydrate) {
			this.hydrated = true;
		}

		this.flags = camelCaseSettings.flags;
		const normalizedStatus = normalizeStatus(camelCaseSettings.status);
		this.status = normalizedStatus;
		this.statusResetsAt = camelCaseSettings.statusResetsAt ?? null;
		this.statusResetsTo = camelCaseSettings.statusResetsTo ?? null;
		const nextTheme = normalizeTheme(camelCaseSettings.theme);
		if (nextTheme) {
			this.theme = nextTheme;
			persistTheme(nextTheme);
		}
		this.timeFormat = camelCaseSettings.timeFormat;
		this.guildPositions = [...camelCaseSettings.guildPositions];
		const localeToLoad = camelCaseSettings.locale ?? this.locale;
		const normalizedLocale = loadLocaleCatalog(localeToLoad);
		this.locale = normalizedLocale;
		this.restrictedGuilds = [...camelCaseSettings.restrictedGuilds];
		this.defaultGuildsRestricted = camelCaseSettings.defaultGuildsRestricted;
		this.inlineAttachmentMedia = camelCaseSettings.inlineAttachmentMedia;
		this.inlineEmbedMedia = camelCaseSettings.inlineEmbedMedia;
		this.gifAutoPlay = camelCaseSettings.gifAutoPlay;
		this.renderEmbeds = camelCaseSettings.renderEmbeds;
		this.renderReactions = camelCaseSettings.renderReactions;
		this.animateEmoji = camelCaseSettings.animateEmoji;
		this.animateStickers = camelCaseSettings.animateStickers;
		this.renderSpoilers = camelCaseSettings.renderSpoilers;
		this.messageDisplayCompact = camelCaseSettings.messageDisplayCompact;

		if (camelCaseSettings.messageDisplayCompact !== previousMessageDisplayCompact) {
			const currentDefault = previousMessageDisplayCompact ? 0 : 16;
			const shouldAutoAdjust = AccessibilityStore.messageGroupSpacing === currentDefault;

			if (shouldAutoAdjust) {
				const newDefault = camelCaseSettings.messageDisplayCompact ? 0 : 16;
				AccessibilityStore.updateSettings({messageGroupSpacing: newDefault});
			}
		}

		this.developerMode = camelCaseSettings.developerMode;
		this.friendSourceFlags = camelCaseSettings.friendSourceFlags;
		this.incomingCallFlags = camelCaseSettings.incomingCallFlags;
		this.groupDmAddPermissionFlags = camelCaseSettings.groupDmAddPermissionFlags;
		this.guildFolders = camelCaseSettings.guildFolders.map((folder) => ({
			...folder,
			guildIds: [...folder.guildIds],
		}));
		const newCustomStatus = normalizeCustomStatus(camelCaseSettings.customStatus ?? null);
		this.customStatus = newCustomStatus ? {...newCustomStatus} : null;
		this.afkTimeout = camelCaseSettings.afkTimeout;

		if (normalizedStatus !== previousStatus) {
			const presence = LocalPresenceStore.getPresence();
			if (!presence.afk) {
				LocalPresenceStore.updatePresence();
			}
		}

		if (!isEqual(this.customStatus, previousCustomStatus)) {
			LocalPresenceStore.updatePresence();
		}

		AccessibilityOverrideStore.updateUserSettings(userSettings);
	}

	private get snapshot(): UserSettings {
		return {
			flags: this.flags,
			status: this.status,
			statusResetsAt: this.statusResetsAt,
			statusResetsTo: this.statusResetsTo,
			theme: this.theme,
			timeFormat: this.timeFormat,
			guildPositions: [...this.guildPositions],
			locale: this.locale,
			restrictedGuilds: [...this.restrictedGuilds],
			defaultGuildsRestricted: this.defaultGuildsRestricted,
			inlineAttachmentMedia: this.inlineAttachmentMedia,
			inlineEmbedMedia: this.inlineEmbedMedia,
			gifAutoPlay: this.gifAutoPlay,
			renderEmbeds: this.renderEmbeds,
			renderReactions: this.renderReactions,
			animateEmoji: this.animateEmoji,
			animateStickers: this.animateStickers,
			renderSpoilers: this.renderSpoilers,
			messageDisplayCompact: this.messageDisplayCompact,
			developerMode: this.developerMode,
			friendSourceFlags: this.friendSourceFlags,
			incomingCallFlags: this.incomingCallFlags,
			groupDmAddPermissionFlags: this.groupDmAddPermissionFlags,
			guildFolders: this.guildFolders.map((folder) => ({
				...folder,
				guildIds: [...folder.guildIds],
			})),
			customStatus: this.customStatus ? {...this.customStatus} : null,
			afkTimeout: this.afkTimeout,
		};
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => ({
				inlineAttachmentMedia: this.inlineAttachmentMedia,
				inlineEmbedMedia: this.inlineEmbedMedia,
				gifAutoPlay: this.gifAutoPlay,
				renderEmbeds: this.renderEmbeds,
				renderReactions: this.renderReactions,
				animateEmoji: this.animateEmoji,
				animateStickers: this.animateStickers,
				renderSpoilers: this.renderSpoilers,
				messageDisplayCompact: this.messageDisplayCompact,
			}),
			() => callback(),
			{fireImmediately: true},
		);
	}

	async saveSettings(settings: Partial<UserSettings>): Promise<void> {
		const previousSnapshot = this.snapshot;
		const sanitizedSettings = {...settings};
		if ('customStatus' in sanitizedSettings) {
			sanitizedSettings.customStatus = normalizeCustomStatus(sanitizedSettings.customStatus ?? null);
		}
		const mergedSnapshot = {...previousSnapshot, ...sanitizedSettings};
		const mergedSnakeCase = convertKeysToSnakeCase(mergedSnapshot);
		runInAction(() => {
			this.updateUserSettings(mergedSnakeCase, {hydrate: false});
		});

		try {
			logger.debug('Updating user settings');
			const payload = convertKeysToSnakeCase(sanitizedSettings);
			await http.patch({url: Endpoints.USER_SETTINGS, body: payload});
			logger.debug('Successfully updated user settings');
		} catch (error) {
			logger.error('Failed to update user settings:', error);
			runInAction(() => {
				this.updateUserSettings(convertKeysToSnakeCase(previousSnapshot), {hydrate: false});
			});
			throw error;
		}
	}
}

export default new UserSettingsStore();
