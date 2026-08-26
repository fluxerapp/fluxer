// SPDX-License-Identifier: AGPL-3.0-or-later

import {applyLocaleChange} from '@app/app/I18n';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {migrateLegacyMessageGroupSpacing} from '@app/features/accessibility/state/MessageGroupSpacing';
import {
	createMotionPreferencesContext,
	type MotionPreferencesInput,
	selectEffectiveAnimateEmoji,
	selectEffectiveAnimateStickers,
	selectEffectiveGifAutoPlay,
} from '@app/features/accessibility/state/MotionPreferencesMachine';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import LocalPresence, {setLocalPresenceUserSettings} from '@app/features/presence/state/LocalPresence';
import Theme from '@app/features/theme/state/Theme';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {
	type CustomStatus,
	normalizeCustomStatus,
	toApiCustomStatusPayload,
} from '@app/features/user/state/CustomStatus';
import {
	changedSyncedPreferenceFields,
	createEmptySyncedPreferences,
	decodeSyncedPreferencesLenient,
	encodeSyncedPreferences,
	isSyncedPreferencesField,
	mergeIncomingSyncedPreferences as mergeIncomingSyncedPreferencesWithEngine,
	preferencesFromBytes,
	preferencesToBytes,
	SYNCED_PREFERENCES_FIELDS,
	type SyncedPreferences,
	type SyncedPreferencesField,
	syncedPreferencesEqual,
} from '@app/features/user/state/SyncedPreferencesEngine';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {normalizeStatus, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	type GuildFolderIcon,
	type ProfilePrivacyLevel,
	ProfilePrivacyLevels,
	RenderSpoilers,
	SensitiveMediaFilterLevel,
	StickerAnimationOptions,
	ThemeTypes,
	TimeFormatTypes,
} from '@fluxer/constants/src/UserConstants';
import camelCase from 'lodash/camelCase';
import isEqual from 'lodash/isEqual';
import isPlainObject from 'lodash/isPlainObject';
import snakeCase from 'lodash/snakeCase';
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';

function restoreSettingValue<K extends keyof UserSettings>(target: UserSettings, source: UserSettings, key: K): void {
	target[key] = source[key];
}

export interface GuildFolder {
	id: number | null;
	name: string | null;
	color: number | null;
	flags: number;
	icon: GuildFolderIcon;
	guildIds: Array<string>;
}

export interface UserSettings {
	flags: number;
	status: string;
	statusResetsAt: string | null;
	statusResetsTo: string | null;
	theme: string;
	timeFormat: number;
	locale: string;
	restrictedGuilds: Array<string>;
	botRestrictedGuilds: Array<string>;
	defaultGuildsRestricted: boolean;
	botDefaultGuildsRestricted: boolean;
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
	profilePrivacy: ProfilePrivacyLevel;
	defaultShareVoiceActivity: boolean;
	guildFolders: Array<GuildFolder>;
	customStatus: CustomStatus | null;
	afkTimeout: number;
	trustedDomains: Array<string>;
	defaultHideMutedChannels: boolean;
	sensitiveContentFriendDmFilter: number;
	sensitiveContentNonFriendDmFilter: number;
	sensitiveContentGuildFilter: number;
	suppressUnprivilegedSelfMentions: boolean;
	suppressUnprivilegedSelfMentionsBypassUserIds: Array<string>;
	staffDmAccessUserIds: Array<string>;
	syncedPreferences: SyncedPreferences;
}

const logger = new Logger('UserSettings');
const SYNCED_PREFERENCES_DIRTY_FIELDS_KEY = 'UserSettings:syncedPreferencesDirtyFields';
const SYNCED_PREFERENCES_LOCAL_KEY = 'UserSettings:syncedPreferencesLocal';
const SYNCED_PREFERENCES_WIRE_KEY = 'UserSettings:syncedPreferencesWire';
const SYNCED_PREFERENCES_RECENT_ACK_KEY = 'UserSettings:syncedPreferencesRecentAck';
const RECENT_SYNCED_PREFERENCES_ACK_WINDOW_MS = 60_000;

function readDirtySyncedPreferenceFields(): Set<SyncedPreferencesField> {
	try {
		const raw = AppStorage.getJSON<unknown>(SYNCED_PREFERENCES_DIRTY_FIELDS_KEY, []);
		if (!Array.isArray(raw)) return new Set();
		return new Set(raw.filter(isSyncedPreferencesField));
	} catch (error) {
		logger.warn('Failed to read dirty synced preference fields:', error);
		return new Set();
	}
}

function writeDirtySyncedPreferenceFields(fields: ReadonlySet<SyncedPreferencesField>): void {
	try {
		if (fields.size === 0) {
			AppStorage.removeItem(SYNCED_PREFERENCES_DIRTY_FIELDS_KEY);
			return;
		}
		AppStorage.setJSON(SYNCED_PREFERENCES_DIRTY_FIELDS_KEY, Array.from(fields).map(String).sort());
	} catch (error) {
		logger.warn('Failed to persist dirty synced preference fields:', error);
	}
}

function readPersistedSyncedPreferences(key: string): SyncedPreferences {
	try {
		const raw = AppStorage.getItem(key);
		if (raw == null || raw === '') return createEmptySyncedPreferences();
		return decodeSyncedPreferencesLenient(raw);
	} catch (error) {
		logger.warn(`Failed to read persisted synced preferences (${key}):`, error);
		return createEmptySyncedPreferences();
	}
}

function writePersistedSyncedPreferences(key: string, preferences: SyncedPreferences): void {
	try {
		const encoded = encodeSyncedPreferences(preferences);
		if (encoded === '') {
			AppStorage.removeItem(key);
		} else {
			AppStorage.setItem(key, encoded);
		}
	} catch (error) {
		logger.warn(`Failed to persist synced preferences (${key}):`, error);
	}
}

function readPersistedRecentAck(): Map<SyncedPreferencesField, number> {
	try {
		const raw = AppStorage.getJSON<unknown>(SYNCED_PREFERENCES_RECENT_ACK_KEY, {});
		if (raw == null || typeof raw !== 'object' || Array.isArray(raw)) return new Map();
		const now = Date.now();
		const map = new Map<SyncedPreferencesField, number>();
		for (const [key, value] of Object.entries(raw as Record<string, unknown>)) {
			if (typeof value === 'number' && Number.isFinite(value) && value > now && isSyncedPreferencesField(key)) {
				map.set(key, value);
			}
		}
		return map;
	} catch (error) {
		logger.warn('Failed to read persisted recent-ack window:', error);
		return new Map();
	}
}

function writePersistedRecentAck(entries: ReadonlyMap<SyncedPreferencesField, number>): void {
	try {
		if (entries.size === 0) {
			AppStorage.removeItem(SYNCED_PREFERENCES_RECENT_ACK_KEY);
			return;
		}
		const obj: Record<string, number> = {};
		for (const [key, value] of entries) obj[String(key)] = value;
		AppStorage.setJSON(SYNCED_PREFERENCES_RECENT_ACK_KEY, obj);
	} catch (error) {
		logger.warn('Failed to persist recent-ack window:', error);
	}
}

function cloneSyncedPreferences(preferences: SyncedPreferences): SyncedPreferences {
	return preferencesFromBytes(preferencesToBytes(preferences));
}

function normalizeLegacyMessageGroupSpacingPreference(
	preferences: SyncedPreferences,
	messageDisplayCompact: boolean,
): {preferences: SyncedPreferences; migrated: boolean} {
	const accessibility = preferences.accessibility;
	if (accessibility?.messageGroupSpacing === undefined || accessibility.compactMessageGroupSpacing !== undefined) {
		return {preferences, migrated: false};
	}
	const normalized = cloneSyncedPreferences(preferences);
	const normalizedAccessibility = normalized.accessibility;
	if (normalizedAccessibility === undefined) {
		return {preferences, migrated: false};
	}
	const spacing = migrateLegacyMessageGroupSpacing(accessibility.messageGroupSpacing, messageDisplayCompact);
	normalizedAccessibility.messageGroupSpacing = spacing.messageGroupSpacing;
	normalizedAccessibility.compactMessageGroupSpacing = spacing.compactMessageGroupSpacing;
	return {preferences: normalized, migrated: true};
}

function convertKeysToCamelCaseInternal(value: unknown): unknown {
	if (Array.isArray(value)) {
		return value.map(convertKeysToCamelCaseInternal);
	}
	if (isPlainObject(value)) {
		const record = value as Record<string, unknown>;
		return Object.fromEntries(
			Object.entries(record).map(([key, v]) => [camelCase(key), convertKeysToCamelCaseInternal(v)]),
		);
	}
	return value;
}

function convertKeysToCamelCase<T>(obj: unknown): T {
	return convertKeysToCamelCaseInternal(obj) as T;
}

function convertKeysToSnakeCaseInternal(value: unknown): unknown {
	if (Array.isArray(value)) {
		return value.map(convertKeysToSnakeCaseInternal);
	}
	if (isPlainObject(value)) {
		const record = value as Record<string, unknown>;
		return Object.fromEntries(
			Object.entries(record).map(([key, v]) => [snakeCase(key), convertKeysToSnakeCaseInternal(v)]),
		);
	}
	return value;
}

function convertKeysToSnakeCase<T>(obj: unknown): T {
	return convertKeysToSnakeCaseInternal(obj) as T;
}

class UserSettingsState {
	flags: number = 0;
	status: StatusType = StatusTypes.ONLINE;
	statusResetsAt: string | null = null;
	statusResetsTo: string | null = null;
	theme: string = ThemeTypes.SYSTEM;
	timeFormat: number = TimeFormatTypes.AUTO;
	locale: string = 'en-US';
	restrictedGuilds: Array<string> = [];
	botRestrictedGuilds: Array<string> = [];
	defaultGuildsRestricted: boolean = false;
	botDefaultGuildsRestricted: boolean = false;
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
	profilePrivacy: ProfilePrivacyLevel = ProfilePrivacyLevels.ALL_GUILDS;
	defaultShareVoiceActivity: boolean = true;
	guildFolders: Array<GuildFolder> = [];
	customStatus: CustomStatus | null = null;
	afkTimeout: number = 600;
	trustedDomains: Array<string> = [];
	defaultHideMutedChannels: boolean = false;
	sensitiveContentFriendDmFilter: number = SensitiveMediaFilterLevel.SHOW;
	sensitiveContentNonFriendDmFilter: number = SensitiveMediaFilterLevel.SHOW;
	sensitiveContentGuildFilter: number = SensitiveMediaFilterLevel.SHOW;
	suppressUnprivilegedSelfMentions: boolean = false;
	suppressUnprivilegedSelfMentionsBypassUserIds: Array<string> = [];
	staffDmAccessUserIds: Array<string> = [];
	syncedPreferences: SyncedPreferences = readPersistedSyncedPreferences(SYNCED_PREFERENCES_LOCAL_KEY);
	private hydrated = false;
	private wireSyncedPreferences: SyncedPreferences = readPersistedSyncedPreferences(SYNCED_PREFERENCES_WIRE_KEY);
	private syncFlushTimer: NodeJS.Timeout | 'microtask' | null = null;
	private syncFlushInFlight = false;
	private syncFlushPendingPromise: Promise<void> | null = null;
	private syncFlushPendingResolvers: Array<{
		resolve: () => void;
		reject: (error: unknown) => void;
	}> = [];
	private syncConsecutive429s = 0;
	private dirtySyncedPreferenceFields: Set<SyncedPreferencesField> = readDirtySyncedPreferenceFields();
	private inFlightSyncedPreferenceFields: Set<SyncedPreferencesField> = new Set();
	private inFlightSyncedPreferences: SyncedPreferences | null = null;
	private recentlyAckedSyncedPreferenceFields: Map<SyncedPreferencesField, number> = readPersistedRecentAck();
	private accountEpoch = 0;

	constructor() {
		makeAutoObservable<
			UserSettings,
			| 'wireSyncedPreferences'
			| 'syncFlushTimer'
			| 'syncFlushInFlight'
			| 'syncFlushPendingPromise'
			| 'syncFlushPendingResolvers'
			| 'syncConsecutive429s'
			| 'dirtySyncedPreferenceFields'
			| 'inFlightSyncedPreferenceFields'
			| 'inFlightSyncedPreferences'
			| 'recentlyAckedSyncedPreferenceFields'
			| 'accountEpoch'
		>(
			this,
			{
				wireSyncedPreferences: false,
				syncFlushTimer: false,
				syncFlushInFlight: false,
				syncFlushPendingPromise: false,
				syncFlushPendingResolvers: false,
				syncConsecutive429s: false,
				dirtySyncedPreferenceFields: false,
				inFlightSyncedPreferenceFields: false,
				inFlightSyncedPreferences: false,
				recentlyAckedSyncedPreferenceFields: false,
				accountEpoch: false,
			},
			{autoBind: true},
		);
		this.installSyncedPreferencesTabSync();
	}

	private installSyncedPreferencesTabSync(): void {
		try {
			AppStorage.subscribe(
				() => {
					try {
						const incoming = readPersistedSyncedPreferences(SYNCED_PREFERENCES_LOCAL_KEY);
						runInAction(() => this.mergeIncomingSyncedPreferences(incoming));
					} catch (error) {
						logger.warn('Failed to apply external synced preferences change:', error);
					}
				},
				{key: SYNCED_PREFERENCES_LOCAL_KEY, source: 'external'},
			);
		} catch (error) {
			logger.warn('Failed to install synced preferences tab sync:', error);
		}
	}

	private persistLocalSyncedPreferences(): void {
		writePersistedSyncedPreferences(SYNCED_PREFERENCES_LOCAL_KEY, this.syncedPreferences);
	}

	private persistWireSyncedPreferences(): void {
		writePersistedSyncedPreferences(SYNCED_PREFERENCES_WIRE_KEY, this.wireSyncedPreferences);
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

	getTimeFormat(): number {
		return this.timeFormat;
	}

	getGuildPositions(): ReadonlyArray<string> {
		return this.guildFolders.flatMap((folder) => folder.guildIds);
	}

	getLocale(): string {
		return this.locale;
	}

	applyLocalLocale(locale: string): void {
		this.locale = applyLocaleChange(locale);
	}

	getRestrictedGuilds(): ReadonlyArray<string> {
		return this.restrictedGuilds;
	}

	getBotRestrictedGuilds(): ReadonlyArray<string> {
		return this.botRestrictedGuilds;
	}

	getBotDefaultGuildsRestricted(): boolean {
		return this.botDefaultGuildsRestricted;
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

	getMotionPreferencesInput(): MotionPreferencesInput {
		return {
			syncWithSystem: Accessibility.syncReducedMotionWithSystem,
			manualReducedMotion: Accessibility.reducedMotionOverride ?? false,
			systemReducedMotion: Accessibility.systemReducedMotion,
			enableSmoothScrolling: Accessibility.enableSmoothScrolling,
			isMobile: MobileLayout.isMobileLayout(),
			animateEmoji: this.animateEmoji,
			gifAutoPlay: this.gifAutoPlay,
			animateStickers: this.animateStickers,
			mobileAnimateEmojiOverridden: Accessibility.mobileAnimateEmojiOverridden,
			mobileAnimateEmojiValue: Accessibility.mobileAnimateEmojiValue,
			mobileGifAutoPlayOverridden: Accessibility.mobileGifAutoPlayOverridden,
			mobileGifAutoPlayValue: Accessibility.mobileGifAutoPlayValue,
			mobileStickerAnimationOverridden: Accessibility.mobileStickerAnimationOverridden,
			mobileStickerAnimationValue: Accessibility.mobileStickerAnimationValue,
			keepAnimatedEmojiUnderReducedMotion: Accessibility.keepAnimatedEmojiUnderReducedMotion,
			keepGifAutoPlayUnderReducedMotion: Accessibility.keepGifAutoPlayUnderReducedMotion,
			keepStickerAnimationUnderReducedMotion: Accessibility.keepStickerAnimationUnderReducedMotion,
		};
	}

	getGifAutoPlay(): boolean {
		return selectEffectiveGifAutoPlay(createMotionPreferencesContext(this.getMotionPreferencesInput()));
	}

	getRenderEmbeds(): boolean {
		return this.renderEmbeds;
	}

	getRenderReactions(): boolean {
		return this.renderReactions;
	}

	getAnimateEmoji(): boolean {
		return selectEffectiveAnimateEmoji(createMotionPreferencesContext(this.getMotionPreferencesInput()));
	}

	getAnimateStickers(): number {
		return selectEffectiveAnimateStickers(createMotionPreferencesContext(this.getMotionPreferencesInput()));
	}

	getRenderSpoilers(): number {
		return this.renderSpoilers;
	}

	getMessageDisplayCompact(): boolean {
		if (MobileLayout.isMobileLayout()) {
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

	getProfilePrivacy(): ProfilePrivacyLevel {
		return this.profilePrivacy;
	}

	getDefaultShareVoiceActivity(): boolean {
		return this.defaultShareVoiceActivity;
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

	getTrustedDomains(): ReadonlyArray<string> {
		return this.trustedDomains;
	}

	trustAllDomains(): boolean {
		return this.trustedDomains.includes('*');
	}

	getDefaultHideMutedChannels(): boolean {
		return this.defaultHideMutedChannels;
	}

	getSensitiveContentFriendDmFilter(): number {
		return this.sensitiveContentFriendDmFilter;
	}

	getSensitiveContentNonFriendDmFilter(): number {
		return this.sensitiveContentNonFriendDmFilter;
	}

	getSensitiveContentGuildFilter(): number {
		return this.sensitiveContentGuildFilter;
	}

	getSuppressUnprivilegedSelfMentions(): boolean {
		return this.suppressUnprivilegedSelfMentions;
	}

	getSuppressUnprivilegedSelfMentionsBypassUserIds(): ReadonlyArray<string> {
		return this.suppressUnprivilegedSelfMentionsBypassUserIds;
	}

	getSuppressUnprivilegedSelfMentionBypassUserIds(): ReadonlyArray<string> {
		return this.getSuppressUnprivilegedSelfMentionsBypassUserIds();
	}

	getStaffDmAccessUserIds(): ReadonlyArray<string> {
		return this.staffDmAccessUserIds;
	}

	isHydrated(): boolean {
		return this.hydrated;
	}

	@action
	markSessionChanging(): void {
		this.hydrated = false;
	}

	@action
	handleAccountTransition(): void {
		this.accountEpoch += 1;
		this.hydrated = false;
		this.syncedPreferences = createEmptySyncedPreferences();
		this.wireSyncedPreferences = createEmptySyncedPreferences();
		this.dirtySyncedPreferenceFields.clear();
		writeDirtySyncedPreferenceFields(this.dirtySyncedPreferenceFields);
		this.inFlightSyncedPreferenceFields.clear();
		this.inFlightSyncedPreferences = null;
		this.recentlyAckedSyncedPreferenceFields.clear();
		writePersistedRecentAck(this.recentlyAckedSyncedPreferenceFields);
		AppStorage.removeItem(SYNCED_PREFERENCES_LOCAL_KEY);
		AppStorage.removeItem(SYNCED_PREFERENCES_WIRE_KEY);
		if (this.syncFlushTimer != null && this.syncFlushTimer !== 'microtask') {
			clearTimeout(this.syncFlushTimer);
		}
		this.syncFlushTimer = null;
		this.syncConsecutive429s = 0;
		const resolvers = this.syncFlushPendingResolvers;
		this.syncFlushPendingResolvers = [];
		this.syncFlushPendingPromise = null;
		for (const resolver of resolvers) {
			resolver.resolve();
		}
	}

	@action
	setStatus(status: StatusType): void {
		this.status = status;
		LocalPresence.updatePresence();
	}

	handleGatewayReady(userSettings: unknown): void {
		this.updateUserSettings(userSettings);
	}

	updateUserSettings(
		userSettings: unknown,
		options: {
			hydrate?: boolean;
		} = {},
	): void {
		const {hydrate = true} = options;
		const previousStatus = this.status;
		const previousCustomStatus = this.customStatus;
		const wasHydrated = this.hydrated;
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
		if (camelCaseSettings.theme) {
			this.theme = camelCaseSettings.theme;
			Theme.updateServerTheme(camelCaseSettings.theme);
		}
		this.timeFormat = camelCaseSettings.timeFormat;
		const localeToLoad = camelCaseSettings.locale ?? this.locale;
		const normalizedLocale = applyLocaleChange(localeToLoad);
		this.locale = normalizedLocale;
		this.restrictedGuilds = [...camelCaseSettings.restrictedGuilds];
		this.botRestrictedGuilds = [...camelCaseSettings.botRestrictedGuilds];
		this.defaultGuildsRestricted = camelCaseSettings.defaultGuildsRestricted;
		this.botDefaultGuildsRestricted = camelCaseSettings.botDefaultGuildsRestricted;
		this.inlineAttachmentMedia = camelCaseSettings.inlineAttachmentMedia;
		this.inlineEmbedMedia = camelCaseSettings.inlineEmbedMedia;
		this.gifAutoPlay = camelCaseSettings.gifAutoPlay;
		this.renderEmbeds = camelCaseSettings.renderEmbeds;
		this.renderReactions = camelCaseSettings.renderReactions;
		this.animateEmoji = camelCaseSettings.animateEmoji;
		this.animateStickers = camelCaseSettings.animateStickers;
		this.renderSpoilers = camelCaseSettings.renderSpoilers;
		this.messageDisplayCompact = camelCaseSettings.messageDisplayCompact;
		this.developerMode = camelCaseSettings.developerMode;
		AppStorage.setItem('debugLoggingEnabled', this.developerMode.toString());
		Logger.refreshGlobalLogLevel();
		this.friendSourceFlags = camelCaseSettings.friendSourceFlags;
		this.incomingCallFlags = camelCaseSettings.incomingCallFlags;
		this.groupDmAddPermissionFlags = camelCaseSettings.groupDmAddPermissionFlags;
		if (camelCaseSettings.profilePrivacy !== undefined) {
			this.profilePrivacy = camelCaseSettings.profilePrivacy;
		}
		if (camelCaseSettings.defaultShareVoiceActivity !== undefined) {
			this.defaultShareVoiceActivity = camelCaseSettings.defaultShareVoiceActivity;
		}
		this.guildFolders = camelCaseSettings.guildFolders.map((folder) => ({
			...folder,
			flags: folder.flags ?? 0,
			icon: folder.icon ?? DEFAULT_GUILD_FOLDER_ICON,
			guildIds: [...folder.guildIds],
		}));
		const newCustomStatus = normalizeCustomStatus(camelCaseSettings.customStatus ?? null);
		this.customStatus = newCustomStatus ? {...newCustomStatus} : null;
		this.afkTimeout = camelCaseSettings.afkTimeout;
		if (camelCaseSettings.trustedDomains !== undefined) {
			this.trustedDomains = [...camelCaseSettings.trustedDomains];
		}
		if (camelCaseSettings.defaultHideMutedChannels !== undefined) {
			this.defaultHideMutedChannels = camelCaseSettings.defaultHideMutedChannels;
		}
		if (camelCaseSettings.sensitiveContentFriendDmFilter !== undefined) {
			this.sensitiveContentFriendDmFilter = camelCaseSettings.sensitiveContentFriendDmFilter;
		}
		if (camelCaseSettings.sensitiveContentNonFriendDmFilter !== undefined) {
			this.sensitiveContentNonFriendDmFilter = camelCaseSettings.sensitiveContentNonFriendDmFilter;
		}
		if (camelCaseSettings.sensitiveContentGuildFilter !== undefined) {
			this.sensitiveContentGuildFilter = camelCaseSettings.sensitiveContentGuildFilter;
		}
		if (camelCaseSettings.suppressUnprivilegedSelfMentions !== undefined) {
			this.suppressUnprivilegedSelfMentions = camelCaseSettings.suppressUnprivilegedSelfMentions;
		}
		if (camelCaseSettings.suppressUnprivilegedSelfMentionsBypassUserIds !== undefined) {
			this.suppressUnprivilegedSelfMentionsBypassUserIds = [
				...(camelCaseSettings.suppressUnprivilegedSelfMentionsBypassUserIds ?? []),
			];
		}
		if (camelCaseSettings.staffDmAccessUserIds !== undefined) {
			this.staffDmAccessUserIds = [...(camelCaseSettings.staffDmAccessUserIds ?? [])];
		}
		const incomingSyncedPreferences = (
			camelCaseSettings as {
				syncedPreferences?: unknown;
			}
		).syncedPreferences;
		if (
			incomingSyncedPreferences !== undefined &&
			(typeof incomingSyncedPreferences === 'string' || incomingSyncedPreferences === null)
		) {
			this.mergeIncomingSyncedPreferences(decodeSyncedPreferencesLenient(incomingSyncedPreferences));
		}
		if (normalizedStatus !== previousStatus) {
			const presence = LocalPresence.getPresence();
			if (!presence.afk) {
				LocalPresence.updatePresence();
			}
		}
		if (!isEqual(this.customStatus, previousCustomStatus)) {
			LocalPresence.updatePresence();
		}
		if (hydrate && !wasHydrated) {
			LocalPresence.updatePresence();
		}
		if (hydrate && !wasHydrated && this.dirtySyncedPreferenceFields.size > 0) {
			void this.scheduleSyncedPreferencesFlush();
		}
	}

	private get snapshot(): UserSettings {
		return {
			flags: this.flags,
			status: this.status,
			statusResetsAt: this.statusResetsAt,
			statusResetsTo: this.statusResetsTo,
			theme: this.theme,
			timeFormat: this.timeFormat,
			locale: this.locale,
			restrictedGuilds: [...this.restrictedGuilds],
			botRestrictedGuilds: [...this.botRestrictedGuilds],
			defaultGuildsRestricted: this.defaultGuildsRestricted,
			botDefaultGuildsRestricted: this.botDefaultGuildsRestricted,
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
			profilePrivacy: this.profilePrivacy,
			defaultShareVoiceActivity: this.defaultShareVoiceActivity,
			guildFolders: this.guildFolders.map((folder) => ({
				...folder,
				guildIds: [...folder.guildIds],
			})),
			customStatus: this.customStatus ? {...this.customStatus} : null,
			afkTimeout: this.afkTimeout,
			trustedDomains: [...this.trustedDomains],
			defaultHideMutedChannels: this.defaultHideMutedChannels,
			sensitiveContentFriendDmFilter: this.sensitiveContentFriendDmFilter,
			sensitiveContentNonFriendDmFilter: this.sensitiveContentNonFriendDmFilter,
			sensitiveContentGuildFilter: this.sensitiveContentGuildFilter,
			suppressUnprivilegedSelfMentions: this.suppressUnprivilegedSelfMentions,
			suppressUnprivilegedSelfMentionsBypassUserIds: [...this.suppressUnprivilegedSelfMentionsBypassUserIds],
			staffDmAccessUserIds: [...this.staffDmAccessUserIds],
			syncedPreferences: cloneSyncedPreferences(this.syncedPreferences),
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

	getSubPreference<F extends SyncedPreferencesField>(field: F): SyncedPreferences[F] | undefined {
		const value = this.syncedPreferences[field];
		return value === undefined ? undefined : value;
	}

	async setSubPreference<F extends SyncedPreferencesField>(
		field: F,
		value: NonNullable<SyncedPreferences[F]>,
	): Promise<void> {
		runInAction(() => {
			this.markSyncedPreferenceFieldDirty(field);
			(this.syncedPreferences as SyncedPreferences)[field] = value;
		});
		this.persistLocalSyncedPreferences();
		return this.scheduleSyncedPreferencesFlush();
	}

	getSanitizeUrls(): boolean {
		return this.syncedPreferences.sanitizeUrls;
	}

	async setSanitizeUrls(value: boolean): Promise<void> {
		runInAction(() => {
			this.markSyncedPreferenceFieldDirty('sanitizeUrls');
			this.syncedPreferences.sanitizeUrls = value;
		});
		this.persistLocalSyncedPreferences();
		return this.scheduleSyncedPreferencesFlush();
	}

	private markSyncedPreferenceFieldDirty(field: SyncedPreferencesField): void {
		this.dirtySyncedPreferenceFields.add(field);
		writeDirtySyncedPreferenceFields(this.dirtySyncedPreferenceFields);
	}

	private clearSyncedPreferenceFieldsDirty(fields: Iterable<SyncedPreferencesField>): void {
		let changed = false;
		for (const field of fields) {
			changed = this.dirtySyncedPreferenceFields.delete(field) || changed;
		}
		if (changed) {
			writeDirtySyncedPreferenceFields(this.dirtySyncedPreferenceFields);
		}
	}

	private markSyncedPreferenceFieldsAcked(fields: Iterable<SyncedPreferencesField>): void {
		const expiresAt = Date.now() + RECENT_SYNCED_PREFERENCES_ACK_WINDOW_MS;
		let changed = false;
		for (const field of fields) {
			if (this.dirtySyncedPreferenceFields.has(field)) continue;
			this.recentlyAckedSyncedPreferenceFields.set(field, expiresAt);
			changed = true;
		}
		if (changed) {
			writePersistedRecentAck(this.recentlyAckedSyncedPreferenceFields);
		}
	}

	private isRecentlyAckedSyncedPreferenceField(field: SyncedPreferencesField): boolean {
		const expiresAt = this.recentlyAckedSyncedPreferenceFields.get(field);
		if (expiresAt === undefined) return false;
		if (expiresAt > Date.now()) return true;
		this.recentlyAckedSyncedPreferenceFields.delete(field);
		writePersistedRecentAck(this.recentlyAckedSyncedPreferenceFields);
		return false;
	}

	private mergeIncomingSyncedPreferences(incoming: SyncedPreferences): void {
		const {preferences: normalizedIncoming, migrated: migratedLegacyMessageGroupSpacing} =
			normalizeLegacyMessageGroupSpacingPreference(incoming, this.messageDisplayCompact);
		const protectedFields = new Set<SyncedPreferencesField>([
			...this.dirtySyncedPreferenceFields,
			...this.inFlightSyncedPreferenceFields,
		]);
		const shouldSyncMigratedMessageGroupSpacing =
			migratedLegacyMessageGroupSpacing && !protectedFields.has('accessibility');
		const recentlyAckedFields = SYNCED_PREFERENCES_FIELDS.filter((field) =>
			this.isRecentlyAckedSyncedPreferenceField(field),
		);
		const {
			merged,
			wire: nextWire,
			dirtyFields,
		} = mergeIncomingSyncedPreferencesWithEngine({
			local: this.syncedPreferences,
			wire: this.wireSyncedPreferences,
			incoming: normalizedIncoming,
			protectedFields,
			recentlyAckedFields,
			inFlight: this.inFlightSyncedPreferences,
			syncInFlight: this.syncFlushInFlight,
		});
		for (const field of dirtyFields) {
			this.markSyncedPreferenceFieldDirty(field);
		}
		const wireChanged = !syncedPreferencesEqual(nextWire, this.wireSyncedPreferences);
		const localChanged = !syncedPreferencesEqual(merged, this.syncedPreferences);
		if (!wireChanged && !localChanged) {
			if (shouldSyncMigratedMessageGroupSpacing) {
				this.markSyncedPreferenceFieldDirty('accessibility');
			}
			return;
		}
		this.wireSyncedPreferences = nextWire;
		this.persistWireSyncedPreferences();
		if (localChanged) {
			this.syncedPreferences = merged;
			this.persistLocalSyncedPreferences();
		}
		if (shouldSyncMigratedMessageGroupSpacing) {
			this.markSyncedPreferenceFieldDirty('accessibility');
		}
	}

	private decodeSyncedPreferencesFromPatchResponse(
		responseBody: unknown,
		fallback: SyncedPreferences,
	): SyncedPreferences {
		if (responseBody == null || typeof responseBody !== 'object') {
			return cloneSyncedPreferences(fallback);
		}
		const candidate =
			(
				responseBody as {
					synced_preferences?: unknown;
				}
			).synced_preferences ??
			(
				responseBody as {
					syncedPreferences?: unknown;
				}
			).syncedPreferences;
		if (typeof candidate === 'string' || candidate === null) {
			return decodeSyncedPreferencesLenient(candidate);
		}
		return cloneSyncedPreferences(fallback);
	}

	private scheduleSyncedPreferencesFlush(): Promise<void> {
		if (this.syncFlushPendingPromise == null) {
			this.syncFlushPendingPromise = new Promise<void>((resolve, reject) => {
				this.syncFlushPendingResolvers.push({resolve, reject});
			});
		}
		if (this.syncFlushTimer != null || this.syncFlushInFlight) return this.syncFlushPendingPromise;
		if (!this.hydrated) return this.syncFlushPendingPromise;
		this.syncFlushTimer = 'microtask';
		queueMicrotask(() => {
			if (this.syncFlushTimer !== 'microtask') return;
			this.syncFlushTimer = null;
			void this.runSyncedPreferencesFlush();
		});
		return this.syncFlushPendingPromise;
	}

	private async runSyncedPreferencesFlush(): Promise<void> {
		if (this.syncFlushInFlight) return;
		const resolvers = this.syncFlushPendingResolvers;
		this.syncFlushPendingResolvers = [];
		const pendingPromise = this.syncFlushPendingPromise;
		this.syncFlushPendingPromise = null;
		const completeAll = (error?: unknown): void => {
			if (error !== undefined) {
				for (const r of resolvers) r.reject(error);
			} else {
				for (const r of resolvers) r.resolve();
			}
		};
		const changedFields = changedSyncedPreferenceFields(this.syncedPreferences, this.wireSyncedPreferences);
		if (changedFields.length === 0) {
			this.clearSyncedPreferenceFieldsDirty(SYNCED_PREFERENCES_FIELDS);
			completeAll();
			return;
		}
		const changedFieldSet = new Set(changedFields);
		const dirtyChangedFields = Array.from(this.dirtySyncedPreferenceFields).filter((field) =>
			changedFieldSet.has(field),
		);
		const fieldsInRequest = new Set<SyncedPreferencesField>(
			dirtyChangedFields.length > 0 ? dirtyChangedFields : changedFields,
		);
		this.syncFlushInFlight = true;
		const epochAtFlush = this.accountEpoch;
		const snapshotAtFlush = cloneSyncedPreferences(this.syncedPreferences);
		this.inFlightSyncedPreferenceFields = new Set(fieldsInRequest);
		this.inFlightSyncedPreferences = cloneSyncedPreferences(snapshotAtFlush);
		const encoded = encodeSyncedPreferences(snapshotAtFlush);
		try {
			const response = await http.patch(Endpoints.USER_SETTINGS, {
				body: {synced_preferences: encoded === '' ? null : encoded},
			});
			if (this.accountEpoch !== epochAtFlush) {
				this.syncFlushInFlight = false;
				completeAll();
				return;
			}
			runInAction(() => {
				this.syncFlushInFlight = false;
				const stillChangedFields = new Set(changedSyncedPreferenceFields(snapshotAtFlush, this.syncedPreferences));
				const ackedFields: Array<SyncedPreferencesField> = [];
				for (const field of fieldsInRequest) {
					if (!stillChangedFields.has(field)) {
						this.dirtySyncedPreferenceFields.delete(field);
						ackedFields.push(field);
					}
				}
				writeDirtySyncedPreferenceFields(this.dirtySyncedPreferenceFields);
				this.inFlightSyncedPreferenceFields.clear();
				this.inFlightSyncedPreferences = null;
				const confirmed = this.decodeSyncedPreferencesFromPatchResponse(
					(
						response as
							| {
									body?: unknown;
							  }
							| undefined
					)?.body,
					snapshotAtFlush,
				);
				this.mergeIncomingSyncedPreferences(confirmed);
				this.markSyncedPreferenceFieldsAcked(ackedFields);
			});
			this.syncConsecutive429s = 0;
			completeAll();
			if (
				this.dirtySyncedPreferenceFields.size > 0 ||
				changedSyncedPreferenceFields(this.syncedPreferences, this.wireSyncedPreferences).length > 0
			) {
				void this.scheduleSyncedPreferencesFlush();
			}
		} catch (error) {
			this.syncFlushInFlight = false;
			this.inFlightSyncedPreferenceFields.clear();
			this.inFlightSyncedPreferences = null;
			if (this.accountEpoch !== epochAtFlush) {
				completeAll();
				return;
			}
			if (this.isRateLimitError(error)) {
				this.syncConsecutive429s += 1;
				const retryAfterMs = this.extractRetryAfterMs(error) ?? this.syncBackoffMs();
				logger.warn(
					`synced_preferences PATCH rate-limited; retry in ${Math.round(retryAfterMs / 1000)}s ` +
						`(attempt ${this.syncConsecutive429s})`,
				);
				if (this.syncFlushPendingPromise == null) {
					this.syncFlushPendingPromise = pendingPromise ?? new Promise<void>(() => undefined);
				}
				this.syncFlushPendingResolvers.unshift(...resolvers);
				this.syncFlushTimer = setTimeout(() => {
					this.syncFlushTimer = null;
					void this.runSyncedPreferencesFlush();
				}, retryAfterMs);
				return;
			}
			logger.error('Failed to save synced preferences:', error);
			completeAll(error);
		}
	}

	private syncBackoffMs(): number {
		const exp = Math.min(this.syncConsecutive429s, 6);
		return Math.min(60000, 1000 * 2 ** exp);
	}

	private isRateLimitError(error: unknown): boolean {
		if (error == null || typeof error !== 'object') return false;
		const status = (
			error as {
				status?: unknown;
			}
		).status;
		return status === 429;
	}

	private extractRetryAfterMs(error: unknown): number | null {
		if (error == null || typeof error !== 'object') return null;
		const candidate =
			(
				error as {
					retryAfter?: unknown;
				}
			).retryAfter ??
			(
				error as {
					body?: {
						retry_after?: unknown;
					};
				}
			).body?.retry_after;
		if (typeof candidate !== 'number' || !Number.isFinite(candidate) || candidate < 0) return null;
		return Math.min(60000, Math.max(250, candidate * 1000));
	}

	async saveSettings(settings: Partial<UserSettings>): Promise<void> {
		const previousSnapshot = this.snapshot;
		const sanitizedSettings = {...settings};
		if ('customStatus' in sanitizedSettings) {
			sanitizedSettings.customStatus = normalizeCustomStatus(sanitizedSettings.customStatus ?? null);
		}
		const mergedSnapshot = {...previousSnapshot, ...sanitizedSettings};
		const includesSyncedPreferences = 'syncedPreferences' in sanitizedSettings;
		const mergedSnakeCase = convertKeysToSnakeCase<Record<string, unknown>>(mergedSnapshot);
		if (!includesSyncedPreferences) {
			delete mergedSnakeCase.synced_preferences;
		}
		runInAction(() => {
			this.updateUserSettings(mergedSnakeCase, {hydrate: false});
		});
		try {
			logger.debug('Updating user settings');
			const payload = convertKeysToSnakeCase<Record<string, unknown>>(sanitizedSettings);
			if ('customStatus' in sanitizedSettings) {
				payload.custom_status = toApiCustomStatusPayload(sanitizedSettings.customStatus ?? null);
			}
			await http.patch(Endpoints.USER_SETTINGS, {body: payload});
			logger.debug('Successfully updated user settings');
		} catch (error) {
			logger.error('Failed to update user settings:', error);
			runInAction(() => {
				const currentSnapshot = this.snapshot;
				const revertedSnapshot = {...currentSnapshot};
				for (const key of Object.keys(sanitizedSettings) as Array<keyof UserSettings>) {
					restoreSettingValue(revertedSnapshot, previousSnapshot, key);
				}
				const revertedSnakeCase = convertKeysToSnakeCase<Record<string, unknown>>(revertedSnapshot);
				if (!includesSyncedPreferences) {
					delete revertedSnakeCase.synced_preferences;
				}
				this.updateUserSettings(revertedSnakeCase, {hydrate: false});
			});
			throw error;
		}
	}
}

const UserSettings = new UserSettingsState();
setLocalPresenceUserSettings(UserSettings);

export default UserSettings;
