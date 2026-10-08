// SPDX-License-Identifier: AGPL-3.0-or-later

type GoldenStorageDestination =
	| 'raw'
	| 'global'
	| 'every-account'
	| 'content-account'
	| 'shared-content'
	| 'named-account';

interface GoldenStorageEntry {
	readonly key: string;
	readonly value: string;
	readonly kind: 'session' | 'mobx-persist' | 'plain' | 'accessibility' | 'unknown';
	readonly destination: GoldenStorageDestination;
	readonly source: string;
}

const GOLDEN_ACCOUNT_USER_ID = '100000000000000002';
const GOLDEN_OTHER_ACCOUNT_USER_ID = '100000000000000003';
const GOLDEN_MAINTENANCE_ID = '9876543210';

function mobxPersistBlob(properties: Record<string, unknown>, version = 1): string {
	return JSON.stringify({...properties, __mps__: {version}});
}

const SESSION_ENTRIES: ReadonlyArray<GoldenStorageEntry> = [
	{
		key: 'token',
		value: 'ODAwMDAwMDAwMDAwMDAwMDAx.aBcDeF.notARealTokenValue00000000000',
		kind: 'session',
		destination: 'raw',
		source: 'features/platform/state/auth_session/AuthSessionStorage.ts:4',
	},
	{
		key: 'userId',
		value: GOLDEN_ACCOUNT_USER_ID,
		kind: 'session',
		destination: 'raw',
		source: 'features/platform/state/auth_session/AuthSessionStorage.ts:5',
	},
	{
		key: 'runtimeConfig',
		value: '{"legacy":true}',
		kind: 'session',
		destination: 'raw',
		source: 'features/auth/state/AccountStorage.ts:67 (MANAGED_KEY_EXACT, nothing writes it)',
	},
	{
		key: 'AccountManager',
		value: '{"legacy":true}',
		kind: 'session',
		destination: 'raw',
		source: 'features/auth/state/AccountStorage.ts:67 (MANAGED_KEY_EXACT, nothing writes it)',
	},
];

const MOBX_PERSIST_ENTRIES: ReadonlyArray<GoldenStorageEntry> = [
	{
		key: 'Theme',
		value: mobxPersistBlob({syncAcrossDevices: true, localTheme: 'dark', serverTheme: 'dark'}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/theme/state/Theme.ts:193',
	},
	{
		key: 'Drafts',
		value: mobxPersistBlob({drafts: {'110000000000000001': 'unsent message text'}, draftSegments: {}}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/messaging/state/MessagingDrafts.ts:61',
	},
	{
		key: 'SelectedGuild',
		value: mobxPersistBlob({lastSelectedGuildId: '120000000000000001'}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/navigation/state/SelectedGuild.ts:23',
	},
	{
		key: 'SelectedChannel',
		value: mobxPersistBlob({selectedChannelIds: {}, recentlyVisitedChannels: []}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/navigation/state/SelectedChannel.ts:50',
	},
	{
		key: 'VoiceSettings',
		value: mobxPersistBlob({inputDeviceId: 'default', outputVolume: 100, noiseSuppression: 'standard'}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/VoiceSettings.ts:426',
	},
	{
		key: 'LocalVoiceState',
		value: mobxPersistBlob({persistedSelfMute: true, persistedSelfDeaf: false, noiseSuppressionEnabled: true}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/LocalVoiceState.ts:248',
	},
	{
		key: 'VoiceSessionRestore',
		value: mobxPersistBlob({snapshot: {userId: GOLDEN_ACCOUNT_USER_ID, channelId: '110000000000000009'}}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/VoiceSessionRestore.ts:30',
	},
	{
		key: 'Notification',
		value: mobxPersistBlob({
			browserNotificationsEnabled: true,
			unreadMessageBadgeEnabled: true,
			ttsNotificationMode: 'never',
		}),
		kind: 'mobx-persist',
		destination: 'every-account',
		source: 'features/ui/state/Notification.ts:180',
	},
	{
		key: 'Inbox',
		value: mobxPersistBlob({
			selectedTab: 'unreads',
			hasAutoOpenedBookmarksPopoutForFirstSave: true,
			skipMarkAllAsReadConfirmation: false,
		}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/inbox/state/Inbox.ts:22',
	},
	{
		key: 'Location',
		value: mobxPersistBlob({lastLocation: '/channels/@me', lastMobileLayoutState: null}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/ui/state/Location.ts:22',
	},
	{
		key: 'GuildFolderExpanded',
		value: mobxPersistBlob({expandedFolderIds: [1]}),
		kind: 'mobx-persist',
		destination: 'every-account',
		source: 'features/guild/state/GuildFolderExpanded.ts:15',
	},
	{
		key: 'SearchHistory',
		value: mobxPersistBlob({entriesByChannel: {'110000000000000001': ['from:me']}}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/search/state/SearchHistory.ts:18',
	},
	{
		key: 'ChannelFrecencyLocal',
		value: mobxPersistBlob({localUseLog: {}}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/channel/state/ChannelFrecency.ts:206',
	},
	{
		key: 'GuildMatureContentAgreeLocal',
		value: mobxPersistBlob({localAgreedChannelIds: ['110000000000000001']}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/guild/state/GuildMatureContentAgree.ts:116',
	},
	{
		key: 'ThreadPanelWidth',
		value: mobxPersistBlob({width: 420}),
		kind: 'mobx-persist',
		destination: 'shared-content',
		source: 'features/threads/state/ThreadPanelWidth.ts:30',
	},
	{
		key: 'SearchEngine',
		value: mobxPersistBlob({engines: [{id: 'ddg', name: 'DuckDuckGo'}]}, 2),
		kind: 'mobx-persist',
		destination: 'every-account',
		source: 'features/search/state/SearchEngine.ts:62',
	},
	{
		key: 'ReverseImageSearch',
		value: mobxPersistBlob({engines: [{id: 'google', name: 'Google Lens'}]}, 2),
		kind: 'mobx-persist',
		destination: 'every-account',
		source: 'features/search/state/ReverseImageSearch.ts:71',
	},
	{
		key: 'Slowmode',
		value: mobxPersistBlob({lastSendTimestamps: {}, cooldownExpiresAt: {}}),
		kind: 'mobx-persist',
		destination: 'every-account',
		source: 'features/slowmode/state/Slowmode.ts:50',
	},
	{
		key: 'Translation',
		value: mobxPersistBlob({engines: [{id: 'deepl'}]}),
		kind: 'mobx-persist',
		destination: 'every-account',
		source: 'features/messaging/state/Translation.ts:77',
	},
	{
		key: 'Keybind',
		value: mobxPersistBlob(
			{
				customKeybinds: {},
				transmitMode: 'voice_activity',
				pushToTalkReleaseDelay: 200,
				syncAcrossDevices: false,
				disableBuiltinKeybinds: false,
			},
			5,
		),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/input/state/InputKeybind.ts:107,287,1001',
	},
	{
		key: 'ParticipantVolume',
		value: mobxPersistBlob({volumes: {'130000000000000001': 140}, localMutes: {}}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/ParticipantVolume.ts:90',
	},
	{
		key: 'StreamAudioPrefs',
		value: mobxPersistBlob({entries: {'130000000000000001': {volume: 80}}}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/StreamAudioPrefs.ts:45',
	},
	{
		key: 'EntranceSoundListenerPrefs',
		value: mobxPersistBlob({volumes: {'130000000000000001': 50}, localMutes: {}}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/EntranceSoundListenerPrefs.ts:27',
	},
	{
		key: 'NewDeviceMonitoring',
		value: mobxPersistBlob({knownDeviceIds: ['device-a'], ignoredDeviceIds: [], suppressAlerts: false}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/auth/state/NewDeviceMonitoring.tsx:94',
	},
	{
		key: 'AudioVolume',
		value: mobxPersistBlob({volume: 75, isMuted: false}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/AudioVolume.ts:19',
	},
	{
		key: 'VideoVolume',
		value: mobxPersistBlob({volume: 60, isMuted: true}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/voice/state/VideoVolume.ts:19',
	},
	{
		key: 'MobileLayout',
		value: mobxPersistBlob({navExpanded: false, chatExpanded: true}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/ui/state/MobileLayout.ts:31',
	},
	{
		key: 'MacPermissions',
		value: mobxPersistBlob({decisions: {microphone: 'granted'}, setupCompleted: true}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/permissions/system/state/MacPermissions.ts:58',
	},
	{
		key: 'DeveloperMode',
		value: mobxPersistBlob({manuallyEnabled: true}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/devtools/state/DeveloperMode.ts:22',
	},
	{
		key: 'DeveloperOptions',
		value: mobxPersistBlob({showVoiceDebugOverlay: true}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/devtools/state/DeveloperOptions.ts:193',
	},
	{
		key: 'StreamerMode',
		value: mobxPersistBlob({
			manualEnabled: false,
			autoEnable: true,
			hidePersonalInformation: true,
			hideInviteLinks: true,
			disableSounds: false,
			disableNotifications: false,
			nagbarDismissed: true,
		}),
		kind: 'mobx-persist',
		destination: 'global',
		source: 'features/streamer_mode/state/StreamerMode.ts:9,50',
	},
];

const PLAIN_ENTRIES: ReadonlyArray<GoldenStorageEntry> = [
	{
		key: 'theme',
		value: 'dark',
		kind: 'plain',
		destination: 'global',
		source: 'features/theme/state/Theme.ts:25',
	},
	{
		key: 'locale',
		value: 'sv',
		kind: 'plain',
		destination: 'global',
		source: 'app/I18n.ts:156',
	},
	{
		key: 'debugLoggingEnabled',
		value: 'true',
		kind: 'plain',
		destination: 'global',
		source: 'features/platform/utils/AppLogger.ts:41',
	},
	{
		key: 'SkeletonLayoutMemory',
		value: JSON.stringify({version: 9, updatedAt: 1_755_000_000_000, accountFingerprint: 'abc123'}),
		kind: 'plain',
		destination: 'global',
		source: 'features/app/components/skeleton/SkeletonLayoutMemory.ts:16',
	},
	{
		key: 'MessageEdit',
		value: JSON.stringify({'110000000000000001': 'half-finished edit'}),
		kind: 'plain',
		destination: 'shared-content',
		source: 'features/messaging/state/MessageEdit.ts:7',
	},
	{
		key: 'ChannelFrecencyThreadGuilds',
		value: JSON.stringify(['110000000000000002']),
		kind: 'plain',
		destination: 'shared-content',
		source: 'features/channel/state/ChannelFrecency.ts:42',
	},
	{
		key: 'TrustedDomain',
		value: JSON.stringify(['example.com']),
		kind: 'plain',
		destination: 'shared-content',
		source: 'features/trusted_domain/state/TrustedDomain.ts:17',
	},
	{
		key: 'SoftwareEncoderWarning_neverShowAgain',
		value: 'true',
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/state/SoftwareEncoderWarning.ts:8',
	},
	{
		key: 'pip_corner',
		value: 'bottom-right',
		kind: 'plain',
		destination: 'global',
		source: 'features/ui/state/PiP.ts:21',
	},
	{
		key: 'pip_width',
		value: '480',
		kind: 'plain',
		destination: 'global',
		source: 'features/ui/state/PiP.ts:22',
	},
	{
		key: 'incoming_call_overlay_position',
		value: JSON.stringify({x: 24, y: 96}),
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/components/IncomingCallOverlayConstants.ts:5',
	},
	{
		key: 'compact_voice_call_heights',
		value: JSON.stringify({defaultHeight: 220, heightsByKey: {}, expandedByKey: {}}),
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/state/CompactVoiceCallHeight.ts:12',
	},
	{
		key: 'member_list_default_hidden_channel_overrides',
		value: JSON.stringify({'110000000000000001': true}),
		kind: 'plain',
		destination: 'shared-content',
		source: 'features/member/state/MemberList.ts:10',
	},
	{
		key: 'AdvancedSettings:unreadBadgeCustomizationEnabled',
		value: 'true',
		kind: 'plain',
		destination: 'every-account',
		source: 'features/user/state/AdvancedSettings.ts:6',
	},
	{
		key: 'AdvancedSettings:keepAttachmentsOnEmptyMessageEdit',
		value: 'true',
		kind: 'plain',
		destination: 'every-account',
		source: 'features/user/state/AdvancedSettings.ts:7',
	},
	{
		key: 'AdvancedSettings:expressionCloneShortcutsEnabled',
		value: 'true',
		kind: 'plain',
		destination: 'every-account',
		source: 'features/user/state/AdvancedSettings.ts:8',
	},
	{
		key: 'UserSettings:syncedPreferencesLocal',
		value: 'CgYIARICEAE=',
		kind: 'plain',
		destination: 'every-account',
		source: 'features/user/state/UserSettings.ts:115',
	},
	{
		key: 'UserSettings:syncedPreferencesWire',
		value: 'CgYIARICEAE=',
		kind: 'plain',
		destination: 'raw',
		source: 'features/user/state/UserSettings.ts:116',
	},
	{
		key: 'UserSettings:syncedPreferencesDirtyFields',
		value: JSON.stringify(['theme']),
		kind: 'plain',
		destination: 'raw',
		source: 'features/user/state/UserSettings.ts:114',
	},
	{
		key: 'UserSettings:syncedPreferencesRecentAck',
		value: JSON.stringify({theme: 1_755_000_000_000}),
		kind: 'plain',
		destination: 'raw',
		source: 'features/user/state/UserSettings.ts:117',
	},
	{
		key: 'ThemeStudio:section',
		value: 'library',
		kind: 'plain',
		destination: 'global',
		source: 'features/theme_studio/state/ThemeStudioState.ts:8',
	},
	{
		key: 'ThemeStudio:expandedGroups',
		value: JSON.stringify(['colors']),
		kind: 'plain',
		destination: 'global',
		source: 'features/theme_studio/state/ThemeStudioState.ts:9',
	},
	{
		key: 'ThemeStudio:librarySplit',
		value: '320',
		kind: 'plain',
		destination: 'global',
		source: 'features/theme_studio/sections/LibrarySection.tsx:405',
	},
	{
		key: 'fluxer.lastPushEndpoint',
		value: 'https://fcm.googleapis.com/fcm/send/abc123',
		kind: 'plain',
		destination: 'global',
		source: 'features/platform/push/PushSubscriptionService.ts:19',
	},
	{
		key: 'fluxer:media:volume',
		value: '0.8',
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/components/media_player/hooks/useMediaPlayer.ts:9',
	},
	{
		key: 'fluxer:media:muted',
		value: 'false',
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/components/media_player/hooks/useMediaPlayer.ts:10',
	},
	{
		key: 'fluxer:media:playbackRate',
		value: '1.25',
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/components/media_player/hooks/useMediaPlayer.ts:11',
	},
	{
		key: 'fluxer:media_player:volume',
		value: '0.6',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/voice/components/media_player/utils/MediaConstants.ts:15',
	},
	{
		key: 'fluxer:media_player:muted',
		value: 'true',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/voice/components/media_player/utils/MediaConstants.ts:16',
	},
	{
		key: 'fluxer:media_player:playback-rate',
		value: '2',
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/components/media_player/utils/MediaConstants.ts:17',
	},
	{
		key: 'fluxer:media_caps:v2',
		value: JSON.stringify({av1: false, h265: true}),
		kind: 'plain',
		destination: 'global',
		source: 'features/voice/utils/MediaCapabilities.ts:19',
	},
	{
		key: 'fluxer:ui:sidebar-width',
		value: '286',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/ui/state/SidebarWidth.ts:7',
	},
	{
		key: 'fluxer:ui:channel-pins-popout-size',
		value: '{"width":480,"height":620}',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/channel/components/popouts/ChannelPinsPopout.tsx:74',
	},
	{
		key: 'fluxer:media:playbackRates',
		value: '{"audio":1.25,"video":1.5}',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/voice/components/media_player/hooks/useMediaPlayer.ts:16',
	},
	{
		key: 'Keybind:globalDefaultMigration:v1',
		value: '1',
		kind: 'plain',
		destination: 'global',
		source: 'features/input/state/InputKeybind.ts:208',
	},
	{
		key: 'Keybind:builtinDisableMarkerMigration:v1',
		value: '1',
		kind: 'plain',
		destination: 'global',
		source: 'features/input/state/InputKeybind.ts:209',
	},
	{
		key: 'fluxer:ui:expression-picker-size',
		value: JSON.stringify({width: 420, height: 480}),
		kind: 'plain',
		destination: 'content-account',
		source: 'features/expressions/components/popouts/ExpressionPickerPopout.tsx:220',
	},
	{
		key: 'fluxer:ui:inbox-popout-size',
		value: JSON.stringify({width: 500, height: 640}),
		kind: 'plain',
		destination: 'content-account',
		source: 'features/messaging/components/popouts/InboxPopout.tsx:95',
	},
	{
		key: `fluxer_scheduled_maintenance_dismissed:${GOLDEN_MAINTENANCE_ID}`,
		value: '1',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/app/components/layout/app_layout/ScheduledMaintenanceDismissal.ts:8',
	},
	{
		key: `fluxer_scheduled_maintenance_dismissed:${GOLDEN_MAINTENANCE_ID}:in_progress`,
		value: '1',
		kind: 'plain',
		destination: 'content-account',
		source: 'features/app/components/layout/app_layout/ScheduledMaintenanceDismissal.ts:12',
	},
];

const ACCESSIBILITY_ENTRIES: ReadonlyArray<GoldenStorageEntry> = [
	{
		key: 'Accessibility',
		value: JSON.stringify({zoomLevel: 1.25, reducedMotionOverride: null}),
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:34',
	},
	{
		key: 'Accessibility:zoomLevel',
		value: '1.25',
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:35',
	},
	{
		key: 'Accessibility:customThemeCss',
		value: JSON.stringify(':root { --flx-accent: #ff00aa; }'),
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:36',
	},
	{
		key: 'Accessibility:customThemeCssSyncAcrossDevices',
		value: 'false',
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:42',
	},
	{
		key: 'Accessibility:motion',
		value: JSON.stringify({syncReducedMotionWithSystem: false, reducedMotionOverride: true}),
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:37',
	},
	{
		key: 'Accessibility:videoSeekPreviewThumbnails',
		value: 'false',
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:41',
	},
	{
		key: 'Accessibility:showNeko',
		value: 'true',
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:38',
	},
	{
		key: `Accessibility:showNeko:${GOLDEN_OTHER_ACCOUNT_USER_ID}`,
		value: 'false',
		kind: 'accessibility',
		destination: 'named-account',
		source: 'features/accessibility/state/Accessibility.ts:43',
	},
	{
		key: 'Accessibility:keepNekoStill',
		value: 'false',
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:39',
	},
	{
		key: `Accessibility:keepNekoStill:${GOLDEN_OTHER_ACCOUNT_USER_ID}`,
		value: 'true',
		kind: 'accessibility',
		destination: 'named-account',
		source: 'features/accessibility/state/Accessibility.ts:44',
	},
	{
		key: 'Accessibility:pinNekoToTextarea',
		value: 'true',
		kind: 'accessibility',
		destination: 'every-account',
		source: 'features/accessibility/state/Accessibility.ts:40',
	},
	{
		key: `Accessibility:pinNekoToTextarea:${GOLDEN_OTHER_ACCOUNT_USER_ID}`,
		value: 'false',
		kind: 'accessibility',
		destination: 'named-account',
		source: 'features/accessibility/state/Accessibility.ts:45',
	},
];

export const GOLDEN_UNKNOWN_STORAGE_ENTRIES: ReadonlyArray<GoldenStorageEntry> = [
	{
		key: 'LegacyExperiment:cohort',
		value: 'control',
		kind: 'unknown',
		destination: 'global',
		source: 'no writer in this tree, retired build residue',
	},
	{
		key: 'fluxer:experimental:unshipped-toggle',
		value: 'on',
		kind: 'unknown',
		destination: 'global',
		source: 'no writer in this tree. It matches MANAGED_KEY_PREFIXES, so the deployed swap moves it today',
	},
	{
		key: 'mobx-legacy-orphan-store',
		value: '{"__mps__":{"version":1}}',
		kind: 'unknown',
		destination: 'global',
		source: 'no writer in this tree, matches MANAGED_KEY_PREFIXES',
	},
	{
		key: 'persist:orphaned-widget-state',
		value: '{}',
		kind: 'unknown',
		destination: 'global',
		source: 'no writer in this tree, matches MANAGED_KEY_PREFIXES',
	},
	{
		key: '__vendor_extension_probe__',
		value: '1',
		kind: 'unknown',
		destination: 'global',
		source: 'no writer in this tree, third-party residue on the same origin',
	},
];

export const GOLDEN_DEPLOYED_STORAGE_ENTRIES: ReadonlyArray<GoldenStorageEntry> = [
	...SESSION_ENTRIES,
	...MOBX_PERSIST_ENTRIES,
	...PLAIN_ENTRIES,
	...ACCESSIBILITY_ENTRIES,
];

export const GOLDEN_LOCAL_STORAGE_CORPUS: ReadonlyArray<GoldenStorageEntry> = [
	...GOLDEN_DEPLOYED_STORAGE_ENTRIES,
	...GOLDEN_UNKNOWN_STORAGE_ENTRIES,
];

export const NEVER_SCOPED_STORAGE_KEYS = ['token', 'userId', 'runtimeConfig', 'AccountManager'] as const;

export const OWNED_CONTENT_STORAGE_KEYS: ReadonlyArray<string> = GOLDEN_DEPLOYED_STORAGE_ENTRIES.filter(
	(entry) => entry.destination === 'content-account',
).map((entry) => entry.key);

export const SHARED_CONTENT_STORAGE_KEYS: ReadonlyArray<string> = GOLDEN_DEPLOYED_STORAGE_ENTRIES.filter(
	(entry) => entry.destination === 'shared-content',
).map((entry) => entry.key);

export const CONTENT_STORAGE_KEYS: ReadonlyArray<string> = [
	...OWNED_CONTENT_STORAGE_KEYS,
	...SHARED_CONTENT_STORAGE_KEYS,
];

export const NOT_MIGRATED_STORAGE_KEYS = [
	'UserSettings:syncedPreferencesWire',
	'UserSettings:syncedPreferencesDirtyFields',
	'UserSettings:syncedPreferencesRecentAck',
] as const;

export interface GoldenStorageScopes {
	readonly global: string;
	readonly everyAccount: ReadonlyArray<string>;
	readonly contentAccount: string | null;
	readonly sharedContent: ReadonlyArray<string>;
	readonly namedAccount: string;
}

function goldenDestinationScopes(entry: GoldenStorageEntry, scopes: GoldenStorageScopes): ReadonlyArray<string> {
	switch (entry.destination) {
		case 'raw':
			return [];
		case 'global':
			return [scopes.global];
		case 'every-account':
			return scopes.everyAccount;
		case 'content-account':
			return scopes.contentAccount === null ? [] : [scopes.contentAccount];
		case 'shared-content':
			return scopes.sharedContent;
		case 'named-account':
			return [scopes.namedAccount];
	}
}

export function goldenStorageRowIdentity(scope: string, key: string): string {
	return `${scope} ${key}`;
}

export function expectedGoldenStorageRows(scopes: GoldenStorageScopes): Map<string, string> {
	const rows = new Map<string, string>();
	for (const entry of GOLDEN_LOCAL_STORAGE_CORPUS) {
		for (const scope of goldenDestinationScopes(entry, scopes)) {
			rows.set(goldenStorageRowIdentity(scope, entry.key), entry.value);
		}
	}
	rows.set(
		goldenStorageRowIdentity(scopes.global, 'fluxer:migration:unclassified-legacy-keys'),
		JSON.stringify(GOLDEN_UNKNOWN_STORAGE_ENTRIES.map((entry) => entry.key).sort()),
	);
	return rows;
}

export const DEPLOYED_MANAGED_KEY_PREFIXES = ['mobx', 'mobx-persist', 'persist', 'fluxer'] as const;

export function seedGoldenLocalStorage(storage: Storage): void {
	for (const entry of GOLDEN_LOCAL_STORAGE_CORPUS) {
		storage.setItem(entry.key, entry.value);
	}
}

export function readGoldenLocalStorageSnapshot(storage: Storage): Record<string, string> {
	const snapshot: Record<string, string> = {};
	for (let index = 0; index < storage.length; index++) {
		const key = storage.key(index);
		if (key == null) {
			continue;
		}
		const value = storage.getItem(key);
		if (value != null) {
			snapshot[key] = value;
		}
	}
	return snapshot;
}
