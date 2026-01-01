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

import {msg} from '@lingui/core/macro';
import i18n from '~/i18n';
import type {SettingsTab, UserSettingsTabType} from './settingsConstants';

export interface SearchableSettingItem {
	id: string;
	tabType: UserSettingsTabType;
	sectionId?: string;
	label: string;
	keywords: Array<string>;
	description?: string;
}

export interface SettingsSearchResult {
	tab: SettingsTab;
	matchedItems: Array<SearchableSettingItem>;
	score: number;
}

const SEARCHABLE_ITEMS_DESCRIPTORS = [
	{
		id: 'profile-avatar',
		tabType: 'my_profile',
		label: msg`Avatar`,
		keywords: ['avatar', 'profile picture', 'photo', 'image', 'pfp', 'picture'],
		description: msg`Change your profile picture`,
	},
	{
		id: 'profile-banner',
		tabType: 'my_profile',
		label: msg`Banner`,
		keywords: ['banner', 'cover', 'header', 'background'],
		description: msg`Customize your profile banner`,
	},
	{
		id: 'profile-username',
		tabType: 'my_profile',
		label: msg`Username`,
		keywords: ['username', 'name', 'display name', 'user', 'handle', 'tag'],
		description: msg`Change your username`,
	},
	{
		id: 'profile-bio',
		tabType: 'my_profile',
		label: msg`Bio`,
		keywords: ['bio', 'about me', 'description', 'biography', 'about'],
		description: msg`Edit your profile bio`,
	},
	{
		id: 'profile-accent-color',
		tabType: 'my_profile',
		label: msg`Accent Color`,
		keywords: ['accent', 'color', 'theme color', 'profile color'],
		description: msg`Choose your profile accent color`,
	},
	{
		id: 'profile-pronouns',
		tabType: 'my_profile',
		label: msg`Pronouns`,
		keywords: ['pronouns', 'identity', 'he', 'she', 'they'],
		description: msg`Set your pronouns`,
	},
	{
		id: 'profile-badge',
		tabType: 'my_profile',
		label: msg`Badge`,
		keywords: ['badge', 'premium badge', 'nitro', 'plutonium'],
		description: msg`Configure your profile badge`,
	},

	{
		id: 'account-email',
		tabType: 'account_security',
		sectionId: 'account',
		label: msg`Email`,
		keywords: ['email', 'mail', 'address', 'contact'],
		description: msg`Change your email address`,
	},
	{
		id: 'account-password',
		tabType: 'account_security',
		sectionId: 'security',
		label: msg`Password`,
		keywords: ['password', 'credentials', 'login', 'security', 'change password'],
		description: msg`Change your password`,
	},
	{
		id: 'account-phone',
		tabType: 'account_security',
		sectionId: 'account',
		label: msg`Phone Number`,
		keywords: ['phone', 'number', 'mobile', 'telephone', 'sms'],
		description: msg`Add or change your phone number`,
	},
	{
		id: 'account-2fa',
		tabType: 'account_security',
		sectionId: 'security',
		label: msg`Two-Factor Authentication`,
		keywords: ['2fa', 'two factor', 'mfa', 'authentication', 'authenticator', 'totp', 'otp', 'security'],
		description: msg`Enable two-factor authentication`,
	},
	{
		id: 'account-backup-codes',
		tabType: 'account_security',
		sectionId: 'security',
		label: msg`Backup Codes`,
		keywords: ['backup codes', 'recovery', 'codes', '2fa backup'],
		description: msg`View or regenerate backup codes`,
	},
	{
		id: 'account-delete',
		tabType: 'account_security',
		sectionId: 'account',
		label: msg`Delete Account`,
		keywords: ['delete', 'remove', 'close account', 'deactivate'],
		description: msg`Delete your account`,
	},
	{
		id: 'account-disable',
		tabType: 'account_security',
		sectionId: 'account',
		label: msg`Disable Account`,
		keywords: ['disable', 'deactivate', 'suspend'],
		description: msg`Temporarily disable your account`,
	},

	{
		id: 'appearance-theme',
		tabType: 'appearance',
		sectionId: 'theme',
		label: msg`Theme`,
		keywords: ['theme', 'dark mode', 'light mode', 'dark', 'light', 'coal', 'color scheme', 'system theme'],
		description: msg`Choose your color theme`,
	},
	{
		id: 'appearance-sync-theme',
		tabType: 'appearance',
		sectionId: 'theme',
		label: msg`Sync Theme`,
		keywords: ['sync', 'sync theme', 'theme sync', 'devices'],
		description: msg`Sync theme across devices`,
	},
	{
		id: 'appearance-custom-css',
		tabType: 'appearance',
		sectionId: 'theme',
		label: msg`Custom CSS`,
		keywords: ['css', 'custom', 'theme tokens', 'overrides', 'colors', 'fonts', 'styling'],
		description: msg`Customize with CSS`,
	},
	{
		id: 'appearance-message-display',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Message Display`,
		keywords: ['messages', 'display', 'compact', 'cozy', 'message style'],
		description: msg`Change message display style`,
	},
	{
		id: 'appearance-font-size',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Font Size`,
		keywords: ['font', 'size', 'text size', 'zoom', 'scale', 'chat font'],
		description: msg`Adjust font size`,
	},
	{
		id: 'appearance-message-grouping',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Message Grouping`,
		keywords: ['grouping', 'spacing', 'messages', 'group'],
		description: msg`Configure message grouping`,
	},
	{
		id: 'appearance-sidebar',
		tabType: 'appearance',
		sectionId: 'interface',
		label: msg`Sidebar`,
		keywords: ['sidebar', 'channel list', 'navigation', 'member list'],
		description: msg`Configure sidebar settings`,
	},
	{
		id: 'appearance-member-list',
		tabType: 'appearance',
		sectionId: 'interface',
		label: msg`Member List`,
		keywords: ['member list', 'users', 'sidebar', 'members'],
		description: msg`Toggle member list visibility`,
	},
	{
		id: 'appearance-timestamps',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Timestamps`,
		keywords: ['timestamps', 'time', 'date', 'message time'],
		description: msg`Configure timestamp display`,
	},
	{
		id: 'appearance-channel-typing-indicators',
		tabType: 'appearance',
		sectionId: 'interface',
		label: msg`Channel List Typing Indicators`,
		keywords: ['typing', 'typing indicator', 'channel list', 'typing avatars', 'indicator', 'who is typing'],
		description: msg`Choose how typing indicators appear in the channel list`,
	},
	{
		id: 'appearance-keyboard-hints',
		tabType: 'appearance',
		sectionId: 'interface',
		label: msg`Keyboard Hints`,
		keywords: ['keyboard', 'hints', 'tooltips', 'shortcut badges', 'keyboard shortcuts'],
		description: msg`Control keyboard shortcut hints in tooltips`,
	},
	{
		id: 'appearance-voice-channel-join',
		tabType: 'appearance',
		sectionId: 'interface',
		label: msg`Voice Channel Join Behavior`,
		keywords: ['voice channel', 'join', 'double click', 'double-click', 'single click', 'voice join'],
		description: msg`Control how you join voice channels`,
	},
	{
		id: 'appearance-message-display-mode',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Message Display Mode`,
		keywords: ['comfy', 'dense', 'compact', 'cozy', 'message layout', 'display mode'],
		description: msg`Choose between comfy and dense message layouts`,
	},
	{
		id: 'appearance-hide-avatars',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Hide User Avatars`,
		keywords: ['avatars', 'hide avatars', 'compact mode', 'user avatars', 'profile pictures'],
		description: msg`Hide user avatars in compact message mode`,
	},
	{
		id: 'appearance-message-group-spacing',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Message Group Spacing`,
		keywords: ['spacing', 'message groups', 'gap', 'space between', 'message spacing'],
		description: msg`Adjust spacing between groups of messages`,
	},
	{
		id: 'appearance-message-dividers',
		tabType: 'appearance',
		sectionId: 'messages',
		label: msg`Message Dividers`,
		keywords: ['dividers', 'separator', 'lines', 'divider lines', 'message separator'],
		description: msg`Show divider lines between message groups`,
	},

	{
		id: 'accessibility-reduced-motion',
		tabType: 'accessibility',
		sectionId: 'motion',
		label: msg`Reduced Motion`,
		keywords: ['motion', 'animation', 'reduced motion', 'accessibility', 'animations'],
		description: msg`Reduce interface animations`,
	},
	{
		id: 'accessibility-contrast',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: msg`Contrast`,
		keywords: ['contrast', 'high contrast', 'visibility', 'accessibility'],
		description: msg`Adjust interface contrast`,
	},
	{
		id: 'accessibility-saturation',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: msg`Saturation`,
		keywords: ['saturation', 'color', 'colors', 'vibrancy'],
		description: msg`Adjust color saturation`,
	},
	{
		id: 'accessibility-stickers',
		tabType: 'accessibility',
		sectionId: 'animation',
		label: msg`Sticker Animation`,
		keywords: ['stickers', 'animation', 'animated stickers', 'gif'],
		description: msg`Control sticker animations`,
	},
	{
		id: 'accessibility-emoji',
		tabType: 'accessibility',
		sectionId: 'animation',
		label: msg`Emoji Animation`,
		keywords: ['emoji', 'animated emoji', 'animation'],
		description: msg`Control emoji animations`,
	},
	{
		id: 'accessibility-gif',
		tabType: 'accessibility',
		sectionId: 'animation',
		label: msg`GIF Autoplay`,
		keywords: ['gif', 'autoplay', 'animation', 'auto play'],
		description: msg`Control GIF autoplay`,
	},
	{
		id: 'accessibility-keyboard-navigation',
		tabType: 'accessibility',
		sectionId: 'keyboard',
		label: msg`Keyboard Navigation`,
		keywords: ['keyboard', 'navigation', 'shortcuts', 'focus', 'tab'],
		description: msg`Configure keyboard navigation`,
	},
	{
		id: 'accessibility-screen-reader',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: msg`Screen Reader`,
		keywords: ['screen reader', 'accessibility', 'a11y', 'aria'],
		description: msg`Optimize for screen readers`,
	},

	{
		id: 'chat-render-embeds',
		tabType: 'chat_settings',
		sectionId: 'media',
		label: msg`Embeds`,
		keywords: ['embeds', 'previews', 'links', 'url preview', 'link preview'],
		description: msg`Show link previews`,
	},
	{
		id: 'chat-render-reactions',
		tabType: 'chat_settings',
		sectionId: 'display',
		label: msg`Reactions`,
		keywords: ['reactions', 'emoji', 'react'],
		description: msg`Show message reactions`,
	},
	{
		id: 'chat-inline-media',
		tabType: 'chat_settings',
		sectionId: 'media',
		label: msg`Inline Media`,
		keywords: ['media', 'images', 'videos', 'attachments', 'inline'],
		description: msg`Show inline images and videos`,
	},
	{
		id: 'chat-spoilers',
		tabType: 'chat_settings',
		sectionId: 'display',
		label: msg`Spoilers`,
		keywords: ['spoilers', 'spoiler', 'hide', 'reveal'],
		description: msg`Configure spoiler behavior`,
	},
	{
		id: 'chat-autocomplete',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: msg`Autocomplete`,
		keywords: ['autocomplete', 'suggestions', 'emoji picker', 'mentions'],
		description: msg`Configure autocomplete`,
	},
	{
		id: 'chat-emoji-picker',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: msg`Emoji Picker`,
		keywords: ['emoji picker', 'emoji', 'expressions'],
		description: msg`Configure emoji picker`,
	},
	{
		id: 'chat-sticker-suggestions',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: msg`Sticker Suggestions`,
		keywords: ['sticker', 'suggestions', 'stickers'],
		description: msg`Show sticker suggestions`,
	},
	{
		id: 'chat-send-typing',
		tabType: 'chat_settings',
		sectionId: 'interaction',
		label: msg`Typing Indicator`,
		keywords: ['typing', 'indicator', 'typing status'],
		description: msg`Show when you are typing`,
	},

	{
		id: 'voice-input-device',
		tabType: 'voice_video',
		sectionId: 'voice',
		label: msg`Input Device`,
		keywords: ['microphone', 'mic', 'input', 'audio input', 'device'],
		description: msg`Select your microphone`,
	},
	{
		id: 'voice-output-device',
		tabType: 'voice_video',
		sectionId: 'voice',
		label: msg`Output Device`,
		keywords: ['speaker', 'output', 'audio output', 'headphones', 'device'],
		description: msg`Select your speakers`,
	},
	{
		id: 'voice-volume',
		tabType: 'voice_video',
		sectionId: 'voice',
		label: msg`Volume`,
		keywords: ['volume', 'loudness', 'audio level'],
		description: msg`Adjust volume levels`,
	},
	{
		id: 'voice-push-to-talk',
		tabType: 'voice_video',
		sectionId: 'voice',
		label: msg`Push to Talk`,
		keywords: ['push to talk', 'ptt', 'voice activation', 'keybind'],
		description: msg`Configure push to talk`,
	},
	{
		id: 'voice-noise-suppression',
		tabType: 'voice_video',
		sectionId: 'voice',
		label: msg`Noise Suppression`,
		keywords: ['noise', 'suppression', 'echo', 'background noise', 'krisp'],
		description: msg`Enable noise suppression`,
	},
	{
		id: 'voice-echo-cancellation',
		tabType: 'voice_video',
		sectionId: 'voice',
		label: msg`Echo Cancellation`,
		keywords: ['echo', 'cancellation', 'audio'],
		description: msg`Enable echo cancellation`,
	},
	{
		id: 'video-camera',
		tabType: 'voice_video',
		sectionId: 'video',
		label: msg`Camera`,
		keywords: ['camera', 'webcam', 'video', 'video input'],
		description: msg`Select your camera`,
	},
	{
		id: 'video-preview',
		tabType: 'voice_video',
		sectionId: 'video',
		label: msg`Video Preview`,
		keywords: ['preview', 'video preview', 'camera preview'],
		description: msg`Preview your video`,
	},

	{
		id: 'notifications-desktop',
		tabType: 'notifications',
		sectionId: 'notifications',
		label: msg`Desktop Notifications`,
		keywords: ['notifications', 'desktop', 'alerts', 'popup', 'toast'],
		description: msg`Configure desktop notifications`,
	},
	{
		id: 'notifications-sounds',
		tabType: 'notifications',
		sectionId: 'sounds',
		label: msg`Notification Sounds`,
		keywords: ['sounds', 'audio', 'notification sound', 'alert sound', 'mute'],
		description: msg`Configure notification sounds`,
	},
	{
		id: 'notifications-mentions',
		tabType: 'notifications',
		sectionId: 'notifications',
		label: msg`Mention Notifications`,
		keywords: ['mentions', 'ping', '@', 'notify'],
		description: msg`Configure mention notifications`,
	},
	{
		id: 'notifications-dms',
		tabType: 'notifications',
		sectionId: 'notifications',
		label: msg`DM Notifications`,
		keywords: ['dm', 'direct message', 'private message'],
		description: msg`Configure DM notifications`,
	},
	{
		id: 'notifications-push',
		tabType: 'notifications',
		sectionId: 'push',
		label: msg`Push Notifications`,
		keywords: ['push', 'mobile', 'push notifications'],
		description: msg`Configure push notifications`,
	},
	{
		id: 'notifications-message-sound',
		tabType: 'notifications',
		sectionId: 'sounds',
		label: msg`Message Sound`,
		keywords: ['message sound', 'sound effect', 'new message'],
		description: msg`Configure message sounds`,
	},
	{
		id: 'notifications-call-sound',
		tabType: 'notifications',
		sectionId: 'sounds',
		label: msg`Call Sound`,
		keywords: ['call', 'ring', 'ringtone', 'incoming call'],
		description: msg`Configure call sounds`,
	},
	{
		id: 'notifications-disable-all-sounds',
		tabType: 'notifications',
		sectionId: 'sounds',
		label: msg`Disable All Sounds`,
		keywords: ['disable sounds', 'mute all', 'silence', 'quiet', 'mute notifications'],
		description: msg`Disable all notification sounds`,
	},
	{
		id: 'notifications-custom-sounds',
		tabType: 'notifications',
		sectionId: 'sounds',
		label: msg`Custom Sounds`,
		keywords: [
			'custom sounds',
			'upload sound',
			'custom notification',
			'custom ringtone',
			'mp3',
			'wav',
			'audio',
			'plutonium',
		],
		description: msg`Upload custom notification sounds`,
	},

	{
		id: 'privacy-dms',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Direct Messages`,
		keywords: ['dm', 'direct message', 'privacy', 'who can dm'],
		description: msg`Control who can DM you`,
	},
	{
		id: 'privacy-friend-requests',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Friend Requests`,
		keywords: ['friends', 'requests', 'friend requests', 'privacy'],
		description: msg`Control friend requests`,
	},
	{
		id: 'privacy-incoming-calls',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Incoming Calls`,
		keywords: [
			'incoming calls',
			'who can call',
			'call permissions',
			'block calls',
			'friends only calls',
			'call privacy',
		],
		description: msg`Control who can call you`,
	},
	{
		id: 'privacy-silent-calls',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Silent Calls`,
		keywords: ['silent', 'silent calls', 'ring behavior', 'mute calls', 'no ring'],
		description: msg`Configure silent call notifications`,
	},
	{
		id: 'privacy-group-chats',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Group Chat Permissions`,
		keywords: ['group chat', 'group dm', 'add to group', 'who can add', 'group permissions', 'group privacy'],
		description: msg`Control who can add you to group chats`,
	},
	{
		id: 'privacy-friends-of-friends',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Friends of Friends`,
		keywords: ['friends of friends', 'mutual friends', 'extended network'],
		description: msg`Allow friends of friends to contact you`,
	},
	{
		id: 'privacy-community-members',
		tabType: 'privacy_safety',
		sectionId: 'communication',
		label: msg`Community Members`,
		keywords: ['community', 'guild members', 'server members', 'community members'],
		description: msg`Allow community members to contact you`,
	},
	{
		id: 'privacy-activity-status',
		tabType: 'privacy_safety',
		sectionId: 'connections',
		label: msg`Activity Status`,
		keywords: ['activity', 'status', 'online status', 'presence', 'invisible'],
		description: msg`Control your activity status`,
	},
	{
		id: 'privacy-data-export',
		tabType: 'privacy_safety',
		sectionId: 'data-export',
		label: msg`Data Export`,
		keywords: ['export', 'data', 'download', 'gdpr'],
		description: msg`Export your data`,
	},
	{
		id: 'privacy-data-deletion',
		tabType: 'privacy_safety',
		sectionId: 'data-deletion',
		label: msg`Data Deletion`,
		keywords: ['delete', 'data', 'privacy', 'gdpr', 'remove'],
		description: msg`Delete your data`,
	},

	{
		id: 'language-locale',
		tabType: 'language',
		label: msg`Language`,
		keywords: ['language', 'locale', 'translation', 'localization'],
		description: msg`Choose your language`,
	},
	{
		id: 'language-timezone',
		tabType: 'language',
		label: msg`Timezone`,
		keywords: ['timezone', 'time zone', 'time', 'utc', 'gmt'],
		description: msg`Set your timezone`,
	},
	{
		id: 'language-date-format',
		tabType: 'language',
		label: msg`Date Format`,
		keywords: ['date', 'format', 'date format', 'time format'],
		description: msg`Configure date format`,
	},

	{
		id: 'keybinds-shortcuts',
		tabType: 'keybinds',
		label: msg`Keyboard Shortcuts`,
		keywords: ['keybinds', 'shortcuts', 'hotkeys', 'keyboard', 'keys'],
		description: msg`Configure keyboard shortcuts`,
	},
	{
		id: 'keybinds-ptt',
		tabType: 'keybinds',
		label: msg`Push to Talk Key`,
		keywords: ['push to talk', 'ptt', 'keybind', 'key'],
		description: msg`Set push to talk key`,
	},
	{
		id: 'keybinds-mute',
		tabType: 'keybinds',
		label: msg`Mute Keybind`,
		keywords: ['mute', 'keybind', 'toggle mute'],
		description: msg`Set mute keybind`,
	},
	{
		id: 'keybinds-deafen',
		tabType: 'keybinds',
		label: msg`Deafen Keybind`,
		keywords: ['deafen', 'keybind', 'toggle deafen'],
		description: msg`Set deafen keybind`,
	},

	{
		id: 'advanced-developer-mode',
		tabType: 'advanced',
		label: msg`Developer Mode`,
		keywords: ['developer', 'developer mode', 'dev', 'debug', 'copy id'],
		description: msg`Enable developer mode`,
	},
	{
		id: 'advanced-hardware-acceleration',
		tabType: 'advanced',
		label: msg`Hardware Acceleration`,
		keywords: ['hardware', 'acceleration', 'gpu', 'graphics', 'performance'],
		description: msg`Toggle hardware acceleration`,
	},

	{
		id: 'devices-sessions',
		tabType: 'devices',
		label: msg`Active Sessions`,
		keywords: ['devices', 'sessions', 'login', 'active', 'logged in'],
		description: msg`View active sessions`,
	},
	{
		id: 'devices-logout-all',
		tabType: 'devices',
		label: msg`Log Out All Devices`,
		keywords: ['logout', 'log out', 'devices', 'sessions', 'sign out'],
		description: msg`Log out of all devices`,
	},

	{
		id: 'blocked-users',
		tabType: 'blocked_users',
		label: msg`Blocked Users`,
		keywords: ['blocked', 'block', 'users', 'unblock'],
		description: msg`Manage blocked users`,
	},

	{
		id: 'authorized-apps',
		tabType: 'authorized_apps',
		label: msg`Authorized Apps`,
		keywords: ['apps', 'authorized', 'oauth', 'permissions', 'third party', 'integrations'],
		description: msg`Manage authorized applications`,
	},

	{
		id: 'beta-codes',
		tabType: 'beta_codes',
		label: msg`Beta Codes`,
		keywords: ['beta', 'codes', 'early access', 'invite', 'invite codes'],
		description: msg`Manage beta access codes`,
	},

	{
		id: 'plutonium-subscription',
		tabType: 'plutonium',
		label: msg`Subscription`,
		keywords: ['plutonium', 'premium', 'subscription', 'nitro', 'upgrade', 'plan'],
		description: msg`Manage your subscription`,
	},
	{
		id: 'plutonium-perks',
		tabType: 'plutonium',
		label: msg`Premium Perks`,
		keywords: ['perks', 'benefits', 'features', 'premium'],
		description: msg`View premium perks`,
	},

	{
		id: 'gift-inventory',
		tabType: 'gift_inventory',
		label: msg`Gift Inventory`,
		keywords: ['gifts', 'inventory', 'codes', 'redeem', 'gift codes'],
		description: msg`Manage your gifts`,
	},

	{
		id: 'expression-packs',
		tabType: 'expression_packs',
		label: msg`Expression Packs`,
		keywords: ['stickers', 'emoji', 'packs', 'expressions', 'sticker packs'],
		description: msg`Manage expression packs`,
	},

	{
		id: 'applications-dev',
		tabType: 'applications',
		label: msg`Developer Applications`,
		keywords: ['applications', 'bots', 'developer', 'api', 'oauth', 'create app'],
		description: msg`Manage developer applications`,
	},

	{
		id: 'developer-options',
		tabType: 'developer_options',
		label: msg`Developer Tools`,
		keywords: ['developer', 'debug', 'tools', 'dev tools', 'testing'],
		description: msg`Access developer tools`,
	},

	{
		id: 'feature-flags',
		tabType: 'feature_flags',
		label: msg`Feature Flags`,
		keywords: ['feature', 'flags', 'experiments', 'beta features'],
		description: msg`Manage feature flags`,
	},

	{
		id: 'component-gallery',
		tabType: 'component_gallery',
		label: msg`Component Gallery`,
		keywords: ['components', 'ui', 'gallery', 'design', 'ui kit'],
		description: msg`View UI components`,
	},
];

function createSearchableItems(): Array<SearchableSettingItem> {
	return SEARCHABLE_ITEMS_DESCRIPTORS.map((item) => ({
		...item,
		label: i18n._(item.label),
		description: item.description ? i18n._(item.description) : undefined,
	})) as Array<SearchableSettingItem>;
}

let cachedSearchableItems: Array<SearchableSettingItem> | null = null;

export function getSearchableItems(): Array<SearchableSettingItem> {
	if (!cachedSearchableItems) {
		cachedSearchableItems = createSearchableItems();
	}
	return cachedSearchableItems;
}

export function invalidateSearchCache(): void {
	cachedSearchableItems = null;
}

function normalizeSearchQuery(query: string): string {
	return query.toLowerCase().trim().replace(/\s+/g, ' ');
}

function calculateMatchScore(item: SearchableSettingItem, queryWords: Array<string>): number {
	let score = 0;
	const labelLower = item.label.toLowerCase();
	const descriptionLower = item.description?.toLowerCase() ?? '';
	const keywordsLower = item.keywords.map((k) => k.toLowerCase());

	for (const word of queryWords) {
		if (labelLower.includes(word)) {
			score += labelLower === word ? 100 : labelLower.startsWith(word) ? 50 : 25;
		}

		for (const keyword of keywordsLower) {
			if (keyword.includes(word)) {
				score += keyword === word ? 80 : keyword.startsWith(word) ? 40 : 20;
			}
		}

		if (descriptionLower.includes(word)) {
			score += 10;
		}
	}

	return score;
}

export function searchSettings(query: string, tabs: Array<SettingsTab>): Array<SettingsSearchResult> {
	const normalizedQuery = normalizeSearchQuery(query);
	if (!normalizedQuery) {
		return [];
	}

	const queryWords = normalizedQuery.split(' ').filter((w) => w.length > 0);
	const items = getSearchableItems();
	const tabSet = new Set(tabs.map((t) => t.type));

	const resultsByTab = new Map<UserSettingsTabType, SettingsSearchResult>();

	for (const item of items) {
		if (!tabSet.has(item.tabType)) {
			continue;
		}

		const score = calculateMatchScore(item, queryWords);
		if (score > 0) {
			const existing = resultsByTab.get(item.tabType);
			if (existing) {
				existing.matchedItems.push(item);
				existing.score = Math.max(existing.score, score);
			} else {
				const tab = tabs.find((t) => t.type === item.tabType);
				if (tab) {
					resultsByTab.set(item.tabType, {
						tab,
						matchedItems: [item],
						score,
					});
				}
			}
		}
	}

	const results = Array.from(resultsByTab.values());
	results.sort((a, b) => b.score - a.score);

	return results;
}

export function getMatchedSectionIds(results: Array<SettingsSearchResult>): Set<string> {
	const sectionIds = new Set<string>();
	for (const result of results) {
		for (const item of result.matchedItems) {
			if (item.sectionId) {
				sectionIds.add(item.sectionId);
			}
		}
	}
	return sectionIds;
}

export function getMatchedTabTypes(results: Array<SettingsSearchResult>): Set<UserSettingsTabType> {
	return new Set(results.map((r) => r.tab.type));
}
