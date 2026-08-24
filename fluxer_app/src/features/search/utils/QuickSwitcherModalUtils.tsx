// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import Channels from '@app/features/channel/state/Channels';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import {COMMUNITIES_DESCRIPTOR, MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {shouldDisableAutofocusOnMobile} from '@app/features/platform/utils/AutofocusUtils';
import {isTextInputKeyEvent} from '@app/features/platform/utils/IsTextInputKeyEvent';
import ReadStates from '@app/features/read_state/state/ReadStates';
import * as QuickSwitcherCommands from '@app/features/search/commands/QuickSwitcherCommands';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {type EscapeIntent, trackEscapeIntentUntilNextTask} from '@app/features/search/state/QuickSwitcherEscapeIntent';
import type {
	GroupDMResult,
	GuildResult,
	HeaderResult,
	QuickSwitcherExecutableResult,
	QuickSwitcherResult,
	SettingsResult,
	TextChannelResult,
	UserResult,
	VirtualGuildResult,
	VoiceChannelResult,
} from '@app/features/search/state/QuickSwitcherTypes';
import {ChannelContextMenu} from '@app/features/ui/action_menu/ChannelContextMenu';
import {DMContextMenu} from '@app/features/ui/action_menu/DMContextMenu';
import {GroupDMContextMenu} from '@app/features/ui/action_menu/GroupDMContextMenu';
import {GuildContextMenu} from '@app/features/ui/action_menu/GuildContextMenu';
import {UserContextMenu} from '@app/features/ui/action_menu/UserContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import type {SegmentedTab} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import LayerManager from '@app/features/ui/state/LayerManager';
import Modal from '@app/features/ui/state/Modal';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {ArrowRightIcon, HashIcon, HouseIcon, SpeakerHighIcon, StarIcon, UsersIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useEffect, useLayoutEffect, useRef} from 'react';

const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Quick switcher tab label that searches all entity types.',
});
const FRIENDS_DESCRIPTOR = msg({
	message: 'Friends',
	comment: 'Quick switcher tab label that lists friends.',
});
const PEOPLE_DESCRIPTOR = msg({
	message: 'People',
	comment: 'Quick switcher section header listing user candidates.',
});
const TEXT_CHANNELS_DESCRIPTOR = msg({
	message: 'Text channels',
	comment: 'Quick switcher section header listing text channel candidates.',
});
const VOICE_CHANNELS_DESCRIPTOR = msg({
	message: 'Voice channels',
	comment: 'Quick switcher section header listing voice channel candidates.',
});
const MESSAGE_1_MENTION_DESCRIPTOR = msg({
	message: '1 mention',
	comment: 'Badge text on a quick switcher row when the channel has exactly one mention.',
});
const MESSAGE_1_UNREAD_DESCRIPTOR = msg({
	message: '1 unread',
	comment: 'Badge text on a quick switcher row when the channel has exactly one unread message.',
});
const UNREAD_DESCRIPTOR = msg({
	message: '{unreadCount} unread',
	comment:
		'Badge text on a quick switcher row when the channel has multiple unread messages. unreadCount is the count.',
});

export interface QuickSwitcherSection {
	key: string;
	header?: HeaderResult;
	rows: Array<{result: QuickSwitcherExecutableResult; index: number}>;
}

export interface QuickSwitcherResultAccessibilityMetadata {
	guildName: string | null;
	isChannel: boolean;
	isUser: boolean;
	label: string;
	mentionCount: number;
	subtext: string | null;
	unreadCount: number;
}

export interface QuickSwitcherSharedProps {
	isOpen: boolean;
	query: string;
	results: Array<QuickSwitcherResult>;
	selectedIndex: number;
	onClose: () => void;
	onSearch: (value: string) => void;
	onMoveSelection: (direction: 'up' | 'down') => void;
	onConfirmSelection: () => Promise<void>;
}

export interface QuickSwitcherMobileTabProps {
	activeTab: 'search' | 'friends';
	onTabChange: (tab: 'search' | 'friends') => void;
	friendsSearchQuery: string;
	onFriendsSearchChange: (value: string) => void;
}

export function getQuickSwitcherTabs(i18n: I18n): Array<SegmentedTab<'search' | 'friends'>> {
	return [
		{id: 'search', label: i18n._(SEARCH_DESCRIPTOR)},
		{id: 'friends', label: i18n._(FRIENDS_DESCRIPTOR)},
	];
}

export const PREFIX_HINTS = [
	{symbol: '@', label: PEOPLE_DESCRIPTOR},
	{symbol: '#', label: TEXT_CHANNELS_DESCRIPTOR},
	{symbol: '!', label: VOICE_CHANNELS_DESCRIPTOR},
	{symbol: '*', label: COMMUNITIES_DESCRIPTOR},
];

export function getViewContext(result: QuickSwitcherExecutableResult): string | undefined {
	if (
		result.type === QuickSwitcherResultTypes.TEXT_CHANNEL ||
		result.type === QuickSwitcherResultTypes.VOICE_CHANNEL ||
		result.type === QuickSwitcherResultTypes.USER ||
		result.type === QuickSwitcherResultTypes.GROUP_DM
	) {
		return result.viewContext;
	}
	return undefined;
}

export function renderIcon(
	result: QuickSwitcherExecutableResult,
	isHighlight: boolean,
	baseIconClass?: string,
	highlightIconClass?: string,
) {
	const iconClass = clsx(baseIconClass || 'optionIcon', isHighlight && (highlightIconClass || 'optionIconHighlight'));
	switch (result.type) {
		case QuickSwitcherResultTypes.USER: {
			const userResult = result as UserResult;
			return {
				type: 'avatar' as const,
				content: (
					<StatusAwareAvatar
						user={userResult.user}
						size={24}
						data-flx="search.quick-switcher-modal-utils.render-icon.status-aware-avatar"
					/>
				),
			};
		}
		case QuickSwitcherResultTypes.GROUP_DM: {
			const groupDMResult = result as GroupDMResult;
			return {
				type: 'avatar' as const,
				content: (
					<GroupDMAvatar
						channel={groupDMResult.channel}
						size={24}
						data-flx="search.quick-switcher-modal-utils.render-icon.group-dm-avatar"
					/>
				),
			};
		}
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
			return {
				type: 'icon' as const,
				content: (
					<HashIcon
						weight="bold"
						className={iconClass}
						data-flx="search.quick-switcher-modal-utils.render-icon.hash-icon"
					/>
				),
			};
		case QuickSwitcherResultTypes.VOICE_CHANNEL:
			return {
				type: 'icon' as const,
				content: (
					<SpeakerHighIcon
						weight="fill"
						className={iconClass}
						data-flx="search.quick-switcher-modal-utils.render-icon.speaker-high-icon"
					/>
				),
			};
		case QuickSwitcherResultTypes.GUILD: {
			const guildResult = result as GuildResult;
			return {
				type: 'guild' as const,
				content: (
					<GuildIcon
						id={guildResult.guild.id}
						name={guildResult.guild.name}
						icon={guildResult.guild.icon}
						sizePx={24}
						data-flx="search.quick-switcher-modal-utils.render-icon.guild-icon"
					/>
				),
			};
		}
		case QuickSwitcherResultTypes.VIRTUAL_GUILD: {
			const virtualGuild = result as VirtualGuildResult;
			if (virtualGuild.virtualGuildType === 'favorites') {
				return {
					type: 'icon' as const,
					content: (
						<StarIcon
							weight="fill"
							className={iconClass}
							data-flx="search.quick-switcher-modal-utils.render-icon.star-icon"
						/>
					),
				};
			}
			return {
				type: 'icon' as const,
				content: (
					<HouseIcon
						weight="fill"
						className={iconClass}
						data-flx="search.quick-switcher-modal-utils.render-icon.house-icon"
					/>
				),
			};
		}
		case QuickSwitcherResultTypes.SETTINGS: {
			const settingsResult = result as SettingsResult;
			return {
				type: 'icon' as const,
				content: (
					<settingsResult.settingsTab.icon
						weight="fill"
						className={iconClass}
						data-flx="search.quick-switcher-modal-utils.render-icon.settings-result-settings-tab-icon"
					/>
				),
			};
		}
		case QuickSwitcherResultTypes.LINK:
			return {
				type: 'icon' as const,
				content: (
					<ArrowRightIcon
						weight="bold"
						className={iconClass}
						data-flx="search.quick-switcher-modal-utils.render-icon.arrow-right-icon"
					/>
				),
			};
		default:
			return {
				type: 'icon' as const,
				content: (
					<UsersIcon
						weight="fill"
						className={iconClass}
						data-flx="search.quick-switcher-modal-utils.render-icon.users-icon"
					/>
				),
			};
	}
}

export function getQuickSwitcherResultAccessibilityMetadata(
	result: QuickSwitcherExecutableResult,
	i18n: I18n,
): QuickSwitcherResultAccessibilityMetadata {
	const channelId = getChannelId(result);
	const unreadCount = channelId ? ReadStates.getUnreadCount(channelId) : 0;
	const mentionCount = channelId ? ReadStates.getMentionCount(channelId) : 0;
	const isUser = result.type === QuickSwitcherResultTypes.USER;
	let subtext = result.subtitle ?? null;
	let guildName: string | null = null;
	let isChannel = false;
	if (result.type === QuickSwitcherResultTypes.TEXT_CHANNEL || result.type === QuickSwitcherResultTypes.VOICE_CHANNEL) {
		isChannel = true;
		const parentCategoryId = result.channel.parentId;
		const categoryChannel = parentCategoryId ? Channels.getChannel(parentCategoryId) : null;
		const categoryName =
			categoryChannel?.type === ChannelTypes.GUILD_CATEGORY && categoryChannel.name ? categoryChannel.name : null;
		subtext = categoryName;
		guildName = result.guild?.name ?? null;
	}
	const labelParts = [result.title];
	if (subtext) labelParts.push(subtext);
	if (guildName) labelParts.push(guildName);
	if (mentionCount > 0) {
		labelParts.push(
			mentionCount === 1 ? i18n._(MESSAGE_1_MENTION_DESCRIPTOR) : i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}),
		);
	}
	if (unreadCount > 0) {
		labelParts.push(unreadCount === 1 ? i18n._(MESSAGE_1_UNREAD_DESCRIPTOR) : i18n._(UNREAD_DESCRIPTOR, {unreadCount}));
	}
	return {
		guildName,
		isChannel,
		isUser,
		label: labelParts.join(', '),
		mentionCount,
		subtext,
		unreadCount,
	};
}

export function handleContextMenu(event: React.MouseEvent, result: QuickSwitcherExecutableResult): void {
	event.preventDefault();
	event.stopPropagation();
	switch (result.type) {
		case QuickSwitcherResultTypes.USER: {
			const userResult = result as UserResult;
			const user = Users.getUser(userResult.user.id);
			if (user) {
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<UserContextMenu
						user={user}
						onClose={onClose}
						data-flx="search.quick-switcher-modal-utils.handle-context-menu.user-context-menu"
					/>
				));
			}
			break;
		}
		case QuickSwitcherResultTypes.GROUP_DM: {
			const groupDMResult = result as GroupDMResult;
			const channel = Channels.getChannel(groupDMResult.channel.id);
			if (channel) {
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<GroupDMContextMenu
						channel={channel}
						onClose={onClose}
						data-flx="search.quick-switcher-modal-utils.handle-context-menu.group-dm-context-menu"
					/>
				));
			}
			break;
		}
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
		case QuickSwitcherResultTypes.VOICE_CHANNEL: {
			const channelResult = result as TextChannelResult | VoiceChannelResult;
			const channel = Channels.getChannel(channelResult.channel.id);
			if (channel) {
				if (channel.isPrivate()) {
					const recipient = channel.recipientIds?.[0] ? Users.getUser(channel.recipientIds[0]) : null;
					ContextMenuCommands.openFromEvent(event, ({onClose}) => (
						<DMContextMenu
							channel={channel}
							recipient={recipient}
							onClose={onClose}
							data-flx="search.quick-switcher-modal-utils.handle-context-menu.dm-context-menu"
						/>
					));
				} else {
					ContextMenuCommands.openFromEvent(event, ({onClose}) => (
						<ChannelContextMenu
							channel={channel}
							onClose={onClose}
							data-flx="search.quick-switcher-modal-utils.handle-context-menu.channel-context-menu"
						/>
					));
				}
			}
			break;
		}
		case QuickSwitcherResultTypes.GUILD: {
			const guildResult = result as GuildResult;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<GuildContextMenu
					guild={guildResult.guild}
					onClose={onClose}
					data-flx="search.quick-switcher-modal-utils.handle-context-menu.guild-context-menu"
				/>
			));
			break;
		}
	}
}

export function getChannelId(result: QuickSwitcherExecutableResult): string | null {
	switch (result.type) {
		case QuickSwitcherResultTypes.USER: {
			const userResult = result as UserResult;
			return userResult.dmChannelId;
		}
		case QuickSwitcherResultTypes.GROUP_DM: {
			const groupDMResult = result as GroupDMResult;
			return groupDMResult.channel.id;
		}
		case QuickSwitcherResultTypes.TEXT_CHANNEL: {
			const textChannelResult = result as TextChannelResult;
			return textChannelResult.channel.id;
		}
		case QuickSwitcherResultTypes.VOICE_CHANNEL: {
			const voiceChannelResult = result as VoiceChannelResult;
			return voiceChannelResult.channel.id;
		}
		default:
			return null;
	}
}

export function getResultKey(result: QuickSwitcherResult): string {
	const viewContext = getViewContext(result as QuickSwitcherExecutableResult);
	return viewContext ? `${result.type}-${viewContext}-${result.id}` : `${result.type}-${result.id}`;
}

export function createSections(results: Array<QuickSwitcherResult>): Array<QuickSwitcherSection> {
	const acc: Array<QuickSwitcherSection> = [];
	let current: QuickSwitcherSection | null = null;
	results.forEach((r, index) => {
		if (r.type === QuickSwitcherResultTypes.HEADER) {
			const header = r as HeaderResult;
			current = {key: `header-${header.id}`, header, rows: []};
			acc.push(current);
			return;
		}
		if (!current) {
			current = {key: 'leading', rows: []};
			acc.push(current);
		}
		current.rows.push({result: r as QuickSwitcherExecutableResult, index});
	});
	return acc;
}

export function useQuickSwitcherKeyboardHandling(
	isOpen: boolean,
	isMobile: boolean,
	inputRef: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>,
	query: string,
) {
	useEffect(() => {
		if (!isOpen) return;
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				event.preventDefault();
				QuickSwitcherCommands.hide();
			}
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => window.removeEventListener('keydown', handleKeyDown);
	}, [isOpen]);
	useEffect(() => {
		if (!isOpen || isMobile) {
			return;
		}
		const handleKeyDown = (event: KeyboardEvent) => {
			if (Modal.hasModalOpen()) {
				return;
			}
			if (!isTextInputKeyEvent(event)) {
				return;
			}
			const input = inputRef.current;
			if (!input) {
				return;
			}
			const activeElement = document.activeElement;
			const isTextInputElement =
				activeElement instanceof HTMLInputElement ||
				activeElement instanceof HTMLTextAreaElement ||
				(activeElement instanceof HTMLElement && activeElement.isContentEditable);
			if (activeElement === input) {
				return;
			}
			if (isTextInputElement) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			event.stopImmediatePropagation();
			input['focus']();
			if (event.key === 'Dead') {
				return;
			}
			const nextValue = query + event.key;
			QuickSwitcherCommands.search(nextValue);
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [isMobile, isOpen, query, inputRef]);
}

export function dismissQuickSwitcher({isEscape}: {isEscape: boolean}): void {
	if (isEscape && QuickSwitcher.query.length > 0) {
		QuickSwitcherCommands.search('');
		return;
	}
	QuickSwitcherCommands.hide();
}

export function useQuickSwitcherEscapeIntent(isActive: boolean): EscapeIntent {
	const escapeIntentRef = useRef(false);
	useEffect(() => {
		if (!isActive) {
			return;
		}
		return trackEscapeIntentUntilNextTask(escapeIntentRef);
	}, [isActive]);
	return escapeIntentRef;
}

export function useQuickSwitcherInputFocus(
	isOpen: boolean,
	isMobile: boolean,
	activeTab?: 'search' | 'friends',
	inputRef?: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>,
) {
	useLayoutEffect(() => {
		if (!isOpen) return;
		if (isMobile || shouldDisableAutofocusOnMobile()) {
			return;
		}
		const key = QuickSwitcherCommands.getModalKey();
		LayerManager.addLayer('modal', key, () => dismissQuickSwitcher({isEscape: true}));
		const focusInput = () => {
			inputRef?.current?.focus();
			inputRef?.current?.select();
		};
		requestAnimationFrame(() => {
			focusInput();
			window.setTimeout(focusInput, 10);
		});
		return () => {
			LayerManager.removeLayer('modal', key);
		};
	}, [isMobile, isOpen, activeTab, inputRef]);
}
