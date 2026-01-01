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
import {
	ArrowRightIcon,
	HashIcon,
	HouseIcon,
	LightningIcon,
	SpeakerHighIcon,
	StarIcon,
	UsersIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as QuickSwitcherActionCreators from '~/actions/QuickSwitcherActionCreators';
import {QuickSwitcherResultTypes} from '~/Constants';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {ChannelContextMenu} from '~/components/uikit/ContextMenu/ChannelContextMenu';
import {DMContextMenu} from '~/components/uikit/ContextMenu/DMContextMenu';
import {GroupDMContextMenu} from '~/components/uikit/ContextMenu/GroupDMContextMenu';
import {GuildContextMenu} from '~/components/uikit/ContextMenu/GuildContextMenu';
import {UserContextMenu} from '~/components/uikit/ContextMenu/UserContextMenu';
import type {SegmentedTab} from '~/components/uikit/SegmentedTabs/SegmentedTabs';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {isTextInputKeyEvent} from '~/lib/isTextInputKeyEvent';
import ChannelStore from '~/stores/ChannelStore';
import LayerManager from '~/stores/LayerManager';
import ModalStore from '~/stores/ModalStore';
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
} from '~/stores/QuickSwitcherStore';
import UserStore from '~/stores/UserStore';

export interface QuickSwitcherSection {
	header?: HeaderResult;
	rows: Array<{result: QuickSwitcherExecutableResult; index: number}>;
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

export const getQuickSwitcherTabs = (t: (message: any) => string): Array<SegmentedTab<'search' | 'friends'>> => [
	{id: 'search', label: t(msg`Search`)},
	{id: 'friends', label: t(msg`Friends`)},
];

export const PREFIX_HINTS = [
	{symbol: '@', label: msg`People`},
	{symbol: '#', label: msg`Text channels`},
	{symbol: '!', label: msg`Voice channels`},
	{symbol: '*', label: msg`Communities`},
	{symbol: '>', label: msg`Quick Actions`},
];

export const getViewContext = (result: QuickSwitcherExecutableResult): string | undefined => {
	if (
		result.type === QuickSwitcherResultTypes.TEXT_CHANNEL ||
		result.type === QuickSwitcherResultTypes.VOICE_CHANNEL ||
		result.type === QuickSwitcherResultTypes.USER ||
		result.type === QuickSwitcherResultTypes.GROUP_DM
	) {
		return result.viewContext;
	}
	return undefined;
};

export const renderIcon = (
	result: QuickSwitcherExecutableResult,
	isHighlight: boolean,
	baseIconClass?: string,
	highlightIconClass?: string,
) => {
	const iconClass = clsx(baseIconClass || 'optionIcon', isHighlight && (highlightIconClass || 'optionIconHighlight'));

	switch (result.type) {
		case QuickSwitcherResultTypes.USER: {
			const userResult = result as UserResult;
			return {
				type: 'avatar' as const,
				content: <StatusAwareAvatar user={userResult.user} size={24} />,
			};
		}
		case QuickSwitcherResultTypes.GROUP_DM: {
			const groupDMResult = result as GroupDMResult;
			return {
				type: 'avatar' as const,
				content: <GroupDMAvatar channel={groupDMResult.channel} size={24} />,
			};
		}
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
			return {
				type: 'icon' as const,
				content: <HashIcon weight="bold" className={iconClass} />,
			};
		case QuickSwitcherResultTypes.VOICE_CHANNEL:
			return {
				type: 'icon' as const,
				content: <SpeakerHighIcon weight="fill" className={iconClass} />,
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
					/>
				),
			};
		}
		case QuickSwitcherResultTypes.VIRTUAL_GUILD: {
			const virtualGuild = result as VirtualGuildResult;
			if (virtualGuild.virtualGuildType === 'favorites') {
				return {
					type: 'icon' as const,
					content: <StarIcon weight="fill" className={iconClass} />,
				};
			}
			return {
				type: 'icon' as const,
				content: <HouseIcon weight="fill" className={iconClass} />,
			};
		}
		case QuickSwitcherResultTypes.SETTINGS: {
			const settingsResult = result as SettingsResult;
			return {
				type: 'icon' as const,
				content: <settingsResult.settingsTab.icon weight="fill" className={iconClass} />,
			};
		}
		case QuickSwitcherResultTypes.QUICK_ACTION:
			return {
				type: 'icon' as const,
				content: <LightningIcon weight="fill" className={iconClass} />,
			};
		case QuickSwitcherResultTypes.LINK:
			return {
				type: 'icon' as const,
				content: <ArrowRightIcon weight="bold" className={iconClass} />,
			};
		default:
			return {
				type: 'icon' as const,
				content: <UsersIcon weight="fill" className={iconClass} />,
			};
	}
};

export const handleContextMenu = (event: React.MouseEvent, result: QuickSwitcherExecutableResult): void => {
	event.preventDefault();
	event.stopPropagation();

	switch (result.type) {
		case QuickSwitcherResultTypes.USER: {
			const userResult = result as UserResult;
			const user = UserStore.getUser(userResult.user.id);
			if (user) {
				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<UserContextMenu user={user} onClose={onClose} />
				));
			}
			break;
		}
		case QuickSwitcherResultTypes.GROUP_DM: {
			const groupDMResult = result as GroupDMResult;
			const channel = ChannelStore.getChannel(groupDMResult.channel.id);
			if (channel) {
				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<GroupDMContextMenu channel={channel} onClose={onClose} />
				));
			}
			break;
		}
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
		case QuickSwitcherResultTypes.VOICE_CHANNEL: {
			const channelResult = result as TextChannelResult | VoiceChannelResult;
			const channel = ChannelStore.getChannel(channelResult.channel.id);
			if (channel) {
				if (channel.isPrivate()) {
					const recipient = channel.recipientIds?.[0] ? UserStore.getUser(channel.recipientIds[0]) : null;
					ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
						<DMContextMenu channel={channel} recipient={recipient} onClose={onClose} />
					));
				} else {
					ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
						<ChannelContextMenu channel={channel} onClose={onClose} />
					));
				}
			}
			break;
		}
		case QuickSwitcherResultTypes.GUILD: {
			const guildResult = result as GuildResult;
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<GuildContextMenu guild={guildResult.guild} onClose={onClose} />
			));
			break;
		}
	}
};

export const getChannelId = (result: QuickSwitcherExecutableResult): string | null => {
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
};

export const getResultKey = (result: QuickSwitcherResult): string => {
	const viewContext = getViewContext(result as QuickSwitcherExecutableResult);
	return viewContext ? `${result.type}-${viewContext}-${result.id}` : `${result.type}-${result.id}`;
};

export const createSections = (results: Array<QuickSwitcherResult>): Array<QuickSwitcherSection> => {
	const acc: Array<QuickSwitcherSection> = [];
	let current: QuickSwitcherSection | null = null;

	results.forEach((r, index) => {
		if (r.type === QuickSwitcherResultTypes.HEADER) {
			current = {header: r as HeaderResult, rows: []};
			acc.push(current);
			return;
		}
		if (!current) {
			current = {rows: []};
			acc.push(current);
		}
		current.rows.push({result: r as QuickSwitcherExecutableResult, index});
	});

	return acc;
};

export const useQuickSwitcherKeyboardHandling = (
	isOpen: boolean,
	isMobile: boolean,
	inputRef: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>,
	query: string,
) => {
	React.useEffect(() => {
		if (!isOpen) return;

		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				event.preventDefault();
				QuickSwitcherActionCreators.hide();
			}
		};

		window.addEventListener('keydown', handleKeyDown);
		return () => window.removeEventListener('keydown', handleKeyDown);
	}, [isOpen]);

	React.useEffect(() => {
		if (!isOpen || isMobile) {
			return;
		}

		const handleKeyDown = (event: KeyboardEvent) => {
			if (ModalStore.hasModalOpen()) {
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

			input.focus();

			if (event.key === 'Dead') {
				return;
			}

			const nextValue = query + event.key;
			QuickSwitcherActionCreators.search(nextValue);
		};

		window.addEventListener('keydown', handleKeyDown);

		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [isMobile, isOpen, query, inputRef]);
};

export const useQuickSwitcherInputFocus = (
	isOpen: boolean,
	isMobile: boolean,
	activeTab?: 'search' | 'friends',
	inputRef?: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>,
) => {
	React.useLayoutEffect(() => {
		if (!isOpen) return;
		if (isMobile && activeTab !== 'search') {
			return;
		}

		const key = QuickSwitcherActionCreators.getModalKey();
		LayerManager.addLayer('modal', key, () => QuickSwitcherActionCreators.hide());

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
};
