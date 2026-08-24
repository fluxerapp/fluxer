// SPDX-License-Identifier: AGPL-3.0-or-later

import {showChannelDeleteFailedModal} from '@app/features/app/components/alerts/ChannelDeleteFailedModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {ChannelSettingsModal} from '@app/features/channel/components/modals/ChannelSettingsModal';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {DELETE_CATEGORY_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {ChannelDebugModal} from '@app/features/devtools/components/debug/ChannelDebugModal';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {
	CHANNEL_DEBUG_DESCRIPTOR,
	DEBUG_CHANNEL_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	MUTE_CATEGORY_DESCRIPTOR,
	RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR,
	UNMUTE_CATEGORY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {
	CollapseCategoryIcon,
	CopyIdIcon,
	DebugChannelIcon,
	DeleteIcon,
	MarkAsReadIcon,
	MuteIcon,
	SettingsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import React, {useMemo} from 'react';

const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_DESCRIPTOR = msg({
	message: 'Delete the category "{categoryName}"? Can\'t be undone.',
	comment: 'Confirm dialog body before deleting an empty channel category.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_2_DESCRIPTOR = msg({
	message:
		'Are you sure you want to delete the category "{categoryName}"? The channel inside will be moved to the top of the channel list.',
	comment: 'Confirm dialog body before deleting a category that contains one channel.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_3_DESCRIPTOR = msg({
	message:
		'Are you sure you want to delete the category "{categoryName}"? All {channelCount} channels inside will be moved to the top of the channel list.',
	comment: 'Confirm dialog body before deleting a category that contains multiple channels.',
});
const CATEGORY_DELETED_DESCRIPTOR = msg({
	message: 'Category deleted',
	comment: 'Toast confirming a channel category was deleted.',
});
const CATEGORY_ID_COPIED_DESCRIPTOR = msg({
	message: 'Category ID copied',
	comment: 'Toast confirming the category ID was copied to the clipboard.',
});
const EXPAND_CATEGORY_DESCRIPTOR = msg({
	message: 'Expand category',
	comment: 'Action label that expands a collapsed channel category.',
});
const COLLAPSE_CATEGORY_DESCRIPTOR = msg({
	message: 'Collapse category',
	comment: 'Action that collapses the channel category section.',
});
const EXPAND_ALL_CATEGORIES_DESCRIPTOR = msg({
	message: 'Expand all categories',
	comment: 'Action label that expands every collapsed channel category.',
});
const COLLAPSE_ALL_CATEGORIES_DESCRIPTOR = msg({
	message: 'Collapse all categories',
	comment: 'Action label that collapses every channel category.',
});
const EDIT_CATEGORY_DESCRIPTOR = msg({
	message: 'Edit category',
	comment: 'Action that opens the edit-category modal.',
});
const COPY_CATEGORY_ID_DESCRIPTOR = msg({
	message: 'Copy category ID',
	comment: 'Developer-mode action that copies the category ID to the clipboard.',
});
const logger = new Logger('CategoryMenuData');

export interface CategoryMenuDataOptions {
	onClose: () => void;
	onOpenMuteSheet?: () => void;
}

export interface CategoryMenuData {
	groups: Array<MenuGroupType>;
	handlers: CategoryMenuHandlers;
	state: CategoryMenuState;
}

export interface CategoryMenuHandlers {
	handleMarkAsRead: () => void;
	handleToggleCollapse: () => void;
	handleToggleCollapseAll: () => void;
	handleOpenMuteSheet: () => void;
	handleEditCategory: () => void;
	handleDeleteCategory: () => void;
	handleCopyCategoryId: () => Promise<void>;
	handleDebugChannel: () => void;
	handleResetMatureContentAgreeState: () => void;
}

export interface CategoryMenuState {
	hasUnread: boolean;
	isCollapsed: boolean;
	allCategoriesCollapsed: boolean;
	isMuted: boolean;
	mutedText: string | undefined;
	canManageChannels: boolean;
	developerMode: boolean;
}

function getCategoryMenuState(category: Channel): CategoryMenuState {
	const guildId = category.guildId!;
	const channels = Channels.getGuildChannels(guildId);
	const channelsInCategory = channels.filter(
		(ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY,
	);
	const hasUnread = channelsInCategory.some((ch) => ReadStates.hasUnread(ch.id));
	const isCollapsed = UserGuildSettings.isChannelSectionCollapsed(guildId, category.id);
	const categoryIds = channels.filter((ch) => ch.type === ChannelTypes.GUILD_CATEGORY).map((ch) => ch.id);
	const allCategoriesCollapsed =
		categoryIds.length > 0
			? categoryIds.every((categoryId) => UserGuildSettings.isChannelSectionCollapsed(guildId, categoryId))
			: false;
	const categoryOverride = UserGuildSettings.getChannelOverride(guildId, category.id);
	const isMuted = categoryOverride?.muted ?? false;
	const muteConfig = categoryOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		channelId: category.id,
		guildId,
	});
	const developerMode = UserSettings.developerMode;
	return {
		hasUnread,
		isCollapsed,
		allCategoriesCollapsed,
		isMuted,
		mutedText,
		canManageChannels,
		developerMode,
	};
}

export function useCategoryMenuData(category: Channel, options: CategoryMenuDataOptions): CategoryMenuData {
	const {i18n} = useLingui();
	const {onClose, onOpenMuteSheet} = options;
	const state = getCategoryMenuState(category);
	const handlers = useMemo(
		() => ({
			handleMarkAsRead: () => {
				const guildId = category.guildId!;
				const channels = Channels.getGuildChannels(guildId);
				const channelsInCategory = channels.filter(
					(ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY,
				);
				for (const channel of channelsInCategory) {
					ReadStateCommands.ack(channel.id, true, true);
				}
				onClose();
			},
			handleToggleCollapse: () => {
				UserGuildSettingsCommands.toggleChannelCollapsed(category.guildId!, category.id);
				onClose();
			},
			handleToggleCollapseAll: () => {
				const guildId = category.guildId!;
				const channels = Channels.getGuildChannels(guildId);
				const categoryIds = channels.filter((ch) => ch.type === ChannelTypes.GUILD_CATEGORY).map((ch) => ch.id);
				UserGuildSettingsCommands.toggleAllCategoriesCollapsed(guildId, categoryIds);
				onClose();
			},
			handleOpenMuteSheet: () => {
				onOpenMuteSheet?.();
			},
			handleEditCategory: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ChannelSettingsModal
							channelId={category.id}
							data-flx="ui.action-menu.items.category-menu-data.handle-edit-category.channel-settings-modal"
						/>
					)),
				);
			},
			handleDeleteCategory: () => {
				onClose();
				const guildId = category.guildId!;
				const categoryName = category.name ?? '';
				const channels = Channels.getGuildChannels(guildId);
				const channelsInCategory = channels.filter(
					(ch) => ch.parentId === category.id && ch.type !== ChannelTypes.GUILD_CATEGORY,
				);
				const channelCount = channelsInCategory.length;
				let description = i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_DESCRIPTOR, {categoryName});
				if (channelCount > 0) {
					if (channelCount === 1) {
						description = i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_2_DESCRIPTOR, {categoryName});
					} else {
						description = i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THE_3_DESCRIPTOR, {categoryName, channelCount});
					}
				}
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(DELETE_CATEGORY_DESCRIPTOR)}
							description={description}
							primaryText={i18n._(DELETE_CATEGORY_DESCRIPTOR)}
							primaryVariant="danger"
							onPrimary={async () => {
								try {
									await ChannelCommands.remove(category.id);
									ToastCommands.createToast({
										type: 'success',
										children: i18n._(CATEGORY_DELETED_DESCRIPTOR),
									});
								} catch (error) {
									logger.error('Failed to delete category:', error);
									showChannelDeleteFailedModal(error, 'category');
								}
							}}
							data-flx="ui.action-menu.items.category-menu-data.handle-delete-category.confirm-modal"
						/>
					)),
				);
			},
			handleCopyCategoryId: async () => {
				await TextCopyCommands.copy(i18n, category.id, true);
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(CATEGORY_ID_COPIED_DESCRIPTOR),
				});
				onClose();
			},
			handleDebugChannel: () => {
				ModalCommands.pushAfterBottomSheetClose(
					onClose,
					modal(() => (
						<ChannelDebugModal
							title={i18n._(CHANNEL_DEBUG_DESCRIPTOR)}
							channel={category}
							data-flx="ui.action-menu.items.category-menu-data.handle-debug-channel.channel-debug-modal"
						/>
					)),
				);
			},
			handleResetMatureContentAgreeState: () => {
				GuildMatureContentAgree.revokeCategory(category.id);
				onClose();
			},
		}),
		[category, i18n.locale, onClose, onOpenMuteSheet],
	);
	const groups = ((): Array<MenuGroupType> => {
		const menuGroups: Array<MenuGroupType> = [];
		menuGroups.push({
			items: [
				{
					icon: React.createElement(MarkAsReadIcon, {size: 20}),
					label: i18n._(MARK_AS_READ_DESCRIPTOR),
					onClick: handlers.handleMarkAsRead,
					disabled: !state.hasUnread,
				},
			],
		});
		const collapseItems: Array<MenuItemType> = [
			{
				icon: React.createElement(CollapseCategoryIcon, {collapsed: state.isCollapsed, size: 20}),
				label: state.isCollapsed ? i18n._(EXPAND_CATEGORY_DESCRIPTOR) : i18n._(COLLAPSE_CATEGORY_DESCRIPTOR),
				onClick: handlers.handleToggleCollapse,
			},
			{
				icon: React.createElement(CollapseCategoryIcon, {collapsed: state.allCategoriesCollapsed, size: 20}),
				label: state.allCategoriesCollapsed
					? i18n._(EXPAND_ALL_CATEGORIES_DESCRIPTOR)
					: i18n._(COLLAPSE_ALL_CATEGORIES_DESCRIPTOR),
				onClick: handlers.handleToggleCollapseAll,
			},
		];
		menuGroups.push({items: collapseItems});
		menuGroups.push({
			items: [
				{
					icon: React.createElement(MuteIcon, {size: 20}),
					label: state.isMuted ? i18n._(UNMUTE_CATEGORY_DESCRIPTOR) : i18n._(MUTE_CATEGORY_DESCRIPTOR),
					onClick: handlers.handleOpenMuteSheet,
					hint: state.mutedText,
				},
			],
		});
		if (state.canManageChannels) {
			menuGroups.push({
				items: [
					{
						icon: React.createElement(SettingsIcon, {size: 20}),
						label: i18n._(EDIT_CATEGORY_DESCRIPTOR),
						onClick: handlers.handleEditCategory,
					},
					{
						icon: React.createElement(DeleteIcon, {size: 20}),
						label: i18n._(DELETE_CATEGORY_DESCRIPTOR),
						onClick: handlers.handleDeleteCategory,
						danger: true,
					},
				],
			});
		}
		const utilityItems: Array<MenuItemType> = [
			{
				icon: React.createElement(CopyIdIcon, {size: 20}),
				label: i18n._(COPY_CATEGORY_ID_DESCRIPTOR),
				onClick: handlers.handleCopyCategoryId,
			},
		];
		if (state.developerMode) {
			utilityItems.unshift({
				icon: React.createElement(DebugChannelIcon, {size: 20}),
				label: i18n._(DEBUG_CHANNEL_DESCRIPTOR),
				onClick: handlers.handleDebugChannel,
			});
			if (GuildMatureContentAgree.hasAgreedToCategory(category.id)) {
				utilityItems.unshift({
					icon: React.createElement(DebugChannelIcon, {size: 20}),
					label: i18n._(RESET_MATURE_CONTENT_AGREE_STATE_DESCRIPTOR),
					onClick: handlers.handleResetMatureContentAgreeState,
				});
			}
		}
		menuGroups.push({items: utilityItems});
		return menuGroups;
	})();
	return {
		groups,
		handlers,
		state,
	};
}
