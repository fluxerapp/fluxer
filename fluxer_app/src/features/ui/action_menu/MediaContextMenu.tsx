// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {clearAllAttachmentMocks, setAttachmentMock} from '@app/features/devtools/commands/DeveloperOptionsCommands';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {
	type MediaMenuDataProps,
	mediaMenuItemIds,
	useMediaMenuData,
} from '@app/features/ui/action_menu/items/MediaMenuData';
import {ReverseImageSearchMenuItems} from '@app/features/ui/action_menu/items/ReverseImageSearchMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MessageContextMenu} from '@app/features/ui/action_menu/MessageContextMenu';
import type {MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MS_PER_DAY, MS_PER_HOUR} from '@fluxer/date_utils/src/DateConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const ATTACHMENT_MOCK_DESCRIPTOR = msg({
	message: 'Attachment mock',
	comment: 'Developer-mode label for a mocked attachment used in debug surfaces.',
});
const MOCK_EXPIRES_IN_1_DAY_DESCRIPTOR = msg({
	message: 'Mock expires in 1 day',
	comment: 'Developer-mode label describing a mocked attachment expiry.',
});
const MOCK_EXPIRES_IN_7_DAYS_DESCRIPTOR = msg({
	message: 'Mock expires in 7 days',
	comment: 'Developer-mode label describing a mocked attachment expiry.',
});
const MOCK_EXPIRED_DESCRIPTOR = msg({
	message: 'Mock expired',
	comment: 'Developer-mode label for an expired mocked attachment.',
});
const CLEAR_MOCK_FOR_THIS_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Clear mock for this attachment',
	comment: 'Developer-mode action that removes a mock for one attachment.',
});
const CLEAR_ALL_ATTACHMENT_MOCKS_DESCRIPTOR = msg({
	message: 'Clear all attachment mocks',
	comment: 'Developer-mode action that removes every attachment mock.',
});

type MediaType = 'image' | 'gif' | 'gifv' | 'video' | 'audio' | 'file';

interface MediaContextMenuProps {
	message: Message;
	sourceChannel?: Channel | null;
	originalSrc: string;
	proxyURL?: string;
	type: MediaType;
	contentHash?: string | null;
	attachmentId?: string;
	embedIndex?: number;
	defaultName?: string;
	defaultAltText?: string;
	naturalWidth?: number;
	naturalHeight?: number;
	snapshotIndex?: number;
	onClose: () => void;
	onDelete: (bypassConfirm?: boolean) => void;
}

const INLINE_MEDIA_ITEM_IDS = new Set<string>([
	mediaMenuItemIds.copy,
	mediaMenuItemIds.download,
	mediaMenuItemIds.copyLink,
	mediaMenuItemIds.openLink,
]);
export const MediaContextMenu: React.FC<MediaContextMenuProps> = observer(
	({
		message,
		sourceChannel,
		originalSrc,
		proxyURL,
		type,
		contentHash,
		attachmentId,
		embedIndex,
		defaultName,
		defaultAltText,
		naturalWidth,
		naturalHeight,
		snapshotIndex,
		onClose,
		onDelete,
	}) => {
		const {i18n} = useLingui();
		const mediaProps: MediaMenuDataProps = useMemo(
			() => ({
				message,
				originalSrc,
				proxyURL,
				type,
				contentHash,
				attachmentId,
				embedIndex,
				defaultName,
				defaultAltText,
				naturalWidth,
				naturalHeight,
				snapshotIndex,
			}),
			[
				message,
				originalSrc,
				proxyURL,
				type,
				contentHash,
				attachmentId,
				embedIndex,
				defaultName,
				defaultAltText,
				naturalWidth,
				naturalHeight,
				snapshotIndex,
			],
		);
		const {groups: mediaGroupsRaw, handlers, state: mediaState} = useMediaMenuData(mediaProps, {onClose});
		const {submenuGroups, inlineCopyItem, inlineDownloadItem} = useMemo<{
			submenuGroups: Array<MenuGroupType>;
			inlineCopyItem: MenuItemType | null;
			inlineDownloadItem: MenuItemType | null;
		}>(() => {
			let copy: MenuItemType | null = null;
			let download: MenuItemType | null = null;
			const submenu: Array<MenuGroupType> = mediaGroupsRaw
				.map((group) => ({
					items: group.items.filter((item) => {
						const id = 'id' in item ? item.id : undefined;
						if (id === mediaMenuItemIds.copy) {
							copy = item as MenuItemType;
							return false;
						}
						if (id === mediaMenuItemIds.download) {
							download = item as MenuItemType;
							return false;
						}
						if (id && INLINE_MEDIA_ITEM_IDS.has(id)) {
							return false;
						}
						return true;
					}),
				}))
				.filter((group) => group.items.length > 0);
			return {submenuGroups: submenu, inlineCopyItem: copy, inlineDownloadItem: download};
		}, [mediaGroupsRaw]);
		const isDev = DeveloperMode.isDeveloper;
		const currentMock = attachmentId ? DeveloperOptions.mockAttachmentStates[attachmentId] : undefined;
		const mockExpiresSoon = () =>
			setAttachmentMock(attachmentId!, {
				expired: false,
				expiresAt: new Date(Date.now() + MS_PER_DAY).toISOString(),
			});
		const mockExpiresWeek = () =>
			setAttachmentMock(attachmentId!, {
				expired: false,
				expiresAt: new Date(Date.now() + 7 * MS_PER_DAY).toISOString(),
			});
		const mockExpired = () =>
			setAttachmentMock(attachmentId!, {
				expired: true,
				expiresAt: new Date(Date.now() - MS_PER_HOUR).toISOString(),
			});
		const clearMock = () => setAttachmentMock(attachmentId!, null);
		const supportsReverseImageSearch = type === 'image' || type === 'gif' || type === 'gifv';
		const inlineMediaItems = (
			<>
				{inlineCopyItem && (
					<MenuItem onClick={inlineCopyItem.onClick} data-flx="ui.action-menu.media-context-menu.menu-item.click">
						{inlineCopyItem.label ?? mediaState.copyLabel}
					</MenuItem>
				)}
				{inlineDownloadItem && (
					<MenuItem
						onClick={inlineDownloadItem.onClick}
						data-flx="ui.action-menu.media-context-menu.menu-item.click--2"
					>
						{inlineDownloadItem.label ?? mediaState.downloadLabel}
					</MenuItem>
				)}
			</>
		);
		const attachmentExtraContent = supportsReverseImageSearch ? (
			<ReverseImageSearchMenuItems
				imageUrl={originalSrc}
				onClose={onClose}
				data-flx="ui.action-menu.media-context-menu.reverse-image-search-menu-items"
			/>
		) : null;
		return (
			<>
				<MessageContextMenu
					message={message}
					sourceChannel={sourceChannel}
					onClose={onClose}
					onDelete={onDelete}
					excludeMediaActions
					mediaHandlers={handlers}
					mediaGroups={submenuGroups}
					attachmentId={attachmentId}
					attachmentExtraContent={attachmentExtraContent}
					inlineMediaItems={inlineMediaItems}
					data-flx="ui.action-menu.media-context-menu.message-context-menu"
				/>
				{isDev && attachmentId && (
					<MenuGroup data-flx="ui.action-menu.media-context-menu.menu-group">
						<MenuItemSubmenu
							label={i18n._(ATTACHMENT_MOCK_DESCRIPTOR)}
							render={() => (
								<>
									<MenuItem
										onClick={mockExpiresSoon}
										data-flx="ui.action-menu.media-context-menu.menu-item.mock-expires-soon"
									>
										{i18n._(MOCK_EXPIRES_IN_1_DAY_DESCRIPTOR)}
									</MenuItem>
									<MenuItem
										onClick={mockExpiresWeek}
										data-flx="ui.action-menu.media-context-menu.menu-item.mock-expires-week"
									>
										{i18n._(MOCK_EXPIRES_IN_7_DAYS_DESCRIPTOR)}
									</MenuItem>
									<MenuItem onClick={mockExpired} data-flx="ui.action-menu.media-context-menu.menu-item.mock-expired">
										{i18n._(MOCK_EXPIRED_DESCRIPTOR)}
									</MenuItem>
									{currentMock && (
										<MenuItem onClick={clearMock} data-flx="ui.action-menu.media-context-menu.menu-item.clear-mock">
											{i18n._(CLEAR_MOCK_FOR_THIS_ATTACHMENT_DESCRIPTOR)}
										</MenuItem>
									)}
									<MenuItem
										onClick={clearAllAttachmentMocks}
										data-flx="ui.action-menu.media-context-menu.menu-item.clear-all-attachment-mocks"
									>
										{i18n._(CLEAR_ALL_ATTACHMENT_MOCKS_DESCRIPTOR)}
									</MenuItem>
								</>
							)}
							data-flx="ui.action-menu.media-context-menu.menu-item-submenu"
						/>
					</MenuGroup>
				)}
			</>
		);
	},
);

MediaContextMenu.displayName = 'MediaContextMenu';
