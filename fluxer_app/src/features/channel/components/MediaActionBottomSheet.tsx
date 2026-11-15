// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMessageActionMenuData} from '@app/features/channel/components/MessageActionMenu';
import {requestDeleteMessage} from '@app/features/channel/components/MessageActionUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {MediaMenuDataProps, MediaType} from '@app/features/ui/action_menu/items/MediaMenuData';
import {useMediaMenuData} from '@app/features/ui/action_menu/items/MediaMenuData';
import {buildReverseImageSearchMenuGroups} from '@app/features/ui/action_menu/items/SearchMenuData';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const IMAGE_OPTIONS_DESCRIPTOR = msg({
	message: 'Image options',
	comment: 'Short label in the channel and chat media action bottom sheet. Keep it concise.',
});
const GIF_OPTIONS_DESCRIPTOR = msg({
	message: 'GIF options',
	comment: 'Short label in the channel and chat media action bottom sheet. Keep it concise.',
});
const VIDEO_OPTIONS_DESCRIPTOR = msg({
	message: 'Video options',
	comment: 'Short label in the channel and chat media action bottom sheet. Keep it concise.',
});
const AUDIO_OPTIONS_DESCRIPTOR = msg({
	message: 'Audio options',
	comment: 'Short label in the channel and chat media action bottom sheet. Keep it concise.',
});
const FILE_OPTIONS_DESCRIPTOR = msg({
	message: 'File options',
	comment: 'Short label in the channel and chat media action bottom sheet. Keep it concise.',
});
const MEDIA_OPTIONS_DESCRIPTOR = msg({
	message: 'Media options',
	comment: 'Short label in the channel and chat media action bottom sheet. Keep it concise.',
});

interface MediaActionBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	message: Message;
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
	handleDelete?: (bypassConfirm?: boolean) => void;
	sourceChannel?: Channel | null;
}

const MediaActionBottomSheetContent: React.FC<MediaActionBottomSheetProps> = observer(
	({
		isOpen,
		onClose,
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
		handleDelete,
		sourceChannel,
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
		const {groups: mediaGroups} = useMediaMenuData(mediaProps, {onClose});
		const handleDeleteMessage = useCallback(
			(bypassConfirm?: boolean) => {
				if (handleDelete) {
					handleDelete(bypassConfirm);
					return;
				}
				requestDeleteMessage(message, i18n, bypassConfirm);
			},
			[handleDelete, message, i18n],
		);
		const {groups: messageGroups} = useMessageActionMenuData(message, {
			onClose,
			onDelete: handleDeleteMessage,
			sourceChannel,
		});
		const visibleMessageGroups = useMemo(
			() => messageGroups.filter((group) => group.items.length > 0),
			[messageGroups],
		);
		const supportsReverseImageSearch = type === 'image' || type === 'gif' || type === 'gifv';
		const reverseImageSearchGroups = useMemo(
			() => (supportsReverseImageSearch ? buildReverseImageSearchMenuGroups(originalSrc, {i18n, onClose}) : []),
			[supportsReverseImageSearch, originalSrc, i18n.locale, onClose],
		);
		const combinedGroups = useMemo(() => {
			const groups = [...mediaGroups, ...reverseImageSearchGroups];
			if (visibleMessageGroups.length > 0) {
				groups.push(...visibleMessageGroups);
			}
			return groups;
		}, [mediaGroups, reverseImageSearchGroups, visibleMessageGroups]);
		const title = useMemo(() => {
			switch (type) {
				case 'image':
					return i18n._(IMAGE_OPTIONS_DESCRIPTOR);
				case 'gif':
				case 'gifv':
					return i18n._(GIF_OPTIONS_DESCRIPTOR);
				case 'video':
					return i18n._(VIDEO_OPTIONS_DESCRIPTOR);
				case 'audio':
					return i18n._(AUDIO_OPTIONS_DESCRIPTOR);
				case 'file':
					return i18n._(FILE_OPTIONS_DESCRIPTOR);
				default:
					return i18n._(MEDIA_OPTIONS_DESCRIPTOR);
			}
		}, [type, i18n.locale]);
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={combinedGroups}
				title={title}
				data-flx="channel.media-action-bottom-sheet.media-action-bottom-sheet-content.menu-bottom-sheet"
			/>
		);
	},
);
export const MediaActionBottomSheet: React.FC<MediaActionBottomSheetProps> = observer((props) => {
	if (!props.isOpen) {
		return null;
	}
	return (
		<MediaActionBottomSheetContent
			data-flx="channel.media-action-bottom-sheet.media-action-bottom-sheet-content"
			{...props}
		/>
	);
});
