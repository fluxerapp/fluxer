// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelStickerCommands from '@app/features/channel/commands/ChannelStickerCommands';
import styles from '@app/features/channel/components/ChannelStickersArea.module.css';
import ChannelSticker from '@app/features/channel/state/ChannelSticker';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {StickerContextMenuItems} from '@app/features/ui/action_menu/items/StickerContextMenuItems';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

const REMOVE_STICKER_DESCRIPTOR = msg({
	message: 'Remove sticker',
	comment:
		'Button or menu action label in the channel stickers area. Keep it concise. Keep the tone plain and specific.',
});

interface ChannelStickersAreaProps {
	channelId: string;
	hasAttachments: boolean;
}

export const ChannelStickersArea: React.FC<ChannelStickersAreaProps> = observer(({channelId, hasAttachments}) => {
	const {i18n} = useLingui();
	const {shouldAnimate, interactionHandlers} = useStickerAnimation();
	const sticker = ChannelSticker.getPendingSticker(channelId);
	const [previousSticker, setPreviousSticker] = useState(sticker);
	useEffect(() => {
		if (previousSticker && !sticker) {
			ComponentDispatch.dispatch('FORCE_JUMP_TO_PRESENT');
		} else if (!previousSticker && sticker) {
			ComponentDispatch.dispatch('FORCE_JUMP_TO_PRESENT');
		}
		setPreviousSticker(sticker);
	}, [sticker, previousSticker]);
	if (!sticker) {
		return null;
	}
	const handleRemove = () => {
		ChannelStickerCommands.removePendingSticker(channelId);
	};
	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		ContextMenuCommands.openFromEvent(event, ({onClose}) => (
			<StickerContextMenuItems
				sticker={sticker}
				onClose={onClose}
				data-flx="channel.channel-stickers-area.handle-context-menu.sticker-context-menu-items"
			/>
		));
	};
	const stickerUrl = AvatarUtils.getStickerURL({
		id: sticker.id,
		animated: shouldAnimate,
		size: 320,
	});
	return (
		<div
			className={clsx(styles.container, !hasAttachments && styles.standalone)}
			data-flx="channel.channel-stickers-area.container"
		>
			<div className={styles.content} data-flx="channel.channel-stickers-area.content">
				<div className={styles.stickerPreview} data-flx="channel.channel-stickers-area.sticker-preview">
					<img
						src={stickerUrl}
						alt={sticker.name}
						className={styles.stickerImage}
						data-flx="channel.channel-stickers-area.sticker-image.context-menu"
						{...interactionHandlers}
						onContextMenu={handleContextMenu}
					/>
				</div>
				<div className={styles.stickerInfo} data-flx="channel.channel-stickers-area.sticker-info">
					<div className={styles.stickerName} data-flx="channel.channel-stickers-area.sticker-name">
						:{sticker.name}:
					</div>
					{sticker.description && (
						<div className={styles.stickerDescription} data-flx="channel.channel-stickers-area.sticker-description">
							{sticker.description}
						</div>
					)}
				</div>
				<Tooltip
					text={i18n._(REMOVE_STICKER_DESCRIPTOR)}
					position="top"
					data-flx="channel.channel-stickers-area.tooltip"
				>
					<FocusRing offset={-2} data-flx="channel.channel-stickers-area.focus-ring">
						<button
							type="button"
							onClick={handleRemove}
							className={styles.removeButton}
							aria-label={i18n._(REMOVE_STICKER_DESCRIPTOR)}
							data-flx="channel.channel-stickers-area.remove-button"
						>
							<TrashIcon weight="regular" className={styles.icon} data-flx="channel.channel-stickers-area.icon" />
						</button>
					</FocusRing>
				</Tooltip>
			</div>
		</div>
	);
});
