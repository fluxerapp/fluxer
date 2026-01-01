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

import {useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useMessageActionMenuData} from '~/components/channel/messageActionMenu';
import {ExpressionPickerSheet} from '~/components/modals/ExpressionPickerSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import type {MessageRecord} from '~/records/MessageRecord';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import {shouldUseNativeEmoji} from '~/utils/EmojiUtils';
import styles from './MessageActionBottomSheet.module.css';

interface MessageActionBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	message: MessageRecord;
	handleDelete: (bypassConfirm?: boolean) => void;
}

export const MessageActionBottomSheet: React.FC<MessageActionBottomSheetProps> = observer(
	({isOpen, onClose, message, handleDelete}) => {
		const {t} = useLingui();
		const [isEmojiPickerOpen, setIsEmojiPickerOpen] = React.useState(false);

		const handleAddReaction = React.useCallback(() => {
			setIsEmojiPickerOpen(true);
		}, []);

		const handleEmojiPickerClose = React.useCallback(() => {
			setIsEmojiPickerOpen(false);
			onClose();
		}, [onClose]);

		const {groups, handlers, quickReactionEmojis, quickReactionRowVisible} = useMessageActionMenuData(message, {
			onClose,
			onDelete: () => handleDelete(),
			onOpenEmojiPicker: handleAddReaction,
			quickReactionCount: 4,
		});

		const quickReactionRow = quickReactionRowVisible ? (
			<div className={styles.quickReactionWrapper}>
				<div className={styles.quickReactionRow}>
					{quickReactionEmojis.map((emoji) => {
						const isUnicodeEmoji = !emoji.guildId && !emoji.id;
						const useNativeRendering = shouldUseNativeEmoji && isUnicodeEmoji;
						return (
							<button
								key={emoji.name}
								type="button"
								onClick={() => {
									EmojiPickerStore.trackEmoji(emoji);
									handlers.handleEmojiSelect(emoji);
									onClose();
								}}
								aria-label={t`React with :${emoji.name}:`}
								className={styles.quickReactionButton}
							>
								{useNativeRendering ? (
									<span className={styles.quickReactionEmoji}>{emoji.surrogates}</span>
								) : (
									<img src={emoji.url ?? ''} alt={emoji.name} className={styles.quickReactionEmoji} />
								)}
							</button>
						);
					})}

					<button
						type="button"
						onClick={handleAddReaction}
						aria-label={t`Add another reaction`}
						className={styles.quickReactionButton}
					>
						<PlusIcon className={styles.addReactionIcon} weight="bold" />
					</button>
				</div>
			</div>
		) : null;

		return (
			<>
				<MenuBottomSheet
					isOpen={isOpen && !isEmojiPickerOpen}
					onClose={onClose}
					groups={groups}
					headerContent={quickReactionRow}
				/>
				<ExpressionPickerSheet
					isOpen={isEmojiPickerOpen}
					onClose={handleEmojiPickerClose}
					channelId={message.channelId}
					onEmojiSelect={handlers.handleEmojiSelect}
					visibleTabs={['emojis']}
				/>
			</>
		);
	},
);
