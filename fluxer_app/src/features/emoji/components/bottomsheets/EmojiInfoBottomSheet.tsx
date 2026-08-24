// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/emoji/components/bottomsheets/EmojiInfoBottomSheet.module.css';
import Emoji from '@app/features/emoji/state/Emoji';
import {
	buildCustomEmojiURL,
	CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG,
} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import Guilds from '@app/features/guild/state/Guilds';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface EmojiInfoData {
	id?: string;
	name: string;
	animated?: boolean;
}

interface EmojiInfoBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	emoji: EmojiInfoData | null;
}

const EMOJI_SHEET_SNAP_POINTS: Array<number> = [0, 0.4, 0.5];
export const EmojiInfoBottomSheet: React.FC<EmojiInfoBottomSheetProps> = observer(({isOpen, onClose, emoji}) => {
	if (!isOpen || !emoji) {
		return null;
	}
	return (
		<EmojiInfoBottomSheetContent
			emoji={emoji}
			onClose={onClose}
			data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content"
		/>
	);
});

interface EmojiInfoBottomSheetContentProps {
	emoji: EmojiInfoData;
	onClose: () => void;
}

const EmojiInfoBottomSheetContent: React.FC<EmojiInfoBottomSheetContentProps> = observer(({emoji, onClose}) => {
	const isCustomEmoji = emoji.id != null;
	const shouldAnimateEmoji = useShouldAnimate({kind: 'emoji', isAnimated: Boolean(emoji.animated)});
	const emojiRecord = isCustomEmoji ? Emoji.getEmojiById(emoji.id!) : null;
	const guildId = emojiRecord?.guildId;
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const defaultEmojiSurrogate = isCustomEmoji ? null : UnicodeEmojis.normalizeEmojiNameToSurrogate(emoji.name);
	const emojiUrl = useMemo(() => {
		if (isCustomEmoji) {
			return buildCustomEmojiURL({
				id: emoji.id!,
				animated: Boolean(emoji.animated) && shouldAnimateEmoji,
				size: CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG,
			});
		}
		return EmojiUtils.getEmojiURL(defaultEmojiSurrogate ?? emoji.name);
	}, [emoji.id, emoji.name, emoji.animated, isCustomEmoji, defaultEmojiSurrogate, shouldAnimateEmoji]);
	const getEmojiDisplayName = (): string => {
		if (isCustomEmoji) {
			return `:${emoji.name}:`;
		}
		return UnicodeEmojis.nameForSurrogate(defaultEmojiSurrogate ?? emoji.name, true, `:${emoji.name}:`);
	};
	const emojiName = getEmojiDisplayName();
	const renderSubtext = () => {
		if (!isCustomEmoji) {
			return (
				<span className={styles.subtext} data-flx="emoji.emoji-info-bottom-sheet.render-subtext.subtext">
					<Trans>Default emoji</Trans>
				</span>
			);
		}
		if (guild) {
			return (
				<span className={styles.subtext} data-flx="emoji.emoji-info-bottom-sheet.render-subtext.subtext--2">
					<Trans>From {guild.name}</Trans>
				</span>
			);
		}
		return (
			<span className={styles.subtext} data-flx="emoji.emoji-info-bottom-sheet.render-subtext.subtext--3">
				<Trans>From another community</Trans>
			</span>
		);
	};
	return (
		<BottomSheet
			isOpen={true}
			onClose={onClose}
			snapPoints={EMOJI_SHEET_SNAP_POINTS}
			initialSnap={EMOJI_SHEET_SNAP_POINTS.length - 1}
			showCloseButton={false}
			data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content.bottom-sheet"
		>
			<div className={styles.content} data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content.content">
				<div
					className={styles.emojiContainer}
					data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content.emoji-container"
				>
					{emojiUrl && (
						<img
							src={emojiUrl}
							alt={emoji.name}
							draggable={false}
							className={styles.emoji}
							data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content.emoji"
						/>
					)}
				</div>
				<div
					className={styles.infoContainer}
					data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content.info-container"
				>
					<span
						className={styles.emojiName}
						data-flx="emoji.emoji-info-bottom-sheet.emoji-info-bottom-sheet-content.emoji-name"
					>
						{emojiName}
					</span>
					{renderSubtext()}
				</div>
			</div>
		</BottomSheet>
	);
});
