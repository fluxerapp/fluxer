// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import styles from '@app/features/expressions/components/bottomsheets/ExpressionInfoBottomSheet.module.css';
import {ExpressionInfoCard} from '@app/features/expressions/components/ExpressionInfoCard';
import {
	buildCustomEmojiURL,
	CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG,
} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {
	EXPRESSION_INFO_SURFACE_OPEN_IS_INTERACTION,
	STICKER_PREVIEW_SIZE,
} from '@app/features/expressions/utils/ExpressionPreviewConstants';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface EmojiInfoData {
	id?: string;
	name: string;
	animated?: boolean;
}

interface StickerInfoData {
	id: string;
	name: string;
	animated: boolean;
}

type ExpressionInfoBottomSheetProps = {
	isOpen: boolean;
	onClose: () => void;
} & ({kind: 'emoji'; emoji: EmojiInfoData | null} | {kind: 'sticker'; sticker: StickerInfoData | null});

const EXPRESSION_SHEET_SNAP_POINTS: Array<number> = [0, 0.4, 0.5];

export const ExpressionInfoBottomSheet: React.FC<ExpressionInfoBottomSheetProps> = observer((props) => {
	if (!props.isOpen) {
		return null;
	}
	if (props.kind === 'sticker') {
		return props.sticker ? (
			<ExpressionInfoBottomSheetShell
				onClose={props.onClose}
				data-flx="expressions.expression-info-bottom-sheet.shell.sticker"
			>
				<StickerInfoCard
					sticker={props.sticker}
					onClose={props.onClose}
					data-flx="expressions.expression-info-bottom-sheet.sticker-info-card"
				/>
			</ExpressionInfoBottomSheetShell>
		) : null;
	}
	return props.emoji ? (
		<ExpressionInfoBottomSheetShell
			onClose={props.onClose}
			data-flx="expressions.expression-info-bottom-sheet.shell.emoji"
		>
			<EmojiInfoCard
				emoji={props.emoji}
				onClose={props.onClose}
				data-flx="expressions.expression-info-bottom-sheet.emoji-info-card"
			/>
		</ExpressionInfoBottomSheetShell>
	) : null;
});

interface ExpressionInfoBottomSheetShellProps {
	onClose: () => void;
	children: React.ReactNode;
}

const ExpressionInfoBottomSheetShell = ({onClose, children}: ExpressionInfoBottomSheetShellProps) => (
	<BottomSheet
		isOpen={true}
		onClose={onClose}
		snapPoints={EXPRESSION_SHEET_SNAP_POINTS}
		initialSnap={EXPRESSION_SHEET_SNAP_POINTS.length - 1}
		showCloseButton={false}
		data-flx="expressions.expression-info-bottom-sheet.shell.bottom-sheet"
	>
		<div className={styles.content} data-flx="expressions.expression-info-bottom-sheet.shell.content">
			{children}
		</div>
	</BottomSheet>
);

interface EmojiInfoCardProps {
	emoji: EmojiInfoData;
	onClose: () => void;
}

const EmojiInfoCard = observer(function EmojiInfoCard({emoji, onClose}: EmojiInfoCardProps) {
	const isCustomEmoji = emoji.id != null;
	const shouldAnimateEmoji = useShouldAnimate({kind: 'emoji', isAnimated: Boolean(emoji.animated)});
	const emojiRecord = isCustomEmoji ? Emoji.getEmojiById(emoji.id!) : null;
	const guildId = emojiRecord?.guildId ?? null;
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
	const displayName = isCustomEmoji
		? `:${emoji.name}:`
		: UnicodeEmojis.nameForSurrogate(defaultEmojiSurrogate ?? emoji.name, true, `:${emoji.name}:`);
	return emoji.id != null ? (
		<ExpressionInfoCard
			kind="emoji"
			expressionId={emoji.id}
			guildId={guildId}
			displayName={displayName}
			previewUrl={emojiUrl}
			onClose={onClose}
			className={styles.card}
			data-flx="expressions.expression-info-bottom-sheet.emoji-info-card.expression-info-card.custom"
		/>
	) : (
		<ExpressionInfoCard
			kind="default_emoji"
			displayName={displayName}
			previewUrl={emojiUrl}
			onClose={onClose}
			className={styles.card}
			data-flx="expressions.expression-info-bottom-sheet.emoji-info-card.expression-info-card.default"
		/>
	);
});

interface StickerInfoCardProps {
	sticker: StickerInfoData;
	onClose: () => void;
}

const StickerInfoCard = observer(function StickerInfoCard({sticker, onClose}: StickerInfoCardProps) {
	const {shouldAnimate: shouldAnimateSticker} = useStickerAnimation({
		isAnimated: sticker.animated,
		isInteracting: EXPRESSION_INFO_SURFACE_OPEN_IS_INTERACTION,
	});
	const guildId = Sticker.getStickerById(sticker.id)?.guildId ?? null;
	const previewUrl = AvatarUtils.getStickerURL({
		id: sticker.id,
		animated: shouldAnimateSticker,
		isAnimatable: sticker.animated,
		size: STICKER_PREVIEW_SIZE,
	});
	return (
		<ExpressionInfoCard
			kind="sticker"
			expressionId={sticker.id}
			guildId={guildId}
			displayName={sticker.name}
			previewUrl={previewUrl}
			onClose={onClose}
			className={styles.card}
			data-flx="expressions.expression-info-bottom-sheet.sticker-info-card.expression-info-card"
		/>
	);
});
