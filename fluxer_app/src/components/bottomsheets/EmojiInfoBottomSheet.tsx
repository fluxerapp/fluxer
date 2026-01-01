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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import UnicodeEmojis from '~/lib/UnicodeEmojis';
import EmojiStore from '~/stores/EmojiStore';
import GuildStore from '~/stores/GuildStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import * as EmojiUtils from '~/utils/EmojiUtils';
import {shouldUseNativeEmoji} from '~/utils/EmojiUtils';
import styles from './EmojiInfoBottomSheet.module.css';

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

	return <EmojiInfoBottomSheetContent emoji={emoji} onClose={onClose} />;
});

interface EmojiInfoBottomSheetContentProps {
	emoji: EmojiInfoData;
	onClose: () => void;
}

const EmojiInfoBottomSheetContent: React.FC<EmojiInfoBottomSheetContentProps> = observer(({emoji, onClose}) => {
	const isCustomEmoji = emoji.id != null;
	const emojiRecord = isCustomEmoji ? EmojiStore.getEmojiById(emoji.id!) : null;
	const guildId = emojiRecord?.guildId;
	const guild = guildId ? GuildStore.getGuild(guildId) : null;

	const emojiUrl = React.useMemo(() => {
		if (isCustomEmoji) {
			const url = AvatarUtils.getEmojiURL({id: emoji.id!, animated: emoji.animated ?? false});
			return `${url}?size=240&quality=lossless`;
		}
		if (shouldUseNativeEmoji) {
			return null;
		}
		return EmojiUtils.getEmojiURL(emoji.name);
	}, [emoji.id, emoji.name, emoji.animated, isCustomEmoji]);

	const getEmojiDisplayName = (): string => {
		if (isCustomEmoji) {
			return `:${emoji.name}:`;
		}
		return UnicodeEmojis.convertSurrogateToName(emoji.name, true, `:${emoji.name}:`);
	};

	const emojiName = getEmojiDisplayName();

	const renderSubtext = () => {
		if (!isCustomEmoji) {
			return (
				<span className={styles.subtext}>
					<Trans>Default emoji</Trans>
				</span>
			);
		}

		if (guild) {
			return (
				<span className={styles.subtext}>
					<Trans>From {guild.name}</Trans>
				</span>
			);
		}

		return (
			<span className={styles.subtext}>
				<Trans>From another server</Trans>
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
		>
			<div className={styles.content}>
				<div className={styles.emojiContainer}>
					{emojiUrl ? (
						<img src={emojiUrl} alt={emoji.name} draggable={false} className={styles.emoji} />
					) : (
						<span className={styles.nativeEmoji}>{emoji.name}</span>
					)}
				</div>
				<div className={styles.infoContainer}>
					<span className={styles.emojiName}>{emojiName}</span>
					{renderSubtext()}
				</div>
			</div>
		</BottomSheet>
	);
});
