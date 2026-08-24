// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const INSPECTOR_PREVIEW_SIZE = 320;

interface StickerPickerInspectorProps {
	hoveredSticker: GuildSticker | null;
	style?: React.CSSProperties;
}

export const StickerPickerInspector = observer(({hoveredSticker, style}: StickerPickerInspectorProps) => {
	const {shouldAnimate} = useStickerAnimation({
		respectUserSettings: false,
		isAnimated: hoveredSticker?.animated ?? false,
	});
	const previewUrl = hoveredSticker
		? AvatarUtils.getStickerURL({
				id: hoveredSticker.id,
				animated: shouldAnimate,
				isAnimatable: hoveredSticker.animated,
				size: INSPECTOR_PREVIEW_SIZE,
			})
		: '';
	return (
		<div
			className={styles.inspector}
			style={style}
			data-flx="channel.sticker-picker.sticker-picker-inspector.inspector"
		>
			{hoveredSticker && (
				<>
					<img
						src={previewUrl}
						alt={hoveredSticker.name}
						className={styles.inspectorEmoji}
						data-flx="channel.sticker-picker.sticker-picker-inspector.inspector-emoji"
					/>
					<span
						className={styles.inspectorText}
						data-flx="channel.sticker-picker.sticker-picker-inspector.inspector-text"
					>
						{hoveredSticker.name}
					</span>
				</>
			)}
		</div>
	);
});
