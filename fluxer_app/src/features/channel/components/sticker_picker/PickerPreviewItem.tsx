// SPDX-License-Identifier: AGPL-3.0-or-later
import premiumStyles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
export function renderStickerPickerPreviewItem(sticker: GuildSticker, shouldAnimateStickerPreview: boolean) {
	return (
		<div
			className={premiumStyles.previewItem}
			key={`${sticker.guildId ?? 'guild'}-${sticker.id}`}
			data-flx="channel.stickers-picker.render-sticker-preview-item.div"
		>
			<img
				src={AvatarUtils.getStickerURL({
					id: sticker.id,
					animated: shouldAnimateStickerPreview,
					isAnimatable: sticker.animated,
					size: 320,
				})}
				alt={sticker.name}
				data-flx="channel.stickers-picker.render-sticker-preview-item.img"
			/>
		</div>
	);
}
